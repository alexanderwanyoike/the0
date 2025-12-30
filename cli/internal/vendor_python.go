package internal

import (
	"context"
	"fmt"
	"io"
	"os"
	"os/user"
	"path/filepath"
	"time"

	"github.com/docker/docker/api/types/container"
	"github.com/docker/docker/api/types/image"
	"github.com/fatih/color"
)

const (
	pythonImage = "python:3.11-slim"
	vendorDir   = "vendor"
)

// PythonVendor handles Python dependency installation via pip
type PythonVendor struct{}

// init registers the Python vendor
func init() {
	RegisterVendor(&PythonVendor{})
}

// Name returns the vendor name
func (v *PythonVendor) Name() string {
	return "Python"
}

// DockerImage returns the Docker image used for vendoring
func (v *PythonVendor) DockerImage() string {
	return pythonImage
}

// Detect checks if requirements.txt exists
func (v *PythonVendor) Detect(projectPath string) bool {
	requirementsPath := filepath.Join(projectPath, "requirements.txt")
	_, err := os.Stat(requirementsPath)
	return err == nil
}

// ShouldVendor checks if Python vendoring should be performed
func (v *PythonVendor) ShouldVendor(projectPath string) (bool, error) {
	// Check if requirements.txt exists
	requirementsPath := filepath.Join(projectPath, "requirements.txt")
	if _, err := os.Stat(requirementsPath); os.IsNotExist(err) {
		return false, nil
	}

	// Check if vendor directory already exists and is recent
	vendorPath := filepath.Join(projectPath, vendorDir)
	vendorInfo, err := os.Stat(vendorPath)
	requirementsInfo, err2 := os.Stat(requirementsPath)

	if err == nil && err2 == nil {
		// If vendor is newer than requirements.txt, skip vendoring
		if vendorInfo.ModTime().After(requirementsInfo.ModTime()) {
			return false, nil
		}
	}

	return true, nil
}

// Vendor performs the Docker-based Python dependency installation
func (v *PythonVendor) Vendor(vm *VendorManager) error {
	blue := color.New(color.FgBlue)
	green := color.New(color.FgGreen)

	blue.Println("Installing Python dependencies...")

	// Step 1: Pull Python image if needed
	if err := v.pullImage(vm); err != nil {
		return fmt.Errorf("failed to pull Python image: %v", err)
	}

	// Step 2: Create vendor directory
	vendorPath := filepath.Join(vm.projectPath, vendorDir)
	if err := os.RemoveAll(vendorPath); err != nil {
		return fmt.Errorf("failed to clean vendor directory: %v", err)
	}
	if err := os.MkdirAll(vendorPath, 0755); err != nil {
		return fmt.Errorf("failed to create vendor directory: %v", err)
	}

	// Step 3: Run vendoring container
	containerID, err := v.runContainer(vm)
	if err != nil {
		return fmt.Errorf("failed to run vendoring container: %v", err)
	}
	defer vm.cleanupContainer(containerID)

	// Step 4: Verify vendored files
	if err := vm.verifyVendoredFiles(vendorPath); err != nil {
		return fmt.Errorf("failed to verify vendored files: %v", err)
	}

	green.Println("v Python dependencies installed")
	return nil
}

// pullImage pulls the Python image if it doesn't exist locally
func (v *PythonVendor) pullImage(vm *VendorManager) error {
	ctx := context.Background()

	// Check if image exists locally
	images, err := vm.dockerClient.ImageList(ctx, image.ListOptions{})
	if err != nil {
		return err
	}

	imageExists := false
	for _, img := range images {
		for _, tag := range img.RepoTags {
			if tag == pythonImage {
				imageExists = true
				break
			}
		}
		if imageExists {
			break
		}
	}

	if imageExists {
		return nil
	}

	blue := color.New(color.FgBlue)
	blue.Printf("Pulling Docker image: %s...\n", pythonImage)

	reader, err := vm.dockerClient.ImagePull(ctx, pythonImage, image.PullOptions{})
	if err != nil {
		return err
	}
	defer reader.Close()

	// Read pull output (but don't display it to keep output clean)
	io.Copy(io.Discard, reader)

	return nil
}

// runContainer creates and runs a container for Python vendoring
func (v *PythonVendor) runContainer(vm *VendorManager) (string, error) {
	ctx := context.Background()

	// Generate unique container name
	containerName := fmt.Sprintf("%s%d", vendorContainerName, time.Now().Unix())

	// Prepare absolute paths for volume mounting
	vendorPath := filepath.Join(vm.projectPath, vendorDir)
	requirementsPath := filepath.Join(vm.projectPath, "requirements.txt")

	absVendorPath, err := filepath.Abs(vendorPath)
	if err != nil {
		return "", fmt.Errorf("failed to get absolute vendor path: %v", err)
	}

	absRequirementsPath, err := filepath.Abs(requirementsPath)
	if err != nil {
		return "", fmt.Errorf("failed to get absolute requirements path: %v", err)
	}

	config := &container.Config{
		Image: pythonImage,
		Cmd: []string{
			"sh", "-c",
			v.getInstallCommand(),
		},
		WorkingDir: "/app",
		Env:        getBuildEnvVars(),
	}

	hostConfig := &container.HostConfig{
		Binds: []string{
			fmt.Sprintf("%s:/vendor", absVendorPath),
			fmt.Sprintf("%s:/requirements.txt:ro", absRequirementsPath),
		},
		AutoRemove: false, // We'll remove manually after copying files
	}

	resp, err := vm.dockerClient.ContainerCreate(ctx, config, hostConfig, nil, nil, containerName)
	if err != nil {
		return "", err
	}

	if err := vm.dockerClient.ContainerStart(ctx, resp.ID, container.StartOptions{}); err != nil {
		return "", err
	}

	// Stream logs in real-time while container is running
	logReader, err := vm.dockerClient.ContainerLogs(ctx, resp.ID, container.LogsOptions{
		ShowStdout: true,
		ShowStderr: true,
		Follow:     true,
		Timestamps: false,
	})
	if err != nil {
		return resp.ID, fmt.Errorf("failed to get container logs: %v", err)
	}

	// Stream logs in a goroutine
	logDone := make(chan struct{})
	go func() {
		defer close(logDone)
		defer logReader.Close()

		blue := color.New(color.FgBlue)
		buffer := make([]byte, 1024)
		for {
			n, err := logReader.Read(buffer)
			if err != nil {
				if err != io.EOF {
					blue.Printf("Log read error: %v\n", err)
				}
				break
			}

			if n > 0 {
				// Clean Docker log headers and display output
				output := vm.cleanDockerLogOutput(buffer[:n])
				if output != "" {
					blue.Printf("  %s", output)
				}
			}
		}
	}()

	// Wait for container to finish
	statusCh, errCh := vm.dockerClient.ContainerWait(ctx, resp.ID, container.WaitConditionNotRunning)
	select {
	case err := <-errCh:
		if err != nil {
			return resp.ID, err
		}
	case status := <-statusCh:
		// Wait for logs to finish streaming
		<-logDone

		if status.StatusCode != 0 {
			// Get full container logs for error details
			logs, _ := vm.getContainerLogs(resp.ID)
			red := color.New(color.FgRed)
			red.Printf("Python dependency installation failed with exit code %d\n", status.StatusCode)
			if logs != "" {
				red.Printf("Full error log:\n%s\n", logs)
			}
			return resp.ID, fmt.Errorf("pip install failed with exit code %d", status.StatusCode)
		}
	}

	return resp.ID, nil
}

// getInstallCommand returns the pip install command with proper ownership
func (v *PythonVendor) getInstallCommand() string {
	// Get current user info for ownership
	currentUser, err := user.Current()
	var uid, gid string
	if err == nil {
		uid = currentUser.Uid
		gid = currentUser.Gid
	} else {
		// Fallback to common non-root user
		uid = "1000"
		gid = "1000"
	}

	// Install git if GITHUB_TOKEN is set (needed for private repo dependencies)
	// The python:3.11-slim image doesn't include git by default
	gitInstall := `if [ -n "$GITHUB_TOKEN" ]; then apt-get update && apt-get install -y --no-install-recommends git && rm -rf /var/lib/apt/lists/*; fi`

	// Configure git to use GITHUB_TOKEN for private repos if set
	// This rewrites git URLs to use the token for authentication
	// Handles multiple URL formats: git@github.com:, https://github.com/, ssh://git@github.com/
	gitConfig := `if [ -n "$GITHUB_TOKEN" ]; then git config --global url."https://${GITHUB_TOKEN}@github.com/".insteadOf "git@github.com:" && git config --global url."https://${GITHUB_TOKEN}@github.com/".insteadOf "https://github.com/" && git config --global url."https://${GITHUB_TOKEN}@github.com/".insteadOf "ssh://git@github.com/"; fi`

	// Configure pip to use extra index URL for private PyPI repos if set
	pipExtraIndex := `PIP_EXTRA_INDEX_FLAG=""; if [ -n "$PIP_EXTRA_INDEX_URL" ]; then PIP_EXTRA_INDEX_FLAG="--extra-index-url $PIP_EXTRA_INDEX_URL"; fi`

	// Always fix ownership even on failure, then exit with original status
	return fmt.Sprintf("%s && %s && %s && pip install --target /vendor -r /requirements.txt $PIP_EXTRA_INDEX_FLAG --no-cache-dir --disable-pip-version-check; STATUS=$?; chown -R %s:%s /vendor 2>/dev/null || true; exit $STATUS", gitInstall, gitConfig, pipExtraIndex, uid, gid)
}
