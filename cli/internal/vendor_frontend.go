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

// FrontendVendor handles frontend bundle building via npm
type FrontendVendor struct{}

// init registers the Frontend vendor
func init() {
	RegisterVendor(&FrontendVendor{})
}

// Name returns the vendor name
func (v *FrontendVendor) Name() string {
	return "Frontend"
}

// DockerImage returns the Docker image used for building
func (v *FrontendVendor) DockerImage() string {
	return nodeImage
}

// Detect checks if frontend/package.json exists
func (v *FrontendVendor) Detect(projectPath string) bool {
	frontendPackageJson := filepath.Join(projectPath, "frontend", "package.json")
	_, err := os.Stat(frontendPackageJson)
	return err == nil
}

// ShouldVendor checks if frontend building should be performed.
// Always returns true if frontend/package.json exists - we don't cache builds
// to ensure fresh bundles after any source changes.
func (v *FrontendVendor) ShouldVendor(projectPath string) (bool, error) {
	frontendPath := filepath.Join(projectPath, "frontend")
	packageJsonPath := filepath.Join(frontendPath, "package.json")

	// Check if frontend/package.json exists
	if _, err := os.Stat(packageJsonPath); os.IsNotExist(err) {
		return false, nil
	}

	// Always build when frontend exists - don't trust cached builds
	return true, nil
}

// Vendor performs the Docker-based frontend bundle building
func (v *FrontendVendor) Vendor(vm *VendorManager) error {
	blue := color.New(color.FgBlue)
	green := color.New(color.FgGreen)

	blue.Println("Building frontend bundle...")

	// Step 0: Clean dist directory to ensure fresh build
	distPath := filepath.Join(vm.projectPath, "frontend", "dist")
	os.RemoveAll(distPath)

	// Step 1: Pull Node image if needed
	if err := v.pullImage(vm); err != nil {
		return fmt.Errorf("failed to pull Node image: %v", err)
	}

	// Step 2: Run frontend build container
	containerID, err := v.runContainer(vm)
	if err != nil {
		return fmt.Errorf("failed to run frontend build container: %v", err)
	}
	defer vm.cleanupContainer(containerID)

	// Step 3: Verify bundle was created
	bundlePath := filepath.Join(vm.projectPath, "frontend", "dist", "bundle.js")
	if _, err := os.Stat(bundlePath); os.IsNotExist(err) {
		return fmt.Errorf("frontend build did not produce dist/bundle.js")
	}

	// Get bundle size
	bundleInfo, _ := os.Stat(bundlePath)
	sizeStr := vm.formatFileSize(bundleInfo.Size())

	green.Printf("v Frontend bundle built (%s)\n", sizeStr)
	return nil
}

// pullImage pulls the Node.js image if it doesn't exist locally
func (v *FrontendVendor) pullImage(vm *VendorManager) error {
	ctx := context.Background()

	// Check if image exists locally
	images, err := vm.dockerClient.ImageList(ctx, image.ListOptions{})
	if err != nil {
		return err
	}

	imageExists := false
	for _, img := range images {
		for _, tag := range img.RepoTags {
			if tag == nodeImage {
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
	blue.Printf("Pulling Docker image: %s...\n", nodeImage)

	reader, err := vm.dockerClient.ImagePull(ctx, nodeImage, image.PullOptions{})
	if err != nil {
		return err
	}
	defer reader.Close()

	// Read pull output (but don't display it to keep output clean)
	io.Copy(io.Discard, reader)

	return nil
}

// runContainer creates and runs a container for frontend building
func (v *FrontendVendor) runContainer(vm *VendorManager) (string, error) {
	ctx := context.Background()

	// Generate unique container name
	containerName := fmt.Sprintf("%sfrontend-%d", vendorContainerName, time.Now().Unix())

	// Prepare absolute paths for volume mounting
	absProjectPath, err := filepath.Abs(vm.projectPath)
	if err != nil {
		return "", fmt.Errorf("failed to get absolute project path: %v", err)
	}

	// Get current user info for ownership
	currentUser, err := user.Current()
	var uid, gid string
	if err == nil {
		uid = currentUser.Uid
		gid = currentUser.Gid
	} else {
		uid = "1000"
		gid = "1000"
	}

	// Configure npm auth for GitHub Packages if GITHUB_TOKEN is set
	npmAuthSetup := `if [ -n "$GITHUB_TOKEN" ]; then echo "//npm.pkg.github.com/:_authToken=$GITHUB_TOKEN" >> ~/.npmrc; fi`

	// Build command: setup auth, install deps, run build script
	// The build script in package.json should output ESM with external react
	buildCmd := fmt.Sprintf(
		"%s && cd /project/frontend && npm install && npm run build; STATUS=$?; chown -R %s:%s /project/frontend/dist /project/frontend/node_modules 2>/dev/null || true; exit $STATUS",
		npmAuthSetup, uid, gid,
	)

	config := &container.Config{
		Image: nodeImage,
		Cmd: []string{
			"sh", "-c",
			buildCmd,
		},
		WorkingDir: "/project/frontend",
		Env:        getBuildEnvVars(),
	}

	hostConfig := &container.HostConfig{
		Binds: []string{
			fmt.Sprintf("%s:/project", absProjectPath),
		},
		AutoRemove: false,
	}

	resp, err := vm.dockerClient.ContainerCreate(ctx, config, hostConfig, nil, nil, containerName)
	if err != nil {
		return "", err
	}

	if err := vm.dockerClient.ContainerStart(ctx, resp.ID, container.StartOptions{}); err != nil {
		return "", err
	}

	// Stream logs in real-time
	logReader, err := vm.dockerClient.ContainerLogs(ctx, resp.ID, container.LogsOptions{
		ShowStdout: true,
		ShowStderr: true,
		Follow:     true,
		Timestamps: false,
	})
	if err != nil {
		return resp.ID, fmt.Errorf("failed to get container logs: %v", err)
	}

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
		<-logDone
		if status.StatusCode != 0 {
			logs, _ := vm.getContainerLogs(resp.ID)
			red := color.New(color.FgRed)
			red.Printf("Frontend build failed with exit code %d\n", status.StatusCode)
			if logs != "" {
				red.Printf("Full error log:\n%s\n", logs)
			}
			return resp.ID, fmt.Errorf("frontend build failed with exit code %d", status.StatusCode)
		}
	}

	return resp.ID, nil
}

// BuildFrontendIfNeeded builds frontend only if needed and Docker is available
// This is a convenience function for backward compatibility
func BuildFrontendIfNeeded(projectPath string) error {
	blue := color.New(color.FgBlue)
	red := color.New(color.FgRed)

	vendor := &FrontendVendor{}
	shouldBuild, err := vendor.ShouldVendor(projectPath)
	if err != nil {
		return err
	}

	if !shouldBuild {
		return nil
	}

	blue.Println("Frontend detected, building bundle...")

	// Check Docker availability
	if err := CheckDockerInstalled(); err != nil {
		red.Printf("Docker required for frontend build: %v\n", err)
		red.Println("Solution: Install Docker or pre-build the frontend with 'npm run build'")
		return fmt.Errorf("docker unavailable for frontend build")
	}

	vm, err := NewVendorManager(projectPath)
	if err != nil {
		return fmt.Errorf("failed to initialize vendor manager: %v", err)
	}
	defer vm.Close()

	if err := vm.CheckDockerRunning(); err != nil {
		red.Printf("Docker daemon not running: %v\n", err)
		red.Println("Solution: Start Docker or pre-build the frontend")
		return fmt.Errorf("docker daemon not running for frontend build")
	}

	if err := vendor.Vendor(vm); err != nil {
		return fmt.Errorf("frontend build failed: %v", err)
	}

	return nil
}
