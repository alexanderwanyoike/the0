package internal

import (
	"context"
	"fmt"
	"io"
	"os"
	"os/user"
	"path/filepath"
	"strings"
	"time"

	"github.com/docker/docker/api/types/container"
	"github.com/docker/docker/api/types/image"
	"github.com/fatih/color"
)

const rustImage = "rust:latest"

// RustBuilder handles Rust project detection and building.
type RustBuilder struct{}

func init() {
	RegisterBuilder(&RustBuilder{})
}

// Name returns the language name for logging.
func (b *RustBuilder) Name() string {
	return "Rust"
}

// DockerImage returns the Docker image for Rust builds.
func (b *RustBuilder) DockerImage() string {
	return rustImage
}

// Detect checks if Cargo.toml exists in the project.
func (b *RustBuilder) Detect(projectPath string) bool {
	cargoTomlPath := filepath.Join(projectPath, "Cargo.toml")
	_, err := os.Stat(cargoTomlPath)
	return err == nil
}

// ShouldBuild checks if Rust project needs building.
func (b *RustBuilder) ShouldBuild(projectPath string) (bool, error) {
	cargoTomlPath := filepath.Join(projectPath, "Cargo.toml")
	if _, err := os.Stat(cargoTomlPath); os.IsNotExist(err) {
		return false, nil
	}
	return true, nil
}

// Build performs the Docker-based Rust build.
func (b *RustBuilder) Build(vm *VendorManager) error {
	blue := color.New(color.FgBlue)
	green := color.New(color.FgGreen)

	blue.Println("Building Rust project...")

	// Pull Rust image if needed
	if err := b.pullImage(vm); err != nil {
		return fmt.Errorf("failed to pull Rust image: %v", err)
	}

	// Run build container
	containerID, err := b.runBuildContainer(vm)
	if err != nil {
		return fmt.Errorf("failed to run Rust build container: %v", err)
	}
	defer vm.cleanupContainer(containerID)

	// Verify binary was created
	binaryPath := b.FindBinary(vm.projectPath)
	if binaryPath == "" {
		return fmt.Errorf("rust build did not produce a binary in target/release/")
	}

	// Get binary info
	binaryInfo, err := os.Stat(binaryPath)
	if err != nil {
		return fmt.Errorf("failed to stat binary: %v", err)
	}

	sizeStr := vm.formatFileSize(binaryInfo.Size())
	green.Printf("v Rust binary built: %s (%s)\n", filepath.Base(binaryPath), sizeStr)
	return nil
}

// FindBinary finds the built binary in target/release/.
func (b *RustBuilder) FindBinary(projectPath string) string {
	releaseDir := filepath.Join(projectPath, "target", "release")

	entries, err := os.ReadDir(releaseDir)
	if err != nil {
		return ""
	}

	for _, entry := range entries {
		if entry.IsDir() {
			continue
		}
		// Skip common non-binary files
		name := entry.Name()
		if strings.HasSuffix(name, ".d") || strings.HasSuffix(name, ".rlib") ||
			strings.HasPrefix(name, "lib") || strings.Contains(name, ".") {
			continue
		}
		// Check if it's executable
		info, err := entry.Info()
		if err != nil {
			continue
		}
		if info.Mode()&0111 != 0 {
			return filepath.Join(releaseDir, name)
		}
	}
	return ""
}

// pullImage pulls the Rust image if it doesn't exist locally.
func (b *RustBuilder) pullImage(vm *VendorManager) error {
	ctx := context.Background()

	images, err := vm.dockerClient.ImageList(ctx, image.ListOptions{})
	if err != nil {
		return err
	}

	imageExists := false
	for _, img := range images {
		for _, tag := range img.RepoTags {
			if tag == rustImage {
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
	blue.Printf("Pulling Docker image: %s...\n", rustImage)

	reader, err := vm.dockerClient.ImagePull(ctx, rustImage, image.PullOptions{})
	if err != nil {
		return err
	}
	defer reader.Close()

	io.Copy(io.Discard, reader)
	return nil
}

// runBuildContainer creates and runs a container for Rust building.
func (b *RustBuilder) runBuildContainer(vm *VendorManager) (string, error) {
	ctx := context.Background()

	ctrName := fmt.Sprintf("%srust-%d", vendorContainerName, time.Now().Unix())

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

	// Build command
	buildCmd := "cargo build --release 2>&1"

	config := &container.Config{
		Image: rustImage,
		Cmd: []string{
			"sh", "-c",
			buildCmd,
		},
		WorkingDir: "/project",
		Env:        getRustBuildEnvVars(),
		User:       fmt.Sprintf("%s:%s", uid, gid),
	}

	hostConfig := &container.HostConfig{
		Binds: []string{
			fmt.Sprintf("%s:/project", absProjectPath),
		},
		AutoRemove: false,
	}

	resp, err := vm.dockerClient.ContainerCreate(ctx, config, hostConfig, nil, nil, ctrName)
	if err != nil {
		return "", err
	}

	if err := vm.dockerClient.ContainerStart(ctx, resp.ID, container.StartOptions{}); err != nil {
		return "", err
	}

	// Stream logs
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
			red.Printf("Rust build failed with exit code %d\n", status.StatusCode)
			if logs != "" {
				red.Printf("Full error log:\n%s\n", logs)
			}
			return resp.ID, fmt.Errorf("cargo build failed with exit code %d", status.StatusCode)
		}
	}

	return resp.ID, nil
}

// getRustBuildEnvVars returns environment variables for Rust build.
func getRustBuildEnvVars() []string {
	secrets, err := LoadBuildSecrets()
	if err != nil || secrets == nil {
		return nil
	}

	var envVars []string
	if secrets.GitHubToken != "" {
		// Configure cargo to use GitHub token for private git dependencies
		envVars = append(envVars, "CARGO_NET_GIT_FETCH_WITH_CLI=true")
		envVars = append(envVars, "GIT_TERMINAL_PROMPT=0")
	}
	return envVars
}

// Legacy functions for backwards compatibility

// IsRustProject checks if the project is a Rust project.
func IsRustProject(projectPath string) bool {
	builder := &RustBuilder{}
	return builder.Detect(projectPath)
}

// ShouldBuildRust checks if Rust project needs building.
func ShouldBuildRust(projectPath string) (bool, error) {
	builder := &RustBuilder{}
	return builder.ShouldBuild(projectPath)
}

// BuildRustIfNeeded builds Rust project only if needed and Docker is available.
func BuildRustIfNeeded(projectPath string) error {
	builder := &RustBuilder{}

	shouldBuild, err := builder.ShouldBuild(projectPath)
	if err != nil {
		return err
	}

	if !shouldBuild {
		return nil
	}

	blue := color.New(color.FgBlue)
	red := color.New(color.FgRed)

	blue.Println("Rust project detected, building...")

	if err := CheckDockerInstalled(); err != nil {
		red.Printf("Docker required for Rust build: %v\n", err)
		red.Println("Solution: Install Docker or pre-build with 'cargo build --release'")
		return fmt.Errorf("docker unavailable for rust build")
	}

	vm, err := NewVendorManager(projectPath)
	if err != nil {
		return fmt.Errorf("failed to initialize vendor manager: %v", err)
	}
	defer vm.Close()

	if err := vm.CheckDockerRunning(); err != nil {
		red.Printf("Docker daemon not running: %v\n", err)
		red.Println("Solution: Start Docker or pre-build with 'cargo build --release'")
		return fmt.Errorf("docker daemon not running for rust build")
	}

	return builder.Build(vm)
}
