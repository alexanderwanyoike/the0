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

const cppImage = "silkeh/clang:17"  // Has cmake preinstalled

// CppBuilder handles C/C++ project detection and building.
type CppBuilder struct{}

func init() {
	RegisterBuilder(&CppBuilder{})
}

// Name returns the language name for logging.
func (b *CppBuilder) Name() string {
	return "C/C++"
}

// DockerImage returns the Docker image for C/C++ builds.
func (b *CppBuilder) DockerImage() string {
	return cppImage
}

// Detect checks if CMakeLists.txt or Makefile exists in the project.
func (b *CppBuilder) Detect(projectPath string) bool {
	cmakePath := filepath.Join(projectPath, "CMakeLists.txt")
	if _, err := os.Stat(cmakePath); err == nil {
		return true
	}
	makefilePath := filepath.Join(projectPath, "Makefile")
	if _, err := os.Stat(makefilePath); err == nil {
		return true
	}
	return false
}

// ShouldBuild checks if C/C++ project needs building.
func (b *CppBuilder) ShouldBuild(projectPath string) (bool, error) {
	return b.Detect(projectPath), nil
}

// isCMakeProject checks if the project uses CMake.
func (b *CppBuilder) isCMakeProject(projectPath string) bool {
	cmakePath := filepath.Join(projectPath, "CMakeLists.txt")
	_, err := os.Stat(cmakePath)
	return err == nil
}

// Build performs the Docker-based C/C++ build.
func (b *CppBuilder) Build(vm *VendorManager) error {
	blue := color.New(color.FgBlue)
	green := color.New(color.FgGreen)

	isCMake := b.isCMakeProject(vm.projectPath)
	if isCMake {
		blue.Println("Building C/C++ project with CMake...")
	} else {
		blue.Println("Building C/C++ project with Makefile...")
	}

	// Pull GCC image if needed
	if err := b.pullImage(vm); err != nil {
		return fmt.Errorf("failed to pull GCC image: %v", err)
	}

	// Run build container
	containerID, err := b.runBuildContainer(vm, isCMake)
	if err != nil {
		return fmt.Errorf("failed to run C/C++ build container: %v", err)
	}
	defer vm.cleanupContainer(containerID)

	// Verify binary was created
	binaryPath := b.FindBinary(vm.projectPath)
	if binaryPath == "" {
		if isCMake {
			return fmt.Errorf("C/C++ build did not produce a binary in build/")
		}
		return fmt.Errorf("C/C++ build did not produce an executable binary")
	}

	// Get binary info
	binaryInfo, err := os.Stat(binaryPath)
	if err != nil {
		return fmt.Errorf("failed to stat binary: %v", err)
	}

	sizeStr := vm.formatFileSize(binaryInfo.Size())
	green.Printf("v C/C++ binary built: %s (%s)\n", filepath.Base(binaryPath), sizeStr)
	return nil
}

// FindBinary finds the built binary.
// For CMake projects: looks in build/ directory
// For Makefile projects: looks in project root
func (b *CppBuilder) FindBinary(projectPath string) string {
	// First check build/ directory (CMake)
	buildDir := filepath.Join(projectPath, "build")
	if binary := b.findExecutableInDir(buildDir); binary != "" {
		return binary
	}

	// Then check project root (Makefile)
	if binary := b.findExecutableInDir(projectPath); binary != "" {
		return binary
	}

	return ""
}

// findExecutableInDir finds an executable binary in a directory.
func (b *CppBuilder) findExecutableInDir(dir string) string {
	entries, err := os.ReadDir(dir)
	if err != nil {
		return ""
	}

	for _, entry := range entries {
		if entry.IsDir() {
			continue
		}

		name := entry.Name()
		// Skip common non-binary files
		if strings.HasSuffix(name, ".o") ||
			strings.HasSuffix(name, ".a") ||
			strings.HasSuffix(name, ".so") ||
			strings.HasSuffix(name, ".h") ||
			strings.HasSuffix(name, ".hpp") ||
			strings.HasSuffix(name, ".c") ||
			strings.HasSuffix(name, ".cpp") ||
			strings.HasSuffix(name, ".cmake") ||
			name == "Makefile" ||
			name == "CMakeLists.txt" ||
			name == "CMakeCache.txt" {
			continue
		}

		// Check if it's executable
		info, err := entry.Info()
		if err != nil {
			continue
		}
		if info.Mode()&0111 != 0 {
			return filepath.Join(dir, name)
		}
	}
	return ""
}

// pullImage pulls the GCC image if it doesn't exist locally.
func (b *CppBuilder) pullImage(vm *VendorManager) error {
	ctx := context.Background()

	images, err := vm.dockerClient.ImageList(ctx, image.ListOptions{})
	if err != nil {
		return err
	}

	imageExists := false
	for _, img := range images {
		for _, tag := range img.RepoTags {
			if tag == cppImage {
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
	blue.Printf("Pulling Docker image: %s...\n", cppImage)

	reader, err := vm.dockerClient.ImagePull(ctx, cppImage, image.PullOptions{})
	if err != nil {
		return err
	}
	defer reader.Close()

	io.Copy(io.Discard, reader)
	return nil
}

// runBuildContainer creates and runs a container for C/C++ building.
func (b *CppBuilder) runBuildContainer(vm *VendorManager, isCMake bool) (string, error) {
	ctx := context.Background()

	ctrName := fmt.Sprintf("%scpp-%d", vendorContainerName, time.Now().Unix())

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

	var buildCmd string
	if isCMake {
		// CMake build: create build dir, configure, and build
		buildCmd = `mkdir -p build && cd build && cmake .. && make 2>&1`
	} else {
		// Makefile build
		buildCmd = `make 2>&1`
	}

	config := &container.Config{
		Image: cppImage,
		Cmd: []string{
			"bash", "-c",
			buildCmd,
		},
		WorkingDir: "/project",
		Env:        getCppBuildEnvVars(),
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
			red.Printf("C/C++ build failed with exit code %d\n", status.StatusCode)
			if logs != "" {
				red.Printf("Full error log:\n%s\n", logs)
			}
			return resp.ID, fmt.Errorf("C/C++ build failed with exit code %d", status.StatusCode)
		}
	}

	return resp.ID, nil
}

// getCppBuildEnvVars returns environment variables for C/C++ build.
func getCppBuildEnvVars() []string {
	secrets, err := LoadBuildSecrets()
	if err != nil || secrets == nil {
		return nil
	}

	var envVars []string
	if secrets.GitHubToken != "" {
		// Configure git to use GitHub token for private dependencies
		envVars = append(envVars, fmt.Sprintf("GITHUB_TOKEN=%s", secrets.GitHubToken))
	}
	return envVars
}

// Legacy functions for backwards compatibility

// IsCppProject checks if the project is a C/C++ project.
func IsCppProject(projectPath string) bool {
	builder := &CppBuilder{}
	return builder.Detect(projectPath)
}

// ShouldBuildCpp checks if C/C++ project needs building.
func ShouldBuildCpp(projectPath string) (bool, error) {
	builder := &CppBuilder{}
	return builder.ShouldBuild(projectPath)
}

// BuildCppIfNeeded builds C/C++ project only if needed and Docker is available.
func BuildCppIfNeeded(projectPath string) error {
	builder := &CppBuilder{}

	shouldBuild, err := builder.ShouldBuild(projectPath)
	if err != nil {
		return err
	}

	if !shouldBuild {
		return nil
	}

	blue := color.New(color.FgBlue)
	red := color.New(color.FgRed)

	blue.Println("C/C++ project detected, building...")

	if err := CheckDockerInstalled(); err != nil {
		red.Printf("Docker required for C/C++ build: %v\n", err)
		red.Println("Solution: Install Docker or pre-build the project")
		return fmt.Errorf("docker unavailable for C/C++ build")
	}

	vm, err := NewVendorManager(projectPath)
	if err != nil {
		return fmt.Errorf("failed to initialize vendor manager: %v", err)
	}
	defer vm.Close()

	if err := vm.CheckDockerRunning(); err != nil {
		red.Printf("Docker daemon not running: %v\n", err)
		red.Println("Solution: Start Docker or pre-build the project")
		return fmt.Errorf("docker daemon not running for C/C++ build")
	}

	return builder.Build(vm)
}
