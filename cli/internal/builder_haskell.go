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

const haskellImage = "haskell:9.6"

// HaskellBuilder handles Haskell project detection and building.
type HaskellBuilder struct{}

func init() {
	RegisterBuilder(&HaskellBuilder{})
}

// Name returns the language name for logging.
func (b *HaskellBuilder) Name() string {
	return "Haskell"
}

// DockerImage returns the Docker image for Haskell builds.
func (b *HaskellBuilder) DockerImage() string {
	return haskellImage
}

// Detect checks if any .cabal file exists in the project.
func (b *HaskellBuilder) Detect(projectPath string) bool {
	files, err := filepath.Glob(filepath.Join(projectPath, "*.cabal"))
	if err != nil {
		return false
	}
	return len(files) > 0
}

// ShouldBuild checks if Haskell project needs building.
func (b *HaskellBuilder) ShouldBuild(projectPath string) (bool, error) {
	files, err := filepath.Glob(filepath.Join(projectPath, "*.cabal"))
	if err != nil {
		return false, err
	}
	if len(files) == 0 {
		return false, nil
	}
	return true, nil
}

// Build performs the Docker-based Haskell build.
func (b *HaskellBuilder) Build(vm *VendorManager) error {
	blue := color.New(color.FgBlue)
	green := color.New(color.FgGreen)

	blue.Println("Building Haskell project...")

	// Pull Haskell image if needed
	if err := b.pullImage(vm); err != nil {
		return fmt.Errorf("failed to pull Haskell image: %v", err)
	}

	// Run build container
	containerID, err := b.runBuildContainer(vm)
	if err != nil {
		return fmt.Errorf("failed to run Haskell build container: %v", err)
	}
	defer vm.cleanupContainer(containerID)

	// Verify binaries were created in bin/
	binaries := b.FindAllBinaries(vm.projectPath)
	if len(binaries) == 0 {
		return fmt.Errorf("haskell build did not produce any binaries in bin/")
	}

	// Report all built binaries
	for _, binaryPath := range binaries {
		binaryInfo, err := os.Stat(binaryPath)
		if err != nil {
			continue
		}
		sizeStr := vm.formatFileSize(binaryInfo.Size())
		green.Printf("v Haskell binary built: %s (%s)\n", filepath.Base(binaryPath), sizeStr)
	}
	return nil
}

// FindBinary finds the first built binary in bin/.
// After build, binaries are copied to bin/ for simplified paths.
func (b *HaskellBuilder) FindBinary(projectPath string) string {
	binaries := b.FindAllBinaries(projectPath)
	if len(binaries) > 0 {
		return binaries[0]
	}
	return ""
}

// FindAllBinaries finds all built binaries in bin/.
func (b *HaskellBuilder) FindAllBinaries(projectPath string) []string {
	binDir := filepath.Join(projectPath, "bin")

	entries, err := os.ReadDir(binDir)
	if err != nil {
		return nil
	}

	var binaries []string
	for _, entry := range entries {
		if entry.IsDir() {
			continue
		}

		// Check if it's executable
		info, err := entry.Info()
		if err != nil {
			continue
		}
		if info.Mode()&0111 != 0 {
			binaries = append(binaries, filepath.Join(binDir, entry.Name()))
		}
	}
	return binaries
}

// pullImage pulls the Haskell image if it doesn't exist locally.
func (b *HaskellBuilder) pullImage(vm *VendorManager) error {
	ctx := context.Background()

	images, err := vm.dockerClient.ImageList(ctx, image.ListOptions{})
	if err != nil {
		return err
	}

	imageExists := false
	for _, img := range images {
		for _, tag := range img.RepoTags {
			if tag == haskellImage {
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
	blue.Printf("Pulling Docker image: %s...\n", haskellImage)

	reader, err := vm.dockerClient.ImagePull(ctx, haskellImage, image.PullOptions{})
	if err != nil {
		return err
	}
	defer reader.Close()

	io.Copy(io.Discard, reader)
	return nil
}

// runBuildContainer creates and runs a container for Haskell building.
func (b *HaskellBuilder) runBuildContainer(vm *VendorManager) (string, error) {
	ctx := context.Background()

	ctrName := fmt.Sprintf("%shaskell-%d", vendorContainerName, time.Now().Unix())

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

	// Build command: build with optimizations, then copy all executables to bin/
	// This provides simple, predictable paths like bin/my-bot instead of
	// dist-newstyle/build/x86_64-linux/ghc-9.6.7/pkg-1.0.0/x/my-bot/opt/build/my-bot/my-bot
	buildCmd := `set -e
cabal update
cabal build --enable-optimization=2

# Create bin directory
mkdir -p bin

# Find and copy all executables from opt/build directories
# These are the optimized binaries built by cabal
find dist-newstyle -path "*/opt/build/*/*" -type f -executable 2>/dev/null | while read exe; do
    name=$(basename "$exe")
    # Skip autogen files and other non-binary files
    case "$name" in
        *.hi|*.o|*.dyn_*|Paths_*) continue ;;
    esac
    cp "$exe" bin/
    echo "Copied $name to bin/"
done

# Verify at least one binary was copied
if [ -z "$(ls -A bin/ 2>/dev/null)" ]; then
    echo "Warning: No executables found in dist-newstyle opt/build directories"
    exit 1
fi
`

	config := &container.Config{
		Image: haskellImage,
		Cmd: []string{
			"sh", "-c",
			buildCmd,
		},
		WorkingDir: "/project",
		Env:        getHaskellBuildEnvVars(),
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
			red.Printf("Haskell build failed with exit code %d\n", status.StatusCode)
			if logs != "" {
				red.Printf("Full error log:\n%s\n", logs)
			}
			return resp.ID, fmt.Errorf("cabal build failed with exit code %d", status.StatusCode)
		}
	}

	return resp.ID, nil
}

// getHaskellBuildEnvVars returns environment variables for Haskell build.
func getHaskellBuildEnvVars() []string {
	envVars := []string{
		"HOME=/tmp",
		"CABAL_DIR=/tmp/.cabal",
	}

	secrets, err := LoadBuildSecrets()
	if err != nil || secrets == nil {
		return envVars
	}

	if secrets.GitHubToken != "" {
		// Configure git to use GitHub token for private dependencies
		envVars = append(envVars, fmt.Sprintf("GITHUB_TOKEN=%s", secrets.GitHubToken))
	}
	return envVars
}

// Legacy functions for backwards compatibility

// IsHaskellProject checks if the project is a Haskell project.
func IsHaskellProject(projectPath string) bool {
	builder := &HaskellBuilder{}
	return builder.Detect(projectPath)
}

// ShouldBuildHaskell checks if Haskell project needs building.
func ShouldBuildHaskell(projectPath string) (bool, error) {
	builder := &HaskellBuilder{}
	return builder.ShouldBuild(projectPath)
}

// BuildHaskellIfNeeded builds Haskell project only if needed and Docker is available.
func BuildHaskellIfNeeded(projectPath string) error {
	builder := &HaskellBuilder{}

	shouldBuild, err := builder.ShouldBuild(projectPath)
	if err != nil {
		return err
	}

	if !shouldBuild {
		return nil
	}

	blue := color.New(color.FgBlue)
	red := color.New(color.FgRed)

	blue.Println("Haskell project detected, building...")

	if err := CheckDockerInstalled(); err != nil {
		red.Printf("Docker required for Haskell build: %v\n", err)
		red.Println("Solution: Install Docker or pre-build with 'cabal build --enable-optimization=2'")
		return fmt.Errorf("docker unavailable for haskell build")
	}

	vm, err := NewVendorManager(projectPath)
	if err != nil {
		return fmt.Errorf("failed to initialize vendor manager: %v", err)
	}
	defer vm.Close()

	if err := vm.CheckDockerRunning(); err != nil {
		red.Printf("Docker daemon not running: %v\n", err)
		red.Println("Solution: Start Docker or pre-build with 'cabal build --enable-optimization=2'")
		return fmt.Errorf("docker daemon not running for haskell build")
	}

	return builder.Build(vm)
}
