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

const dotnetSdkImage = "mcr.microsoft.com/dotnet/sdk:8.0"

// DotnetBuilder handles C#/.NET project detection and building.
type DotnetBuilder struct{}

func init() {
	RegisterBuilder(&DotnetBuilder{})
}

// Name returns the language name for logging.
func (b *DotnetBuilder) Name() string {
	return "C#/.NET"
}

// DockerImage returns the Docker image for .NET builds.
func (b *DotnetBuilder) DockerImage() string {
	return dotnetSdkImage
}

// Detect checks if any .csproj file exists in the project.
func (b *DotnetBuilder) Detect(projectPath string) bool {
	files, err := filepath.Glob(filepath.Join(projectPath, "*.csproj"))
	if err != nil {
		return false
	}
	return len(files) > 0
}

// ShouldBuild checks if .NET project needs building.
func (b *DotnetBuilder) ShouldBuild(projectPath string) (bool, error) {
	files, err := filepath.Glob(filepath.Join(projectPath, "*.csproj"))
	if err != nil {
		return false, err
	}
	if len(files) == 0 {
		return false, nil
	}
	return true, nil
}

// Build performs the Docker-based .NET build.
func (b *DotnetBuilder) Build(vm *VendorManager) error {
	blue := color.New(color.FgBlue)
	green := color.New(color.FgGreen)

	blue.Println("Building .NET project...")

	// Pull .NET SDK image if needed
	if err := b.pullImage(vm); err != nil {
		return fmt.Errorf("failed to pull .NET image: %v", err)
	}

	// Run build container
	containerID, err := b.runBuildContainer(vm)
	if err != nil {
		return fmt.Errorf("failed to run .NET build container: %v", err)
	}
	defer vm.cleanupContainer(containerID)

	// Verify output was created
	binaryPath := b.FindBinary(vm.projectPath)
	if binaryPath == "" {
		return fmt.Errorf(".NET build did not produce output in bin/Release/net8.0/publish/")
	}

	// Get binary info
	binaryInfo, err := os.Stat(binaryPath)
	if err != nil {
		return fmt.Errorf("failed to stat binary: %v", err)
	}

	sizeStr := vm.formatFileSize(binaryInfo.Size())
	green.Printf("v .NET project built: %s (%s)\n", filepath.Base(binaryPath), sizeStr)
	return nil
}

// FindBinary finds the built DLL in bin/Release/net8.0/publish/.
func (b *DotnetBuilder) FindBinary(projectPath string) string {
	publishDir := filepath.Join(projectPath, "bin", "Release", "net8.0", "publish")

	entries, err := os.ReadDir(publishDir)
	if err != nil {
		return ""
	}

	// Find project name from .csproj file
	csprojFiles, _ := filepath.Glob(filepath.Join(projectPath, "*.csproj"))
	if len(csprojFiles) == 0 {
		return ""
	}

	projectName := strings.TrimSuffix(filepath.Base(csprojFiles[0]), ".csproj")

	// Look for the main DLL
	for _, entry := range entries {
		if entry.IsDir() {
			continue
		}
		name := entry.Name()
		if name == projectName+".dll" {
			return filepath.Join(publishDir, name)
		}
	}
	return ""
}

// pullImage pulls the .NET SDK image if it doesn't exist locally.
func (b *DotnetBuilder) pullImage(vm *VendorManager) error {
	ctx := context.Background()

	images, err := vm.dockerClient.ImageList(ctx, image.ListOptions{})
	if err != nil {
		return err
	}

	imageExists := false
	for _, img := range images {
		for _, tag := range img.RepoTags {
			if tag == dotnetSdkImage {
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
	blue.Printf("Pulling Docker image: %s...\n", dotnetSdkImage)

	reader, err := vm.dockerClient.ImagePull(ctx, dotnetSdkImage, image.PullOptions{})
	if err != nil {
		return err
	}
	defer reader.Close()

	io.Copy(io.Discard, reader)
	return nil
}

// runBuildContainer creates and runs a container for .NET building.
func (b *DotnetBuilder) runBuildContainer(vm *VendorManager) (string, error) {
	ctx := context.Background()

	ctrName := fmt.Sprintf("%sdotnet-%d", vendorContainerName, time.Now().Unix())

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

	// Build command - add GitHub Packages source if token available, then build
	buildCmd := `
if [ -n "$GITHUB_TOKEN" ]; then
  dotnet nuget add source https://nuget.pkg.github.com/alexanderwanyoike/index.json --name github --username "$GITHUB_USERNAME" --password "$GITHUB_TOKEN" --store-password-in-clear-text 2>/dev/null || true
fi
dotnet publish -c Release -o /project/bin/Release/net8.0/publish 2>&1`

	config := &container.Config{
		Image: dotnetSdkImage,
		Cmd: []string{
			"sh", "-c",
			buildCmd,
		},
		WorkingDir: "/project",
		Env:        getDotnetBuildEnvVars(),
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
			red.Printf(".NET build failed with exit code %d\n", status.StatusCode)
			if logs != "" {
				red.Printf("Full error log:\n%s\n", logs)
			}
			return resp.ID, fmt.Errorf("dotnet publish failed with exit code %d", status.StatusCode)
		}
	}

	return resp.ID, nil
}

// getDotnetBuildEnvVars returns environment variables for .NET build.
func getDotnetBuildEnvVars() []string {
	envVars := []string{
		"DOTNET_CLI_HOME=/tmp",
		"DOTNET_NOLOGO=1",
		"DOTNET_SKIP_FIRST_TIME_EXPERIENCE=1",
	}

	secrets, err := LoadBuildSecrets()
	if err != nil || secrets == nil {
		return envVars
	}

	if secrets.GitHubToken != "" {
		envVars = append(envVars, fmt.Sprintf("GITHUB_TOKEN=%s", secrets.GitHubToken))
		envVars = append(envVars, fmt.Sprintf("GITHUB_USERNAME=%s", secrets.GetGitHubUsername()))
	}
	return envVars
}

// Legacy functions for backwards compatibility

// IsDotnetProject checks if the project is a .NET project.
func IsDotnetProject(projectPath string) bool {
	builder := &DotnetBuilder{}
	return builder.Detect(projectPath)
}

// ShouldBuildDotnet checks if .NET project needs building.
func ShouldBuildDotnet(projectPath string) (bool, error) {
	builder := &DotnetBuilder{}
	return builder.ShouldBuild(projectPath)
}

// BuildDotnetIfNeeded builds .NET project only if needed and Docker is available.
func BuildDotnetIfNeeded(projectPath string) error {
	builder := &DotnetBuilder{}

	shouldBuild, err := builder.ShouldBuild(projectPath)
	if err != nil {
		return err
	}

	if !shouldBuild {
		return nil
	}

	blue := color.New(color.FgBlue)
	red := color.New(color.FgRed)

	blue.Println("C#/.NET project detected, building...")

	if err := CheckDockerInstalled(); err != nil {
		red.Printf("Docker required for .NET build: %v\n", err)
		red.Println("Solution: Install Docker or pre-build with 'dotnet publish -c Release'")
		return fmt.Errorf("docker unavailable for .NET build")
	}

	vm, err := NewVendorManager(projectPath)
	if err != nil {
		return fmt.Errorf("failed to initialize vendor manager: %v", err)
	}
	defer vm.Close()

	if err := vm.CheckDockerRunning(); err != nil {
		red.Printf("Docker daemon not running: %v\n", err)
		red.Println("Solution: Start Docker or pre-build with 'dotnet publish -c Release'")
		return fmt.Errorf("docker daemon not running for .NET build")
	}

	return builder.Build(vm)
}
