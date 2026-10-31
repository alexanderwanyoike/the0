package internal

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

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
	if err := vm.runner.EnsureImage(dotnetSdkImage); err != nil {
		return fmt.Errorf("failed to pull .NET image: %v", err)
	}

	// Run build container
	containerID, err := b.runBuildContainer(vm)
	if err != nil {
		return fmt.Errorf("failed to run .NET build container: %v", err)
	}
	defer vm.runner.Remove(containerID)

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

// runBuildContainer creates and runs a container for .NET building.
func (b *DotnetBuilder) runBuildContainer(vm *VendorManager) (string, error) {
	absProjectPath, err := filepath.Abs(vm.projectPath)
	if err != nil {
		return "", fmt.Errorf("failed to get absolute project path: %v", err)
	}

	// Build command - publish each .csproj to its own temp dir, then merge outputs
	// This avoids dotnet publish cleaning the shared output directory on each run
	buildCmd := `mkdir -p /project/bin/Release/net8.0/publish && for PROJ in *.csproj; do [ -f "$PROJ" ] && PNAME=$(basename "$PROJ" .csproj) && dotnet publish "$PROJ" -c Release -o "/tmp/$PNAME-publish" 2>&1 && cp -r "/tmp/$PNAME-publish/"* /project/bin/Release/net8.0/publish/ || exit 1; done`

	uid, gid := hostUserIDs()
	return vm.runner.Run(BuildStep{
		NamePrefix:      "dotnet-",
		Image:           dotnetSdkImage,
		Cmd:             []string{"sh", "-c", buildCmd},
		WorkingDir:      "/project",
		Env:             getDotnetBuildEnvVars(),
		User:            fmt.Sprintf("%s:%s", uid, gid),
		Binds:           []string{fmt.Sprintf("%s:/project", absProjectPath)},
		FailureHeadline: ".NET build",
		FailedCommand:   "dotnet publish",
	})
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

	// Pass NuGet token for private feeds (referenced in nuget.config as %NUGET_TOKEN%)
	if secrets.NuGetToken != "" {
		envVars = append(envVars, fmt.Sprintf("NUGET_TOKEN=%s", secrets.NuGetToken))
	}

	// Pass GitHub token for git-based dependencies
	if secrets.GitHubToken != "" {
		envVars = append(envVars, fmt.Sprintf("GITHUB_TOKEN=%s", secrets.GitHubToken))
	}

	return envVars
}
