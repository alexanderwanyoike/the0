package internal

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/fatih/color"
)

const cppImage = "mcr.microsoft.com/devcontainers/cpp:1-debian-12" // Microsoft's official C++ devcontainer with gcc, cmake, git

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
	if err := vm.runner.EnsureImage(cppImage); err != nil {
		return fmt.Errorf("failed to pull GCC image: %v", err)
	}

	// Run build container
	containerID, err := b.runBuildContainer(vm, isCMake)
	if err != nil {
		return fmt.Errorf("failed to run C/C++ build container: %v", err)
	}
	defer vm.runner.Remove(containerID)

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

// runBuildContainer creates and runs a container for C/C++ building.
func (b *CppBuilder) runBuildContainer(vm *VendorManager, isCMake bool) (string, error) {
	absProjectPath, err := filepath.Abs(vm.projectPath)
	if err != nil {
		return "", fmt.Errorf("failed to get absolute project path: %v", err)
	}

	var buildCmd string
	if isCMake {
		// CMake build: create build dir, configure, and build
		buildCmd = `mkdir -p build && cd build && cmake .. && make 2>&1`
	} else {
		// Makefile build
		buildCmd = `make 2>&1`
	}

	uid, gid := hostUserIDs()
	return vm.runner.Run(BuildStep{
		NamePrefix:      "cpp-",
		Image:           cppImage,
		Cmd:             []string{"bash", "-c", buildCmd},
		WorkingDir:      "/project",
		Env:             getCppBuildEnvVars(),
		User:            fmt.Sprintf("%s:%s", uid, gid),
		Binds:           []string{fmt.Sprintf("%s:/project", absProjectPath)},
		FailureHeadline: "C/C++ build",
		FailedCommand:   "C/C++ build",
	})
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
