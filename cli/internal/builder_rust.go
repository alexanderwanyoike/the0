package internal

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

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
	if err := vm.runner.EnsureImage(rustImage); err != nil {
		return fmt.Errorf("failed to pull Rust image: %v", err)
	}

	// Run build container
	containerID, err := b.runBuildContainer(vm)
	if err != nil {
		return fmt.Errorf("failed to run Rust build container: %v", err)
	}
	defer vm.runner.Remove(containerID)

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

// runBuildContainer creates and runs a container for Rust building.
func (b *RustBuilder) runBuildContainer(vm *VendorManager) (string, error) {
	absProjectPath, err := filepath.Abs(vm.projectPath)
	if err != nil {
		return "", fmt.Errorf("failed to get absolute project path: %v", err)
	}

	// Build command
	buildCmd := "cargo build --release 2>&1"

	uid, gid := hostUserIDs()
	return vm.runner.Run(BuildStep{
		NamePrefix:      "rust-",
		Image:           rustImage,
		Cmd:             []string{"sh", "-c", buildCmd},
		WorkingDir:      "/project",
		Env:             getRustBuildEnvVars(),
		User:            fmt.Sprintf("%s:%s", uid, gid),
		Binds:           []string{fmt.Sprintf("%s:/project", absProjectPath)},
		FailureHeadline: "Rust build",
		FailedCommand:   "cargo build",
	})
}

// getRustBuildEnvVars returns environment variables for Rust build.
func getRustBuildEnvVars() []string {
	secrets, err := LoadBuildSecrets()
	if err != nil || secrets == nil {
		return nil
	}

	var envVars []string

	// Pass GitHub token for private git dependencies
	if secrets.GitHubToken != "" {
		envVars = append(envVars, fmt.Sprintf("GITHUB_TOKEN=%s", secrets.GitHubToken))
		envVars = append(envVars, "CARGO_NET_GIT_FETCH_WITH_CLI=true")
		envVars = append(envVars, "GIT_TERMINAL_PROMPT=0")
	}

	// Pass Cargo registry token for private registries
	if secrets.CargoRegistryToken != "" {
		envVars = append(envVars, fmt.Sprintf("CARGO_REGISTRY_TOKEN=%s", secrets.CargoRegistryToken))
	}

	return envVars
}
