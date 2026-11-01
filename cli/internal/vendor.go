package internal

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

	"github.com/fatih/color"
)

const (
	vendorContainerName = "the0-vendor-"
)

// VendorManager runs a project's dependency installs and builds
type VendorManager struct {
	runner      BuildRunner
	projectPath string
	runtime     string
}

// NewVendorManager runs projectPath's installs and builds with runner.
func NewVendorManager(projectPath string, runner BuildRunner) *VendorManager {
	return &VendorManager{runner: runner, projectPath: projectPath}
}

// CheckDockerInstalled verifies if Docker is installed and accessible
func CheckDockerInstalled() error {
	cmd := exec.Command("docker", "--version")
	err := cmd.Run()
	if err != nil {
		return fmt.Errorf("Docker is not installed or not accessible. Please install Docker and ensure it's in your PATH")
	}
	return nil
}

// CheckDockerRunning verifies if Docker daemon is running
func (vm *VendorManager) CheckDockerRunning() error {
	if err := vm.runner.Ping(); err != nil {
		return fmt.Errorf("Docker daemon is not running. Please start Docker and try again")
	}
	return nil
}

// Close closes the runner's connection
func (vm *VendorManager) Close() error {
	return vm.runner.Close()
}

// verifyVendoredFiles verifies that vendoring was successful
func (vm *VendorManager) verifyVendoredFiles(vendorPath string) error {
	entries, err := os.ReadDir(vendorPath)
	if err != nil {
		return fmt.Errorf("failed to read vendor directory: %v", err)
	}

	if len(entries) == 0 {
		return fmt.Errorf("no packages were vendored - check your requirements.txt file")
	}

	// Calculate total size of vendored files
	totalSize, err := vm.calculateDirectorySize(vendorPath)
	if err != nil {
		return fmt.Errorf("failed to calculate vendor directory size: %v", err)
	}

	green := color.New(color.FgGreen)
	blue := color.New(color.FgBlue)

	// Convert size to human readable format
	sizeStr := vm.formatFileSize(totalSize)
	green.Printf("v Successfully vendored %d packages (%s)\n", len(entries), sizeStr)

	// List key packages for verification
	blue.Println("Vendored packages:")
	count := 0
	for _, entry := range entries {
		if count >= 10 { // Show max 10 packages
			blue.Printf("   ... and %d more packages\n", len(entries)-10)
			break
		}
		if entry.IsDir() {
			blue.Printf("   - %s\n", entry.Name())
			count++
		}
	}

	// Warn if the vendor directory seems unusually small
	if totalSize < 1024*1024 { // Less than 1MB
		yellow := color.New(color.FgYellow)
		yellow.Printf("Warning: Vendor directory is only %s - this may indicate incomplete installation\n", sizeStr)
	}

	return nil
}

// calculateDirectorySize calculates the total size of all files in a directory recursively
func (vm *VendorManager) calculateDirectorySize(dirPath string) (int64, error) {
	var totalSize int64

	err := filepath.Walk(dirPath, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if !info.IsDir() {
			totalSize += info.Size()
		}
		return nil
	})

	return totalSize, err
}

// formatFileSize formats a file size in bytes to a human-readable string
func (vm *VendorManager) formatFileSize(bytes int64) string {
	const unit = 1024
	if bytes < unit {
		return fmt.Sprintf("%d B", bytes)
	}

	div, exp := int64(unit), 0
	for n := bytes / unit; n >= unit; n /= unit {
		div *= unit
		exp++
	}

	return fmt.Sprintf("%.1f %cB", float64(bytes)/float64(div), "KMGTPE"[exp])
}

// getBuildEnvVars returns environment variables for the build container
func getBuildEnvVars() []string {
	secrets, err := LoadBuildSecrets()
	if err != nil || secrets == nil {
		return nil
	}

	var envVars []string
	if secrets.GitHubToken != "" {
		envVars = append(envVars, fmt.Sprintf("GITHUB_TOKEN=%s", secrets.GitHubToken))
	}
	if secrets.PipIndexURL != "" {
		envVars = append(envVars, fmt.Sprintf("PIP_EXTRA_INDEX_URL=%s", secrets.PipIndexURL))
	}
	return envVars
}

// CleanupVendoring removes vendor directories and ZIP files
func CleanupVendoring(projectPath string) {
	// Remove vendor directory
	vendorPath := filepath.Join(projectPath, vendorDir)
	if _, err := os.Stat(vendorPath); err == nil {
		os.RemoveAll(vendorPath)
	}

	// Remove node_modules directory
	nodeModulesPath := filepath.Join(projectPath, nodeModulesDir)
	if _, err := os.Stat(nodeModulesPath); err == nil {
		os.RemoveAll(nodeModulesPath)
	}

	// Remove any bot ZIP files
	files, err := filepath.Glob(filepath.Join(projectPath, "bot_*.zip"))
	if err == nil {
		for _, file := range files {
			os.Remove(file)
		}
	}
}

// PerformVendoringIfNeeded performs vendoring only if needed and Docker is available.
// This is a convenience function that uses VendorDependenciesIfNeeded from vendor_interface.go.
func PerformVendoringIfNeeded(projectPath, runtime string) error {
	return VendorDependenciesIfNeeded(projectPath, runtime)
}
