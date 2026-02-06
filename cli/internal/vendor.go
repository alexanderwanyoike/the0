package internal

import (
	"context"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"

	"github.com/docker/docker/api/types/container"
	"github.com/docker/docker/client"
	"github.com/fatih/color"
)

const (
	vendorContainerName = "the0-vendor-"
)

// VendorManager handles Docker-based package vendoring
type VendorManager struct {
	dockerClient *client.Client
	projectPath  string
}

// NewVendorManager creates a new vendor manager instance
func NewVendorManager(projectPath string) (*VendorManager, error) {
	cli, err := client.NewClientWithOpts(client.FromEnv, client.WithAPIVersionNegotiation())
	if err != nil {
		return nil, fmt.Errorf("failed to create Docker client: %v", err)
	}

	return &VendorManager{
		dockerClient: cli,
		projectPath:  projectPath,
	}, nil
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
	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()

	_, err := vm.dockerClient.Ping(ctx)
	if err != nil {
		return fmt.Errorf("Docker daemon is not running. Please start Docker and try again")
	}
	return nil
}

// Close closes the Docker client connection
func (vm *VendorManager) Close() error {
	if vm.dockerClient != nil {
		return vm.dockerClient.Close()
	}
	return nil
}

// cleanupContainer removes a container
func (vm *VendorManager) cleanupContainer(containerID string) {
	ctx := context.Background()
	vm.dockerClient.ContainerRemove(ctx, containerID, container.RemoveOptions{Force: true})
}

// getContainerLogs retrieves logs from a container for debugging
func (vm *VendorManager) getContainerLogs(containerID string) (string, error) {
	ctx := context.Background()

	reader, err := vm.dockerClient.ContainerLogs(ctx, containerID, container.LogsOptions{
		ShowStdout: true,
		ShowStderr: true,
	})
	if err != nil {
		return "", err
	}
	defer reader.Close()

	logs, err := io.ReadAll(reader)
	if err != nil {
		return "", err
	}

	return string(logs), nil
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

// cleanDockerLogOutput removes Docker log headers from raw log data
func (vm *VendorManager) cleanDockerLogOutput(logData []byte) string {
	var output strings.Builder
	i := 0

	for i < len(logData) {
		// Docker log format: 8-byte header + message
		// Header: [stream_type][0][0][0][size_bytes_3][size_bytes_2][size_bytes_1][size_bytes_0]
		if i+8 > len(logData) {
			// Not enough bytes for header, append remaining
			output.Write(logData[i:])
			break
		}

		// Extract message size from header
		size := int(logData[i+4])<<24 | int(logData[i+5])<<16 | int(logData[i+6])<<8 | int(logData[i+7])

		// Skip header
		i += 8

		// Extract message
		if i+size <= len(logData) {
			output.Write(logData[i : i+size])
			i += size
		} else {
			// Invalid size, append remaining
			output.Write(logData[i:])
			break
		}
	}

	return output.String()
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
func PerformVendoringIfNeeded(projectPath string) error {
	return VendorDependenciesIfNeeded(projectPath)
}

// Legacy compatibility functions - these delegate to the new vendor implementations

// ShouldPerformVendoring checks if Python vendoring should be performed
func ShouldPerformVendoring(projectPath string) (bool, error) {
	vendor := &PythonVendor{}
	return vendor.ShouldVendor(projectPath)
}

// ShouldPerformNodeVendoring checks if Node.js vendoring should be performed
func ShouldPerformNodeVendoring(projectPath string) (bool, error) {
	vendor := &NodeVendor{}
	return vendor.ShouldVendor(projectPath)
}

// ShouldBuildFrontend checks if frontend building should be performed
func ShouldBuildFrontend(projectPath string) (bool, error) {
	vendor := &FrontendVendor{}
	return vendor.ShouldVendor(projectPath)
}

// Legacy VendorManager methods for backward compatibility

// CheckRequirementsFile checks if requirements.txt exists in the project
func (vm *VendorManager) CheckRequirementsFile() bool {
	vendor := &PythonVendor{}
	return vendor.Detect(vm.projectPath)
}

// CheckPackageJsonFile checks if package.json exists in the project
func (vm *VendorManager) CheckPackageJsonFile() bool {
	vendor := &NodeVendor{}
	return vendor.Detect(vm.projectPath)
}

// CheckTypeScriptFiles checks if TypeScript files exist in the project
func (vm *VendorManager) CheckTypeScriptFiles() bool {
	vendor := &NodeVendor{}
	return vendor.hasTypeScriptFiles(vm.projectPath)
}

// CheckFrontendExists checks if a frontend directory with package.json exists
func (vm *VendorManager) CheckFrontendExists() bool {
	vendor := &FrontendVendor{}
	return vendor.Detect(vm.projectPath)
}

// PerformVendoring performs the complete Python vendoring process
func (vm *VendorManager) PerformVendoring() error {
	vendor := &PythonVendor{}
	return vendor.Vendor(vm)
}

// PerformNodeVendoring performs Node.js package vendoring
func (vm *VendorManager) PerformNodeVendoring(hasTypeScript bool) error {
	vendor := &NodeVendor{}
	return vendor.Vendor(vm)
}

// BuildFrontend builds the frontend bundle in a Docker container
func (vm *VendorManager) BuildFrontend() error {
	vendor := &FrontendVendor{}
	return vendor.Vendor(vm)
}

// getPythonInstallCommand returns the pip install command with proper ownership
// This is exposed for testing purposes
func (vm *VendorManager) getPythonInstallCommand() string {
	vendor := &PythonVendor{}
	return vendor.getInstallCommand()
}

// BackupExistingNodeModules creates a backup of existing node_modules directory (exported for testing)
func (vm *VendorManager) BackupExistingNodeModules() error {
	vendor := &NodeVendor{}
	return vendor.backupExistingNodeModules(vm.projectPath)
}

// RestoreNodeModulesBackup restores node_modules from backup on failure (exported for testing)
func (vm *VendorManager) RestoreNodeModulesBackup() error {
	vendor := &NodeVendor{}
	return vendor.restoreNodeModulesBackup(vm.projectPath)
}

// CleanupNodeModulesBackup removes backup after successful vendoring (exported for testing)
func (vm *VendorManager) CleanupNodeModulesBackup() error {
	vendor := &NodeVendor{}
	return vendor.cleanupNodeModulesBackup(vm.projectPath)
}
