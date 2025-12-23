package internal

import (
	"context"
	"fmt"
	"io"
	"os"
	"os/exec"
	"os/user"
	"path/filepath"
	"strings"
	"time"

	"github.com/docker/docker/api/types/container"
	"github.com/docker/docker/api/types/image"
	"github.com/docker/docker/client"
	"github.com/fatih/color"
)

const (
	pythonImage          = "python:3.11-slim"
	nodeImage            = "node:20-slim"
	vendorDir            = "vendor"
	nodeModulesDir       = "node_modules"
	nodeModulesBackupDir = "_node_modules_backup"
	containerName        = "the0-vendor-"
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

// CheckRequirementsFile checks if requirements.txt exists in the project
func (vm *VendorManager) CheckRequirementsFile() bool {
	requirementsPath := filepath.Join(vm.projectPath, "requirements.txt")
	_, err := os.Stat(requirementsPath)
	return err == nil
}

// CheckPackageJsonFile checks if package.json exists in the project
func (vm *VendorManager) CheckPackageJsonFile() bool {
	packageJsonPath := filepath.Join(vm.projectPath, "package.json")
	_, err := os.Stat(packageJsonPath)
	return err == nil
}

// CheckTypeScriptFiles checks if TypeScript files exist in the project
func (vm *VendorManager) CheckTypeScriptFiles() bool {
	files, err := filepath.Glob(filepath.Join(vm.projectPath, "*.ts"))
	if err != nil {
		return false
	}
	return len(files) > 0
}

// PerformVendoring performs the complete Python vendoring process
func (vm *VendorManager) PerformVendoring() error {
	blue := color.New(color.FgBlue)
	green := color.New(color.FgGreen)

	blue.Println("Installing Python dependencies...")

	// Step 1: Pull Python image if needed
	if err := vm.pullPythonImage(); err != nil {
		return fmt.Errorf("failed to pull Python image: %v", err)
	}

	// Step 2: Create vendor directory
	vendorPath := filepath.Join(vm.projectPath, vendorDir)
	if err := os.RemoveAll(vendorPath); err != nil {
		return fmt.Errorf("failed to clean vendor directory: %v", err)
	}
	if err := os.MkdirAll(vendorPath, 0755); err != nil {
		return fmt.Errorf("failed to create vendor directory: %v", err)
	}

	// Step 3: Run vendoring container
	containerID, err := vm.runVendorContainer()
	if err != nil {
		return fmt.Errorf("failed to run vendoring container: %v", err)
	}
	defer vm.cleanupContainer(containerID)

	// Step 4: Verify vendored files
	if err := vm.verifyVendoredFiles(vendorPath); err != nil {
		return fmt.Errorf("failed to verify vendored files: %v", err)
	}

	green.Println("v Python dependencies installed")
	return nil
}

// PerformNodeVendoring performs Node.js package vendoring
func (vm *VendorManager) PerformNodeVendoring(hasTypeScript bool) error {
	blue := color.New(color.FgBlue)
	green := color.New(color.FgGreen)

	blue.Println("Installing JavaScript dependencies...")

	// Step 1: Pull Node image if needed
	if err := vm.pullNodeImage(); err != nil {
		return fmt.Errorf("failed to pull Node image: %v", err)
	}

	// Step 1.5: Backup existing node_modules if it exists
	if err := vm.backupExistingNodeModules(); err != nil {
		return fmt.Errorf("failed to backup existing node_modules: %v", err)
	}

	// Ensure backup is restored on any failure after this point
	defer func() {
		if r := recover(); r != nil {
			vm.restoreNodeModulesBackup()
			panic(r)
		}
	}()

	// Step 2: Create node_modules directory
	nodeModulesPath := filepath.Join(vm.projectPath, nodeModulesDir)
	if err := os.MkdirAll(nodeModulesPath, 0755); err != nil {
		vm.restoreNodeModulesBackup()
		return fmt.Errorf("failed to create node_modules directory: %v", err)
	}

	// Step 3: Run vendoring container
	containerID, err := vm.runNodeVendorContainer(hasTypeScript)
	if err != nil {
		vm.restoreNodeModulesBackup()
		return fmt.Errorf("failed to run Node vendoring container: %v", err)
	}
	defer vm.cleanupContainer(containerID)

	// Step 4: Verify vendored files
	if err := vm.verifyVendoredFiles(nodeModulesPath); err != nil {
		vm.restoreNodeModulesBackup()
		return fmt.Errorf("failed to verify vendored files: %v", err)
	}

	// Step 5: Cleanup backup on success
	if err := vm.cleanupNodeModulesBackup(); err != nil {
		// Log warning but don't fail - vendoring was successful
		yellow := color.New(color.FgYellow)
		yellow.Printf("⚠️ Warning: failed to cleanup node_modules backup: %v\n", err)
	}

	green.Println("v JavaScript dependencies installed")
	return nil
}

// pullPythonImage pulls the Python image if it doesn't exist locally
func (vm *VendorManager) pullPythonImage() error {
	ctx := context.Background()

	// Check if image exists locally
	images, err := vm.dockerClient.ImageList(ctx, image.ListOptions{})
	if err != nil {
		return err
	}

	imageExists := false
	for _, img := range images {
		for _, tag := range img.RepoTags {
			if tag == pythonImage {
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
	blue.Printf("Pulling Docker image: %s...\n", pythonImage)

	reader, err := vm.dockerClient.ImagePull(ctx, pythonImage, image.PullOptions{})
	if err != nil {
		return err
	}
	defer reader.Close()

	// Read pull output (but don't display it to keep output clean)
	io.Copy(io.Discard, reader)

	return nil
}

// pullNodeImage pulls the Node.js image if it doesn't exist locally
func (vm *VendorManager) pullNodeImage() error {
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

// runVendorContainer creates and runs a container for Python vendoring
func (vm *VendorManager) runVendorContainer() (string, error) {
	ctx := context.Background()

	// Generate unique container name
	containerName := fmt.Sprintf("%s%d", containerName, time.Now().Unix())

	// Prepare absolute paths for volume mounting
	vendorPath := filepath.Join(vm.projectPath, vendorDir)
	requirementsPath := filepath.Join(vm.projectPath, "requirements.txt")

	absVendorPath, err := filepath.Abs(vendorPath)
	if err != nil {
		return "", fmt.Errorf("failed to get absolute vendor path: %v", err)
	}

	absRequirementsPath, err := filepath.Abs(requirementsPath)
	if err != nil {
		return "", fmt.Errorf("failed to get absolute requirements path: %v", err)
	}

	config := &container.Config{
		Image: pythonImage,
		Cmd: []string{
			"sh", "-c",
			vm.getPythonInstallCommand(),
		},
		WorkingDir: "/app",
		Env:        getBuildEnvVars(),
	}

	hostConfig := &container.HostConfig{
		Binds: []string{
			fmt.Sprintf("%s:/vendor", absVendorPath),
			fmt.Sprintf("%s:/requirements.txt:ro", absRequirementsPath),
		},
		AutoRemove: false, // We'll remove manually after copying files
	}

	resp, err := vm.dockerClient.ContainerCreate(ctx, config, hostConfig, nil, nil, containerName)
	if err != nil {
		return "", err
	}

	if err := vm.dockerClient.ContainerStart(ctx, resp.ID, container.StartOptions{}); err != nil {
		return "", err
	}

	// Stream logs in real-time while container is running
	logReader, err := vm.dockerClient.ContainerLogs(ctx, resp.ID, container.LogsOptions{
		ShowStdout: true,
		ShowStderr: true,
		Follow:     true,
		Timestamps: false,
	})
	if err != nil {
		return resp.ID, fmt.Errorf("failed to get container logs: %v", err)
	}

	// Stream logs in a goroutine
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
				// Clean Docker log headers and display output
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
		// Wait for logs to finish streaming
		<-logDone

		if status.StatusCode != 0 {
			// Get full container logs for error details
			logs, _ := vm.getContainerLogs(resp.ID)
			red := color.New(color.FgRed)
			red.Printf("⚠️ Python dependency installation failed with exit code %d\n", status.StatusCode)
			if logs != "" {
				red.Printf("📋 Full error log:\n%s\n", logs)
			}
			return resp.ID, fmt.Errorf("pip install failed with exit code %d", status.StatusCode)
		}
	}

	return resp.ID, nil
}

// runNodeVendorContainer creates and runs a container for Node.js vendoring
func (vm *VendorManager) runNodeVendorContainer(hasTypeScript bool) (string, error) {
	ctx := context.Background()

	// Generate unique container name
	containerName := fmt.Sprintf("%snode-%d", containerName, time.Now().Unix())

	// Prepare absolute paths for volume mounting
	nodeModulesPath := filepath.Join(vm.projectPath, nodeModulesDir)
	packageJsonPath := filepath.Join(vm.projectPath, "package.json")

	absNodeModulesPath, err := filepath.Abs(nodeModulesPath)
	if err != nil {
		return "", fmt.Errorf("failed to get absolute node_modules path: %v", err)
	}

	absPackageJsonPath, err := filepath.Abs(packageJsonPath)
	if err != nil {
		return "", fmt.Errorf("failed to get absolute package.json path: %v", err)
	}

	// Build install command
	installCmd := vm.getNodeInstallCommand(hasTypeScript)

	config := &container.Config{
		Image: nodeImage,
		Cmd: []string{
			"sh", "-c",
			installCmd,
		},
		WorkingDir: "/app",
	}

	hostConfig := &container.HostConfig{
		Binds: []string{
			fmt.Sprintf("%s:/app/node_modules", absNodeModulesPath),
			fmt.Sprintf("%s:/app/package.json:ro", absPackageJsonPath),
		},
		AutoRemove: false, // We'll remove manually after copying files
	}

	resp, err := vm.dockerClient.ContainerCreate(ctx, config, hostConfig, nil, nil, containerName)
	if err != nil {
		return "", err
	}

	if err := vm.dockerClient.ContainerStart(ctx, resp.ID, container.StartOptions{}); err != nil {
		return "", err
	}

	// Stream logs in real-time while container is running
	logReader, err := vm.dockerClient.ContainerLogs(ctx, resp.ID, container.LogsOptions{
		ShowStdout: true,
		ShowStderr: true,
		Follow:     true,
		Timestamps: false,
	})
	if err != nil {
		return resp.ID, fmt.Errorf("failed to get container logs: %v", err)
	}

	// Stream logs in a goroutine
	logDone := make(chan struct{})
	go func() {
		defer close(logDone)
		defer logReader.Close()

		green := color.New(color.FgGreen)
		buffer := make([]byte, 1024)
		for {
			n, err := logReader.Read(buffer)
			if err != nil {
				if err != io.EOF {
					green.Printf("Log read error: %v\n", err)
				}
				break
			}

			if n > 0 {
				// Clean Docker log headers and display output
				output := vm.cleanDockerLogOutput(buffer[:n])
				if output != "" {
					green.Printf("  %s", output)
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
		// Wait for logs to finish streaming
		<-logDone

		if status.StatusCode != 0 {
			// Get full container logs for error details
			logs, _ := vm.getContainerLogs(resp.ID)
			red := color.New(color.FgRed)
			red.Printf("⚠️ JavaScript dependency installation failed with exit code %d\n", status.StatusCode)
			if logs != "" {
				red.Printf("📋 Full error log:\n%s\n", logs)
			}
			return resp.ID, fmt.Errorf("npm install failed with exit code %d", status.StatusCode)
		}
	}

	return resp.ID, nil
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

// getPythonInstallCommand returns the pip install command with proper ownership
func (vm *VendorManager) getPythonInstallCommand() string {
	// Get current user info for ownership
	currentUser, err := user.Current()
	var uid, gid string
	if err == nil {
		uid = currentUser.Uid
		gid = currentUser.Gid
	} else {
		// Fallback to common non-root user
		uid = "1000"
		gid = "1000"
	}

	// Install git if GITHUB_TOKEN is set (needed for private repo dependencies)
	// The python:3.11-slim image doesn't include git by default
	gitInstall := `if [ -n "$GITHUB_TOKEN" ]; then apt-get update && apt-get install -y --no-install-recommends git && rm -rf /var/lib/apt/lists/*; fi`

	// Configure git to use GITHUB_TOKEN for private repos if set
	// This rewrites git URLs to use the token for authentication
	// Handles multiple URL formats: git@github.com:, https://github.com/, ssh://git@github.com/
	gitConfig := `if [ -n "$GITHUB_TOKEN" ]; then git config --global url."https://${GITHUB_TOKEN}@github.com/".insteadOf "git@github.com:" && git config --global url."https://${GITHUB_TOKEN}@github.com/".insteadOf "https://github.com/" && git config --global url."https://${GITHUB_TOKEN}@github.com/".insteadOf "ssh://git@github.com/"; fi`

	// Configure pip to use extra index URL for private PyPI repos if set
	pipExtraIndex := `PIP_EXTRA_INDEX_FLAG=""; if [ -n "$PIP_EXTRA_INDEX_URL" ]; then PIP_EXTRA_INDEX_FLAG="--extra-index-url $PIP_EXTRA_INDEX_URL"; fi`

	// Always fix ownership even on failure, then exit with original status
	return fmt.Sprintf("%s && %s && %s && pip install --target /vendor -r /requirements.txt $PIP_EXTRA_INDEX_FLAG --no-cache-dir --disable-pip-version-check; STATUS=$?; chown -R %s:%s /vendor 2>/dev/null || true; exit $STATUS", gitInstall, gitConfig, pipExtraIndex, uid, gid)
}

// getNodeInstallCommand returns the npm install command with proper ownership
func (vm *VendorManager) getNodeInstallCommand(hasTypeScript bool) string {
	// Get current user info for ownership
	currentUser, err := user.Current()
	var uid, gid string
	if err == nil {
		uid = currentUser.Uid
		gid = currentUser.Gid
	} else {
		// Fallback to common non-root user
		uid = "1000"
		gid = "1000"
	}

	cmd := "npm install --production"
	if hasTypeScript {
		// Install all dependencies (including devDependencies for TypeScript compilation)
		// Then run the build script - this MUST succeed for TypeScript projects
		cmd = "npm install && npm run build"
	}

	// Always fix ownership even on failure, then exit with original status
	return fmt.Sprintf("%s; STATUS=$?; chown -R %s:%s /app/node_modules 2>/dev/null || true; exit $STATUS", cmd, uid, gid)
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
	green.Printf("✓ Successfully vendored %d packages (%s)\n", len(entries), sizeStr)

	// List key packages for verification
	blue.Println("📋 Vendored packages:")
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
		yellow.Printf("⚠️  Warning: Vendor directory is only %s - this may indicate incomplete installation\n", sizeStr)
	}

	return nil
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

// cleanupContainer removes the vendoring container
func (vm *VendorManager) cleanupContainer(containerID string) {
	ctx := context.Background()
	vm.dockerClient.ContainerRemove(ctx, containerID, container.RemoveOptions{Force: true})
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

// BackupExistingNodeModules creates a backup of existing node_modules directory (exported for testing)
func (vm *VendorManager) BackupExistingNodeModules() error {
	return vm.backupExistingNodeModules()
}

// backupExistingNodeModules creates a backup of existing node_modules directory
func (vm *VendorManager) backupExistingNodeModules() error {
	nodeModulesPath := filepath.Join(vm.projectPath, nodeModulesDir)
	backupPath := filepath.Join(vm.projectPath, nodeModulesBackupDir)

	// Check if node_modules exists
	if _, err := os.Stat(nodeModulesPath); os.IsNotExist(err) {
		return nil // No backup needed
	}

	// Remove any existing backup first
	if _, err := os.Stat(backupPath); err == nil {
		if err := os.RemoveAll(backupPath); err != nil {
			return fmt.Errorf("failed to remove existing backup: %v", err)
		}
	}

	// Rename node_modules to backup
	if err := os.Rename(nodeModulesPath, backupPath); err != nil {
		return fmt.Errorf("failed to backup node_modules: %v", err)
	}

	blue := color.New(color.FgBlue)
	blue.Println("📦 Backed up existing node_modules")
	return nil
}

// RestoreNodeModulesBackup restores node_modules from backup on failure (exported for testing)
func (vm *VendorManager) RestoreNodeModulesBackup() error {
	return vm.restoreNodeModulesBackup()
}

// restoreNodeModulesBackup restores node_modules from backup on failure
func (vm *VendorManager) restoreNodeModulesBackup() error {
	nodeModulesPath := filepath.Join(vm.projectPath, nodeModulesDir)
	backupPath := filepath.Join(vm.projectPath, nodeModulesBackupDir)

	// Check if backup exists
	if _, err := os.Stat(backupPath); os.IsNotExist(err) {
		return nil // No backup to restore
	}

	// Remove failed node_modules if it exists
	if _, err := os.Stat(nodeModulesPath); err == nil {
		if err := os.RemoveAll(nodeModulesPath); err != nil {
			return fmt.Errorf("failed to remove failed node_modules: %v", err)
		}
	}

	// Restore backup
	if err := os.Rename(backupPath, nodeModulesPath); err != nil {
		return fmt.Errorf("failed to restore node_modules backup: %v", err)
	}

	yellow := color.New(color.FgYellow)
	yellow.Println("🔄 Restored original node_modules")
	return nil
}

// CleanupNodeModulesBackup removes backup after successful vendoring (exported for testing)
func (vm *VendorManager) CleanupNodeModulesBackup() error {
	return vm.cleanupNodeModulesBackup()
}

// cleanupNodeModulesBackup removes backup after successful vendoring
func (vm *VendorManager) cleanupNodeModulesBackup() error {
	backupPath := filepath.Join(vm.projectPath, nodeModulesBackupDir)

	// Check if backup exists
	if _, err := os.Stat(backupPath); os.IsNotExist(err) {
		return nil // No backup to cleanup
	}

	// Remove backup
	if err := os.RemoveAll(backupPath); err != nil {
		return fmt.Errorf("failed to cleanup node_modules backup: %v", err)
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

// ShouldPerformVendoring checks if vendoring should be performed for the current project
func ShouldPerformVendoring(projectPath string) (bool, error) {
	// Check if requirements.txt exists
	requirementsPath := filepath.Join(projectPath, "requirements.txt")
	if _, err := os.Stat(requirementsPath); os.IsNotExist(err) {
		return false, nil
	}

	// Check if vendor directory already exists and is recent
	vendorPath := filepath.Join(projectPath, vendorDir)
	vendorInfo, err := os.Stat(vendorPath)
	requirementsInfo, err2 := os.Stat(requirementsPath)

	if err == nil && err2 == nil {
		// If vendor is newer than requirements.txt, skip vendoring
		if vendorInfo.ModTime().After(requirementsInfo.ModTime()) {
			return false, nil
		}
	}

	return true, nil
}

// ShouldPerformNodeVendoring checks if Node.js vendoring should be performed
func ShouldPerformNodeVendoring(projectPath string) (bool, error) {
	// Check if package.json exists
	packageJsonPath := filepath.Join(projectPath, "package.json")
	if _, err := os.Stat(packageJsonPath); os.IsNotExist(err) {
		return false, nil
	}

	// Check if node_modules directory already exists and is recent
	nodeModulesPath := filepath.Join(projectPath, nodeModulesDir)
	nodeModulesInfo, err := os.Stat(nodeModulesPath)
	packageJsonInfo, err2 := os.Stat(packageJsonPath)

	if err == nil && err2 == nil {
		// If node_modules is newer than package.json, skip vendoring
		if nodeModulesInfo.ModTime().After(packageJsonInfo.ModTime()) {
			return false, nil
		}
	}

	return true, nil
}

// PerformVendoringIfNeeded performs vendoring only if needed and Docker is available
func PerformVendoringIfNeeded(projectPath string) error {
	blue := color.New(color.FgBlue)
	red := color.New(color.FgRed)

	// Check if Python vendoring is needed
	shouldVendorPython, err := ShouldPerformVendoring(projectPath)
	if err != nil {
		return err
	}

	// Check if Node.js vendoring is needed
	shouldVendorNode, err := ShouldPerformNodeVendoring(projectPath)
	if err != nil {
		return err
	}

	if !shouldVendorPython && !shouldVendorNode {
		return nil
	}

	// Check Docker availability - fail hard if dependencies exist but Docker unavailable
	if err := CheckDockerInstalled(); err != nil {
		red.Printf("⚠️ Docker required for dependency installation: %v\n", err)
		red.Println("⚠️ Cannot deploy bot without installed dependencies")
		red.Println("💡 Solution: Install Docker and try again")
		return fmt.Errorf("docker unavailable but dependencies detected - installation required")
	}

	vm, err := NewVendorManager(projectPath)
	if err != nil {
		red.Printf("⚠️ Failed to initialize dependency installer: %v\n", err)
		red.Println("⚠️ Cannot deploy bot without installed dependencies")
		return fmt.Errorf("vendor manager initialization failed: %v", err)
	}
	defer vm.Close()

	if err := vm.CheckDockerRunning(); err != nil {
		red.Printf("⚠️ Docker daemon not running: %v\n", err)
		red.Println("⚠️ Cannot install dependencies without Docker")
		red.Println("💡 Solution: Start Docker daemon and try again")
		return fmt.Errorf("docker daemon not running but dependencies detected - installation required")
	}

	// Perform Python vendoring if needed
	if shouldVendorPython {
		blue.Println("Python dependencies detected")
		if err := vm.PerformVendoring(); err != nil {
			red.Printf("⚠️ Python dependency installation failed: %v\n", err)
			red.Println("⚠️ Cannot deploy bot without successfully compiled dependencies")
			return fmt.Errorf("python vendoring failed: %v", err)
		}
	}

	// Perform Node.js vendoring if needed
	if shouldVendorNode {
		blue.Println("JavaScript dependencies detected")
		hasTypeScript := vm.CheckTypeScriptFiles()
		if err := vm.PerformNodeVendoring(hasTypeScript); err != nil {
			red.Printf("⚠️ JavaScript dependency installation failed: %v\n", err)
			red.Println("⚠️ Cannot deploy bot without successfully compiled dependencies")
			return fmt.Errorf("node vendoring failed: %v", err)
		}
	}

	return nil
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

// CheckFrontendExists checks if a frontend directory with package.json exists
func (vm *VendorManager) CheckFrontendExists() bool {
	frontendPackageJson := filepath.Join(vm.projectPath, "frontend", "package.json")
	_, err := os.Stat(frontendPackageJson)
	return err == nil
}

// ShouldBuildFrontend checks if frontend needs to be built
func ShouldBuildFrontend(projectPath string) (bool, error) {
	frontendPath := filepath.Join(projectPath, "frontend")
	packageJsonPath := filepath.Join(frontendPath, "package.json")

	// Check if frontend/package.json exists
	if _, err := os.Stat(packageJsonPath); os.IsNotExist(err) {
		return false, nil
	}

	// Check if bundle already exists
	bundlePath := filepath.Join(frontendPath, "dist", "bundle.js")
	bundleInfo, err := os.Stat(bundlePath)
	if os.IsNotExist(err) {
		// Bundle doesn't exist, need to build
		return true, nil
	}
	if err != nil {
		return false, err
	}

	// Check if any source files are newer than bundle
	packageJsonInfo, _ := os.Stat(packageJsonPath)
	if packageJsonInfo.ModTime().After(bundleInfo.ModTime()) {
		return true, nil
	}

	// Check index.tsx modification time
	indexTsxPath := filepath.Join(frontendPath, "index.tsx")
	if indexInfo, err := os.Stat(indexTsxPath); err == nil {
		if indexInfo.ModTime().After(bundleInfo.ModTime()) {
			return true, nil
		}
	}

	// Bundle is up to date
	return false, nil
}

// BuildFrontend builds the frontend bundle in a Docker container
func (vm *VendorManager) BuildFrontend() error {
	blue := color.New(color.FgBlue)
	green := color.New(color.FgGreen)

	blue.Println("Building frontend bundle...")

	// Step 1: Pull Node image if needed
	if err := vm.pullNodeImage(); err != nil {
		return fmt.Errorf("failed to pull Node image: %v", err)
	}

	// Step 2: Run frontend build container
	containerID, err := vm.runFrontendBuildContainer()
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

	green.Printf("✓ Frontend bundle built (%s)\n", sizeStr)
	return nil
}

// runFrontendBuildContainer creates and runs a container for frontend building
func (vm *VendorManager) runFrontendBuildContainer() (string, error) {
	ctx := context.Background()

	// Generate unique container name
	containerName := fmt.Sprintf("%sfrontend-%d", containerName, time.Now().Unix())

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
func BuildFrontendIfNeeded(projectPath string) error {
	blue := color.New(color.FgBlue)
	red := color.New(color.FgRed)

	shouldBuild, err := ShouldBuildFrontend(projectPath)
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

	if err := vm.BuildFrontend(); err != nil {
		return fmt.Errorf("frontend build failed: %v", err)
	}

	return nil
}
