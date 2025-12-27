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

const (
	nodeImage            = "node:20-slim"
	nodeModulesDir       = "node_modules"
	nodeModulesBackupDir = "_node_modules_backup"
)

// NodeVendor handles Node.js dependency installation via npm
type NodeVendor struct{}

// init registers the Node.js vendor
func init() {
	RegisterVendor(&NodeVendor{})
}

// Name returns the vendor name
func (v *NodeVendor) Name() string {
	return "JavaScript"
}

// DockerImage returns the Docker image used for vendoring
func (v *NodeVendor) DockerImage() string {
	return nodeImage
}

// Detect checks if package.json exists (but not in frontend directory)
func (v *NodeVendor) Detect(projectPath string) bool {
	packageJsonPath := filepath.Join(projectPath, "package.json")
	_, err := os.Stat(packageJsonPath)
	return err == nil
}

// ShouldVendor checks if Node.js vendoring should be performed
func (v *NodeVendor) ShouldVendor(projectPath string) (bool, error) {
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

// Vendor performs the Docker-based Node.js dependency installation
func (v *NodeVendor) Vendor(vm *VendorManager) error {
	blue := color.New(color.FgBlue)
	green := color.New(color.FgGreen)

	blue.Println("Installing JavaScript dependencies...")

	// Check for TypeScript files
	hasTypeScript := v.hasTypeScriptFiles(vm.projectPath)

	// Step 1: Pull Node image if needed
	if err := v.pullImage(vm); err != nil {
		return fmt.Errorf("failed to pull Node image: %v", err)
	}

	// Step 1.5: Backup existing node_modules if it exists
	if err := v.backupExistingNodeModules(vm.projectPath); err != nil {
		return fmt.Errorf("failed to backup existing node_modules: %v", err)
	}

	// Ensure backup is restored on any failure after this point
	defer func() {
		if r := recover(); r != nil {
			v.restoreNodeModulesBackup(vm.projectPath)
			panic(r)
		}
	}()

	// Step 2: Create node_modules directory
	nodeModulesPath := filepath.Join(vm.projectPath, nodeModulesDir)
	if err := os.MkdirAll(nodeModulesPath, 0755); err != nil {
		v.restoreNodeModulesBackup(vm.projectPath)
		return fmt.Errorf("failed to create node_modules directory: %v", err)
	}

	// Step 3: Run vendoring container
	containerID, err := v.runContainer(vm, hasTypeScript)
	if err != nil {
		v.restoreNodeModulesBackup(vm.projectPath)
		return fmt.Errorf("failed to run Node vendoring container: %v", err)
	}
	defer vm.cleanupContainer(containerID)

	// Step 4: Verify vendored files
	if err := vm.verifyVendoredFiles(nodeModulesPath); err != nil {
		v.restoreNodeModulesBackup(vm.projectPath)
		return fmt.Errorf("failed to verify vendored files: %v", err)
	}

	// Step 5: Cleanup backup on success
	if err := v.cleanupNodeModulesBackup(vm.projectPath); err != nil {
		// Log warning but don't fail - vendoring was successful
		yellow := color.New(color.FgYellow)
		yellow.Printf("Warning: failed to cleanup node_modules backup: %v\n", err)
	}

	green.Println("v JavaScript dependencies installed")
	return nil
}

// hasTypeScriptFiles checks if TypeScript files exist in the project
func (v *NodeVendor) hasTypeScriptFiles(projectPath string) bool {
	files, err := filepath.Glob(filepath.Join(projectPath, "*.ts"))
	if err != nil {
		return false
	}
	return len(files) > 0
}

// pullImage pulls the Node.js image if it doesn't exist locally
func (v *NodeVendor) pullImage(vm *VendorManager) error {
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

// runContainer creates and runs a container for Node.js vendoring
func (v *NodeVendor) runContainer(vm *VendorManager, hasTypeScript bool) (string, error) {
	ctx := context.Background()

	// Generate unique container name
	containerName := fmt.Sprintf("%snode-%d", vendorContainerName, time.Now().Unix())

	// Get absolute project path for mounting
	absProjectPath, err := filepath.Abs(vm.projectPath)
	if err != nil {
		return "", fmt.Errorf("failed to get absolute project path: %v", err)
	}

	// Build install command
	installCmd := v.getInstallCommand(hasTypeScript)

	config := &container.Config{
		Image: nodeImage,
		Cmd: []string{
			"sh", "-c",
			installCmd,
		},
		WorkingDir: "/app",
		User:       v.getUserConfig(),
		Env:        getNodeBuildEnvVars(),
	}

	// Mount entire project directory so builds have access to source files
	hostConfig := &container.HostConfig{
		Binds: []string{
			fmt.Sprintf("%s:/app", absProjectPath),
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
			red.Printf("JavaScript dependency installation failed with exit code %d\n", status.StatusCode)
			if logs != "" {
				red.Printf("Full error log:\n%s\n", logs)
			}
			return resp.ID, fmt.Errorf("npm install failed with exit code %d", status.StatusCode)
		}
	}

	return resp.ID, nil
}

// getInstallCommand returns the npm install command
func (v *NodeVendor) getInstallCommand(hasTypeScript bool) string {
	if hasTypeScript {
		// Install all dependencies (including devDependencies for TypeScript compilation)
		// Then run the build script - this MUST succeed for TypeScript projects
		return "npm install && npm run build"
	}
	return "npm install --production"
}

// getUserConfig returns uid:gid string for container User config
func (v *NodeVendor) getUserConfig() string {
	currentUser, err := user.Current()
	if err == nil {
		return fmt.Sprintf("%s:%s", currentUser.Uid, currentUser.Gid)
	}
	return "1000:1000"
}

// backupExistingNodeModules creates a backup of existing node_modules directory
func (v *NodeVendor) backupExistingNodeModules(projectPath string) error {
	nodeModulesPath := filepath.Join(projectPath, nodeModulesDir)
	backupPath := filepath.Join(projectPath, nodeModulesBackupDir)

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
	blue.Println("Backed up existing node_modules")
	return nil
}

// restoreNodeModulesBackup restores node_modules from backup on failure
func (v *NodeVendor) restoreNodeModulesBackup(projectPath string) error {
	nodeModulesPath := filepath.Join(projectPath, nodeModulesDir)
	backupPath := filepath.Join(projectPath, nodeModulesBackupDir)

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
	yellow.Println("Restored original node_modules")
	return nil
}

// cleanupNodeModulesBackup removes backup after successful vendoring
func (v *NodeVendor) cleanupNodeModulesBackup(projectPath string) error {
	backupPath := filepath.Join(projectPath, nodeModulesBackupDir)

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

// getNodeBuildEnvVars returns environment variables for Node.js builds.
func getNodeBuildEnvVars() []string {
	secrets, err := LoadBuildSecrets()
	if err != nil || secrets == nil {
		return nil
	}

	var envVars []string

	// Pass NPM token for private registries (referenced in .npmrc as ${NPM_TOKEN})
	if secrets.NpmToken != "" {
		envVars = append(envVars, fmt.Sprintf("NPM_TOKEN=%s", secrets.NpmToken))
	}

	// Pass GitHub token for git-based dependencies
	if secrets.GitHubToken != "" {
		envVars = append(envVars, fmt.Sprintf("GITHUB_TOKEN=%s", secrets.GitHubToken))
	}

	return envVars
}
