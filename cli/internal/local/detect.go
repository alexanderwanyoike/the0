package local

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

// CheckDockerInstalled verifies that docker is installed
func CheckDockerInstalled() error {
	cmd := exec.Command("docker", "--version")
	output, err := cmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("docker is not installed: %w\nInstall Docker from https://docs.docker.com/get-docker/", err)
	}
	if !strings.Contains(string(output), "Docker") {
		return fmt.Errorf("unexpected docker version output: %s", string(output))
	}
	return nil
}

// CheckDockerRunning verifies that the docker daemon is running
func CheckDockerRunning() error {
	cmd := exec.Command("docker", "info")
	cmd.Stdout = nil
	cmd.Stderr = nil
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("docker daemon is not running: %w\nStart Docker Desktop or run 'sudo systemctl start docker'", err)
	}
	return nil
}

// CheckComposeAvailable verifies that docker compose is available
func CheckComposeAvailable() error {
	cmd := exec.Command("docker", "compose", "version")
	output, err := cmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("docker compose is not available: %w\nInstall Docker Compose v2: https://docs.docker.com/compose/install/", err)
	}
	if !strings.Contains(string(output), "Docker Compose") {
		return fmt.Errorf("unexpected docker compose output: %s", string(output))
	}
	return nil
}

// CheckAllPrerequisites runs all docker checks and returns the first error
func CheckAllPrerequisites() error {
	if err := CheckDockerInstalled(); err != nil {
		return err
	}
	if err := CheckDockerRunning(); err != nil {
		return err
	}
	if err := CheckComposeAvailable(); err != nil {
		return err
	}
	return nil
}

// ValidateRepoPath checks that the given path looks like a the0 repo
func ValidateRepoPath(path string) error {
	absPath, err := filepath.Abs(path)
	if err != nil {
		return fmt.Errorf("failed to resolve path: %w", err)
	}

	info, err := os.Stat(absPath)
	if err != nil {
		return fmt.Errorf("path does not exist: %s", absPath)
	}
	if !info.IsDir() {
		return fmt.Errorf("path is not a directory: %s", absPath)
	}

	requiredDirs := []string{"api", "frontend", "runtime", "docker"}
	missing := []string{}
	for _, dir := range requiredDirs {
		dirPath := filepath.Join(absPath, dir)
		if _, err := os.Stat(dirPath); os.IsNotExist(err) {
			missing = append(missing, dir)
		}
	}

	if len(missing) > 0 {
		return fmt.Errorf("not a valid the0 repository (missing: %s): %s", strings.Join(missing, ", "), absPath)
	}

	return nil
}
