package local

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"the0/internal/logger"
)

// ComposeRunner wraps docker compose commands
type ComposeRunner struct {
	ComposeDir string
	State      *LocalState
}

// NewComposeRunner creates a runner from the saved state
func NewComposeRunner() (*ComposeRunner, error) {
	state, err := EnsureInitialized()
	if err != nil {
		return nil, err
	}

	dir, err := ComposeDir()
	if err != nil {
		return nil, err
	}

	return &ComposeRunner{
		ComposeDir: dir,
		State:      state,
	}, nil
}

// baseArgs returns the common docker compose arguments
func (r *ComposeRunner) baseArgs() []string {
	return []string{
		"compose",
		"-f", filepath.Join(r.ComposeDir, "docker-compose.yml"),
		"-p", "the0",
	}
}

// devArgs returns docker compose arguments including the dev overlay
func (r *ComposeRunner) devArgs() []string {
	return []string{
		"compose",
		"-f", filepath.Join(r.ComposeDir, "docker-compose.yml"),
		"-f", filepath.Join(r.ComposeDir, "docker-compose.dev.yml"),
		"-p", "the0",
	}
}

// Run executes a docker compose command with stdout/stderr streaming
func (r *ComposeRunner) Run(args ...string) error {
	cmdArgs := append(r.baseArgs(), args...)
	logger.Verbose("Running: docker %s", strings.Join(cmdArgs, " "))

	cmd := exec.Command("docker", cmdArgs...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	cmd.Dir = r.ComposeDir

	return cmd.Run()
}

// RunCapture executes a docker compose command and captures output
func (r *ComposeRunner) RunCapture(args ...string) (string, error) {
	cmdArgs := append(r.baseArgs(), args...)
	logger.Verbose("Running: docker %s", strings.Join(cmdArgs, " "))

	cmd := exec.Command("docker", cmdArgs...)
	cmd.Dir = r.ComposeDir

	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr

	if err := cmd.Run(); err != nil {
		return "", fmt.Errorf("%w: %s", err, stderr.String())
	}

	return stdout.String(), nil
}

// RunDev executes a docker compose command with the dev overlay
func (r *ComposeRunner) RunDev(args ...string) error {
	cmdArgs := append(r.devArgs(), args...)
	logger.Verbose("Running: docker %s", strings.Join(cmdArgs, " "))

	cmd := exec.Command("docker", cmdArgs...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	cmd.Dir = r.ComposeDir

	return cmd.Run()
}

// Up starts all services in detached mode and waits for health checks
func (r *ComposeRunner) Up() error {
	return r.Run("up", "-d", "--wait")
}

// Down stops and removes all containers
func (r *ComposeRunner) Down() error {
	return r.Run("down")
}

// DownWithVolumes stops containers and removes volumes
func (r *ComposeRunner) DownWithVolumes() error {
	return r.Run("down", "-v")
}

// Build builds all service images
func (r *ComposeRunner) Build() error {
	return r.Run("build")
}

// BuildRuntimeImage builds the runtime Docker image from the repo
func (r *ComposeRunner) BuildRuntimeImage(repoPath string) error {
	runtimePath := filepath.Join(repoPath, "runtime")
	logger.Verbose("Building runtime image from %s", runtimePath)

	cmd := exec.Command("docker", "build", "-t", "the0/runtime:latest", runtimePath)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	return cmd.Run()
}

// Logs streams logs from a service (or all services if service is empty)
func (r *ComposeRunner) Logs(service string, follow bool, tail string) error {
	args := []string{"logs"}
	if follow {
		args = append(args, "-f")
	}
	if tail != "" {
		args = append(args, "--tail", tail)
	}
	if service != "" {
		args = append(args, service)
	}

	return r.Run(args...)
}

// PS returns docker compose ps output
func (r *ComposeRunner) PS() (string, error) {
	return r.RunCapture("ps", "--format", "json")
}

// UpDev starts frontend and API with the dev overlay (hot reload)
func (r *ComposeRunner) UpDev() error {
	return r.RunDev("up", "-d", "the0-frontend", "the0-api")
}
