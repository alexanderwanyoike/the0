// Package docker provides the ContainerBuilder component.
//
// ContainerBuilder uses the fluent builder pattern to construct Docker container
// configurations. It provides a clean API for setting up environment variables,
// volume mounts, resource limits, and labels without dealing with low-level
// Docker API structures directly.
package docker

import (
	"encoding/json"
	"fmt"
	"runtime/internal/model"

	"github.com/docker/docker/api/types/container"
)

// ContainerBuilder provides a fluent interface for building Docker container configurations.
type ContainerBuilder struct {
	config     *container.Config
	hostConfig *container.HostConfig
}

// NewContainerBuilder creates a new builder initialized with sensible defaults.
func NewContainerBuilder(imageName string) *ContainerBuilder {
	return &ContainerBuilder{
		config: &container.Config{
			Image:      imageName,
			StopSignal: "SIGTERM",
			WorkingDir: "/tmp",
			Labels:     make(map[string]string),
			Env:        []string{"PYTHONDONTWRITEBYTECODE=1"}, // Prevent Python from creating __pycache__ directories
		},
		hostConfig: &container.HostConfig{
			NetworkMode: "host",
		},
	}
}

// WithCommand sets the container's command (shell and script path).
func (b *ContainerBuilder) WithCommand(shell, scriptPath string) *ContainerBuilder {
	b.config.Cmd = []string{shell, scriptPath}
	return b
}

// WithBinds adds volume mounts to the container.
func (b *ContainerBuilder) WithBinds(binds ...string) *ContainerBuilder {
	b.hostConfig.Binds = append(b.hostConfig.Binds, binds...)
	return b
}

// WithEnv adds environment variables to the container.
func (b *ContainerBuilder) WithEnv(envVars ...string) *ContainerBuilder {
	b.config.Env = append(b.config.Env, envVars...)
	return b
}

// WithResources sets memory and CPU limits for the container.
func (b *ContainerBuilder) WithResources(memoryLimit int64, cpuShares int64) *ContainerBuilder {
	b.hostConfig.Resources = container.Resources{
		Memory:    memoryLimit,
		CPUShares: cpuShares,
	}
	return b
}

// WithAutoRemove sets whether the container should be automatically removed on exit.
func (b *ContainerBuilder) WithAutoRemove(autoRemove bool) *ContainerBuilder {
	b.hostConfig.AutoRemove = autoRemove
	return b
}

// WithExecutable configures the container from an Executable, setting environment
// variables, labels, and extracting the entrypoint file.
// If config marshaling fails, an empty JSON object is used as fallback.
func (b *ContainerBuilder) WithExecutable(executable model.Executable) *ContainerBuilder {
	// Extract entrypoint file from executable
	entrypointFile := executable.EntrypointFiles[executable.Entrypoint]

	configJSON, err := json.Marshal(executable.Config)
	if err != nil {
		// Fallback to empty config if marshaling fails
		configJSON = []byte("{}")
	}
	b.config.Env = append(b.config.Env,
		fmt.Sprintf("ID=%s", executable.ID),
		fmt.Sprintf("CONFIG=%s", string(configJSON)),
		fmt.Sprintf("ENTRYPOINT_TYPE=%s", executable.Entrypoint),
		fmt.Sprintf("CODE_MOUNT_DIR=%s", executable.Entrypoint),
		// STATE_DIR points to the SDK-managed state directory inside /state
		// Users can also write directly to /state for custom persistent data
		"STATE_DIR=/state/.the0-state",
	)

	b.config.Labels["runtime.id"] = executable.ID
	b.config.Labels["runtime.entrypoint"] = entrypointFile
	b.config.Labels["runtime.managed"] = "true"
	b.config.Labels["runtime.segment"] = fmt.Sprintf("%d", executable.Segment)

	return b
}

// Build returns the finalized container.Config and container.HostConfig.
func (b *ContainerBuilder) Build() (*container.Config, *container.HostConfig) {
	return b.config, b.hostConfig
}
