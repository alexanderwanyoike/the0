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
			// Default to bridge mode; use WithNetwork to join a specific network
			NetworkMode: "bridge",
		},
	}
}

// WithNetwork sets the Docker network for the container.
// Use this to connect bot containers to the same network as infrastructure services.
func (b *ContainerBuilder) WithNetwork(networkName string) *ContainerBuilder {
	if networkName != "" {
		b.hostConfig.NetworkMode = container.NetworkMode(networkName)
	}
	return b
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

// WithMinIOConfig adds MinIO credentials as environment variables.
// Required for daemon init/sync to access MinIO from within the container.
func (b *ContainerBuilder) WithMinIOConfig(endpoint, accessKey, secretKey string, useSSL bool) *ContainerBuilder {
	b.config.Env = append(b.config.Env,
		fmt.Sprintf("MINIO_ENDPOINT=%s", endpoint),
		fmt.Sprintf("MINIO_ACCESS_KEY=%s", accessKey),
		fmt.Sprintf("MINIO_SECRET_KEY=%s", secretKey),
		fmt.Sprintf("MINIO_USE_SSL=%t", useSSL),
	)
	return b
}

// WithDaemonConfig adds bot-specific config for daemon init/sync.
// This enables the daemon subprocess approach where the container handles
// its own code download, state sync, and log collection.
func (b *ContainerBuilder) WithDaemonConfig(botID, codeFile, runtime, entrypoint, config string, isScheduled bool) *ContainerBuilder {
	b.config.Env = append(b.config.Env,
		fmt.Sprintf("BOT_ID=%s", botID),
		fmt.Sprintf("CODE_FILE=%s", codeFile),
		fmt.Sprintf("RUNTIME=%s", runtime),
		fmt.Sprintf("ENTRYPOINT=%s", entrypoint),
		fmt.Sprintf("BOT_CONFIG=%s", config),
		fmt.Sprintf("IS_SCHEDULED=%t", isScheduled),
	)
	return b
}

// WithDevRuntime mounts the runtime binary from the host for development.
// Allows testing daemon changes without rebuilding images.
func (b *ContainerBuilder) WithDevRuntime(hostPath string) *ContainerBuilder {
	if hostPath != "" {
		b.hostConfig.Binds = append(b.hostConfig.Binds,
			fmt.Sprintf("%s:/app/runtime:ro", hostPath))
	}
	return b
}

// WithQueryConfig configures the container for query execution mode.
// Sets QUERY_PATH and QUERY_PARAMS environment variables that the SDK
// uses to detect query mode and execute the appropriate handler.
func (b *ContainerBuilder) WithQueryConfig(queryPath string, queryParams string) *ContainerBuilder {
	if queryPath != "" {
		b.config.Env = append(b.config.Env,
			fmt.Sprintf("QUERY_PATH=%s", queryPath),
		)
		if queryParams != "" {
			b.config.Env = append(b.config.Env,
				fmt.Sprintf("QUERY_PARAMS=%s", queryParams),
			)
		}
	}
	return b
}

// WithExtraHosts adds extra host-to-IP mappings to the container.
// Use "host.docker.internal:host-gateway" to allow containers to reach
// the Docker host on Linux systems.
func (b *ContainerBuilder) WithExtraHosts(hosts ...string) *ContainerBuilder {
	b.hostConfig.ExtraHosts = append(b.hostConfig.ExtraHosts, hosts...)
	return b
}

// Build returns the finalized container.Config and container.HostConfig.
func (b *ContainerBuilder) Build() (*container.Config, *container.HostConfig) {
	return b.config, b.hostConfig
}
