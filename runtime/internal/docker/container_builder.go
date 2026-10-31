// Package docker provides the ContainerBuilder component.
//
// ContainerBuilder uses the fluent builder pattern to construct Docker container
// configurations. It provides a clean API for setting up environment variables,
// volume mounts, resource limits, and labels without dealing with low-level
// Docker API structures directly.
package docker

import (
	"fmt"
	"runtime/internal/launch"
	"runtime/internal/model"

	"github.com/docker/docker/api/types/container"
	"github.com/docker/docker/api/types/strslice"
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
			User:       "1000:1000",
			StopSignal: "SIGTERM",
			WorkingDir: "/tmp",
			Labels:     make(map[string]string),
			Env: []string{
				"PYTHONDONTWRITEBYTECODE=1", // Prevent Python from creating __pycache__ directories
				// Python block-buffers stdout (8KB) when it's a pipe, so metric()
				// print() output would sit unflushed instead of reaching the log
				// pipeline. Harmless for non-Python runtimes.
				"PYTHONUNBUFFERED=1",
			},
		},
		hostConfig: &container.HostConfig{
			// Default to bridge mode; use WithNetwork to join a specific network
			NetworkMode:    "bridge",
			ReadonlyRootfs: true,
			CapDrop:        strslice.StrSlice{"ALL"},
			SecurityOpt:    []string{"no-new-privileges:true"},
			Tmpfs: map[string]string{
				"/bot":      "rw,exec,nosuid,nodev,uid=1000,gid=1000",
				"/state":    "rw,nosuid,nodev,uid=1000,gid=1000",
				"/query":    "rw,nosuid,nodev,uid=1000,gid=1000",
				"/tmp":      "rw,nosuid,nodev,uid=1000,gid=1000,mode=1777",
				"/var/the0": "rw,nosuid,nodev,uid=1000,gid=1000",
			},
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

// WithExecutable labels the container with the executable it runs.
func (b *ContainerBuilder) WithExecutable(executable model.Executable) *ContainerBuilder {
	entrypointFile := executable.EntrypointFiles[executable.Entrypoint]

	b.config.Labels["runtime.id"] = executable.ID
	b.config.Labels["runtime.entrypoint"] = entrypointFile
	b.config.Labels["runtime.managed"] = "true"
	// Every terminating container, a query's included, is typed scheduled:
	// the schedule service's orphan sweep removes the ones nobody collected.
	containerType := "scheduled"
	if executable.IsLongRunning {
		containerType = "realtime"
	}
	b.config.Labels["runtime.type"] = containerType

	return b
}

// WithLaunch makes the container start the launch: runtime execute with the
// launch's command and environment.
func (b *ContainerBuilder) WithLaunch(settings launch.Settings) *ContainerBuilder {
	b.config.Cmd = settings.Command()
	for _, variable := range settings.Env() {
		b.config.Env = append(b.config.Env, variable.Name+"="+variable.Value)
	}
	return b
}

// WithDevRuntime mounts the runtime binary from the host for development.
// Allows testing daemon changes without rebuilding images.
func (b *ContainerBuilder) WithDevRuntime(hostPath string) *ContainerBuilder {
	if hostPath != "" {
		b.hostConfig.Binds = append(b.hostConfig.Binds,
			fmt.Sprintf("%s:%s:ro", hostPath, launch.Binary))
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
