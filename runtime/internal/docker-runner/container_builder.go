/**
 * Container Builder
 *
 * Constructs the container configurations needed to run a container
 * - The container config
 * - Host config
 */
package dockerrunner

import (
	"encoding/json"
	"fmt"
	"runtime/internal/model"

	"github.com/docker/docker/api/types/container"
)

type ContainerBuilder struct {
	config     *container.Config
	hostConfig *container.HostConfig
}

func NewContainerBuilder(
	imageName string,
) *ContainerBuilder {
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

func (b *ContainerBuilder) WithCommand(shell, scriptPath string) *ContainerBuilder {
	b.config.Cmd = []string{shell, scriptPath}
	return b
}

func (b *ContainerBuilder) WithBinds(binds ...string) *ContainerBuilder {
	b.hostConfig.Binds = append(b.hostConfig.Binds, binds...)
	return b
}

func (b *ContainerBuilder) WithEnv(envVars ...string) *ContainerBuilder {
	b.config.Env = append(b.config.Env, envVars...)
	return b
}

func (b *ContainerBuilder) WithResources(memoryLimit int64, cpuShares int64) *ContainerBuilder {
	b.hostConfig.Resources = container.Resources{
		Memory:    memoryLimit,
		CPUShares: cpuShares,
	}
	return b
}

func (b *ContainerBuilder) WithAutoRemove(autoRemove bool) *ContainerBuilder {
	b.hostConfig.AutoRemove = autoRemove
	return b
}

func (b *ContainerBuilder) WithExecutable(executable model.Executable) *ContainerBuilder {
	// Extract entrypoint file from executable
	entrypointFile := executable.EntrypointFiles[executable.Entrypoint]

	configJSON, _ := json.Marshal(executable.Config)
	b.config.Env = append(b.config.Env,
		fmt.Sprintf("ID=%s", executable.ID),
		fmt.Sprintf("CONFIG=%s", string(configJSON)),
		fmt.Sprintf("ENTRYPPOINT_TYPE=%s", executable.Entrypoint),
		fmt.Sprintf("CODE_MOUNT_DIR=%s", executable.Entrypoint),
	)

	b.config.Labels["runtime.id"] = executable.ID
	b.config.Labels["runtime.entrypoint"] = entrypointFile
	b.config.Labels["runtime.managed"] = "true"
	b.config.Labels["runtime.segment"] = fmt.Sprintf("%d", executable.Segment)

	return b
}

func (b *ContainerBuilder) Build() (*container.Config, *container.HostConfig) {
	return b.config, b.hostConfig
}
