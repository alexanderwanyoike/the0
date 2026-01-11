// Package docker provides a Docker-based execution environment for bots and backtests.
// It orchestrates the entire lifecycle from code download to container execution and log collection.
package docker

import (
	"context"
	"encoding/json"
	"fmt"
	"runtime/internal/model"
	runtimepkg "runtime/internal/runtime"
	"runtime/internal/util"
	"sync"
	"time"

	"github.com/docker/docker/api/types/container"
	"github.com/docker/docker/api/types/filters"
	"github.com/docker/docker/client"
)

// DockerRunner is the main interface for managing Docker-based bot/backtest execution.
// It supports both long-running containers (bots) and short-lived containers (backtests).
type DockerRunner interface {
	// StartContainer starts a container based on the executable configuration.
	// For long-running executables (IsLongRunning=true), returns immediately with container ID.
	// For terminating executables (IsLongRunning=false), waits for completion and returns results.
	StartContainer(
		ctx context.Context,
		executable model.Executable,
	) (*ExecutionResult, error)

	// StopContainer gracefully stops a running container and cleans up its resources.
	StopContainer(
		ctx context.Context,
		containerID string,
		executable model.Executable,
	) error

	// GetContainerStatus returns the current status of a container.
	GetContainerStatus(ctx context.Context, containerID string) (*ContainerStatus, error)

	// ListManagedContainers returns all running containers managed by this runner.
	ListManagedContainers(ctx context.Context) ([]*ContainerInfo, error)

	// ListAllManagedContainers returns ALL containers (including exited/crashed).
	ListAllManagedContainers(ctx context.Context) ([]*ContainerInfo, error)

	// HandleCrashedContainer captures logs from a crashed container and cleans it up.
	// Returns the crash logs that were captured.
	HandleCrashedContainer(ctx context.Context, containerInfo *ContainerInfo) (string, error)

	// GetContainerLogs retrieves the last N lines of logs from a container.
	GetContainerLogs(ctx context.Context, containerID string, tail int) (string, error)

	// GetContainerIP returns the IP address of a running container.
	GetContainerIP(ctx context.Context, containerID string) (string, error)

	// Close shuts down the runner and cleans up all managed resources.
	Close() error
}

// ExecutionResult contains the outcome of container execution.
type ExecutionResult struct {
	Status   string                 `json:"status"`  // "running", "success", or "error"
	Message  string                 `json:"message"` // Human-readable message
	Output   string                 `json:"output"`  // Container stdout/stderr
	Error    string                 `json:"error,omitempty"`
	ExitCode int                    `json:"exit_code"` // Container exit code
	Duration time.Duration          `json:"duration"`  // Execution duration
	Metadata map[string]interface{} `json:"metadata,omitempty"`

	// Long-running container fields
	ContainerID        string `json:"container_id,omitempty"`         // Docker container ID
	IsLongRunning      bool   `json:"is_long_running,omitempty"`      // True for bots, false for backtests
	ResultFileContents []byte `json:"result_file_contents,omitempty"` // Backtest result.json contents
}

// ContainerInfo represents information about a managed container.
type ContainerInfo struct {
	ContainerID string            `json:"container_id"`          // Docker container ID
	ID          string            `json:"id"`                    // Bot/backtest ID
	Entrypoint  string            `json:"entrypoint"`            // Entrypoint script filename (e.g., "main.py")
	Status      string            `json:"status"`                // Container status
	ExitCode    int               `json:"exit_code"`             // Exit code (for crashed containers)
	Error       string            `json:"error,omitempty"`       // Error message from container
	StartedAt   string            `json:"started_at"`            // RFC3339 timestamp
	FinishedAt  string            `json:"finished_at,omitempty"` // RFC3339 timestamp (for exited containers)
	Labels      map[string]string `json:"labels,omitempty"`
}

// dockerRunner is the concrete implementation of DockerRunner interface.
// Uses the daemon subprocess approach where containers handle their own
// code download, state sync, and log collection via /app/runtime daemon.
type dockerRunner struct {
	// Component dependencies
	orchestrator ContainerOrchestrator // Handles Docker operations

	// Configuration
	config            *DockerRunnerConfig
	logger            util.Logger
	managedContainers map[string]*ContainerInfo // Tracks active containers
	containersMutex   sync.RWMutex              // Protects managedContainers map
}

// DockerRunnerOptions contains configuration options for creating a DockerRunner.
type DockerRunnerOptions struct {
	Logger       util.Logger   // Optional logger, defaults to util.DefaultLogger if nil
	ConfigLoader ConfigLoader  // Optional config loader, defaults to EnvConfigLoader if nil
	Config       *DockerRunnerConfig // Optional pre-loaded config, takes precedence over ConfigLoader
}

// NewDockerRunner creates a new DockerRunner instance.
// Uses the daemon subprocess approach - containers handle their own code/state/logs.
func NewDockerRunner(options DockerRunnerOptions) (*dockerRunner, error) {
	// Create Docker client
	dockerClient, err := client.NewClientWithOpts(client.FromEnv, client.WithAPIVersionNegotiation())
	if err != nil {
		return nil, fmt.Errorf("failed to create Docker client: %v", err)
	}

	// Load configuration (contains MinIO creds that will be passed to containers)
	var config *DockerRunnerConfig
	if options.Config != nil {
		// Use provided config
		config = options.Config
	} else {
		// Use config loader (default to environment)
		loader := options.ConfigLoader
		if loader == nil {
			loader = &EnvConfigLoader{}
		}
		config, err = loader.LoadConfig()
		if err != nil {
			return nil, fmt.Errorf("failed to load config: %v", err)
		}
	}

	logger := options.Logger
	if logger == nil {
		logger = &util.DefaultLogger{}
	}

	runner := &dockerRunner{
		orchestrator:      NewDockerOrchestrator(dockerClient, logger),
		logger:            logger,
		managedContainers: make(map[string]*ContainerInfo),
		config:            config,
	}

	return runner, nil
}

func (r *dockerRunner) isValidRuntime(runtime string) bool {
	return runtimepkg.IsValidRuntime(runtime)
}

func (r *dockerRunner) getDockerImage(runtime string) (string, error) {
	return runtimepkg.GetDockerImage(runtime)
}

func (r *dockerRunner) Close() error {
	r.logger.Info("Shutting down Docker runner, cleaning up managed containers")
	r.cleanupManagedContainers()
	return nil
}

// cleanupManagedContainers stops and removes all containers that are still being tracked
func (r *dockerRunner) cleanupManagedContainers() {
	r.containersMutex.Lock()
	if len(r.managedContainers) == 0 {
		r.containersMutex.Unlock()
		return // Nothing to clean up
	}

	containerIDs := make([]string, 0, len(r.managedContainers))
	for containerID := range r.managedContainers {
		containerIDs = append(containerIDs, containerID)
	}
	// Clear the map now to prevent new operations during cleanup
	r.managedContainers = make(map[string]*ContainerInfo)
	r.containersMutex.Unlock()

	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Minute)
	defer cancel()

	var wg sync.WaitGroup
	for _, id := range containerIDs {
		wg.Add(1)
		go func(containerID string) {
			defer wg.Done()
			r.logger.Info("Cleaning up managed container", "container_id", containerID)
			if err := r.orchestrator.Stop(ctx, containerID, 30*time.Second); err != nil {
				r.logger.Info("Failed to stop container during cleanup", "container_id", containerID, "error", err.Error())
			}
		}(id)
	}
	wg.Wait()
	r.logger.Info("Completed cleanup of managed containers")
}

// buildContainerConfig constructs Docker container configuration using the daemon approach.
// The container's bootstrap script handles code download, state sync, and log collection.
// MinIO credentials and daemon config are passed via environment variables.
func (r *dockerRunner) buildContainerConfig(
	executable model.Executable,
	imageName string,
) (*container.Config, *container.HostConfig) {
	// Serialize config to JSON for passing to container
	configJSON := "{}"
	if executable.Config != nil {
		if data, err := json.Marshal(executable.Config); err == nil {
			configJSON = string(data)
		}
	}

	// Determine bot type and query entrypoint
	botType := "scheduled"
	if executable.IsLongRunning {
		botType = "realtime"
	}
	queryEntrypoint := executable.EntrypointFiles["query"] // May be empty

	builder := NewContainerBuilder(imageName).
		WithExecutable(executable).
		WithNetwork(r.config.DockerNetwork).
		WithMinIOConfig(
			r.config.GetContainerEndpoint(), // Use container-accessible endpoint
			r.config.MinIOAccessKeyID,
			r.config.MinIOSecretAccessKey,
			r.config.MinIOUseSSL,
		).
		WithDaemonConfigFull(DaemonConfig{
			BotID:           executable.ID,
			CodeFile:        executable.FilePath, // Path to code.zip in MinIO
			Runtime:         executable.Runtime,
			Entrypoint:      executable.EntrypointFiles[executable.Entrypoint],
			QueryEntrypoint: queryEntrypoint,
			Config:          configJSON,
			IsScheduled:     !executable.IsLongRunning,
			BotType:         botType,
		}).
		WithResources(r.config.MemoryLimitMB, r.config.CPUShares)

	// Add query config if this is a query execution
	if executable.QueryPath != "" {
		queryParamsJSON := "{}"
		if executable.QueryParams != nil {
			if data, err := json.Marshal(executable.QueryParams); err == nil {
				queryParamsJSON = string(data)
			}
		}
		r.logger.Info("Adding query config to container", "query_path", executable.QueryPath, "params", queryParamsJSON)
		builder = builder.WithQueryConfig(executable.QueryPath, queryParamsJSON)
	}

	// Dev mode: mount runtime binary from host for faster iteration
	if r.config.DevRuntimePath != "" {
		builder = builder.WithDevRuntime(r.config.DevRuntimePath)
	}

	// Long-running containers should NOT auto-remove on exit so we can capture crash logs.
	// Terminating containers that need result files also shouldn't auto-remove (race condition).
	// Only enable auto-remove for terminating containers that don't need result extraction.
	if executable.IsLongRunning || executable.ResultFilePath != "" {
		builder = builder.WithAutoRemove(false) // Keep container for log/result capture
	} else {
		builder = builder.WithAutoRemove(true) // Run-to-completion cleans up automatically
	}

	// Enable host.docker.internal on Linux for containers to reach the Docker host.
	// This is needed for containers to access services running on the host (e.g., MinIO in tests).
	builder = builder.WithExtraHosts("host.docker.internal:host-gateway")

	return builder.Build()
}

// StartContainer starts a container based on the executable configuration.
// The container's daemon handles code download, state sync, and log collection internally.
// Returns immediately for long-running containers, waits for completion for terminating containers.
func (r *dockerRunner) StartContainer(ctx context.Context, executable model.Executable) (*ExecutionResult, error) {
	startTime := time.Now()
	entrypointFile, ok := executable.EntrypointFiles[executable.Entrypoint]
	if !ok {
		return &ExecutionResult{
			Status:   "error",
			Message:  "Entrypoint file not found",
			Error:    fmt.Sprintf("Entrypoint '%s' not found in bot configuration", executable.Entrypoint),
			ExitCode: 1,
			Duration: time.Since(startTime),
		}, nil
	}
	util.LogWorker("Starting container - bot_id: %s, entrypoint: %s", executable.ID, entrypointFile)

	// Validate runtime
	if !r.isValidRuntime(executable.Runtime) {
		return &ExecutionResult{
			Status:   "error",
			Message:  fmt.Sprintf("Unsupported runtime: %s", executable.Runtime),
			Error:    fmt.Sprintf("Runtime %s is not supported", executable.Runtime),
			ExitCode: 1,
			Duration: time.Since(startTime),
		}, nil
	}

	// Pull Docker image
	imageName, err := r.getDockerImage(executable.Runtime)
	if err != nil {
		return &ExecutionResult{
			Status:   "error",
			Message:  "Unsupported runtime",
			Error:    err.Error(),
			ExitCode: 1,
			Duration: time.Since(startTime),
		}, nil
	}
	if err := r.orchestrator.PullImage(ctx, imageName); err != nil {
		return &ExecutionResult{
			Status:   "error",
			Message:  "Failed to pull Docker image",
			Error:    err.Error(),
			ExitCode: 1,
			Duration: time.Since(startTime),
		}, nil
	}

	config, hostConfig := r.buildContainerConfig(executable, imageName)

	// Create and start container
	if executable.IsLongRunning {
		return r.startLongRunningContainer(ctx, executable, config, hostConfig, startTime)
	}
	return r.startTerminatingContainer(ctx, executable, config, hostConfig, startTime)
}

// StopContainer stops a specific container.
// State sync and log collection are handled by the daemon inside the container.
// The graceful shutdown gives the daemon time to complete final sync before exit.
func (r *dockerRunner) StopContainer(
	ctx context.Context,
	containerID string,
	executable model.Executable,
) error {
	r.logger.Info("Stopping container", "container_id", containerID, "bot_id", executable.ID)

	r.removeManagedContainer(containerID)

	// Stop container gracefully - daemon's cleanup trap will sync state/logs
	if err := r.orchestrator.Stop(ctx, containerID, 30*time.Second); err != nil {
		r.logger.Info("Failed to stop container", "container_id", containerID, "error", err.Error())
	}

	return nil
}

// ListManagedContainers returns all running containers managed by this runner.
func (r *dockerRunner) ListManagedContainers(ctx context.Context) ([]*ContainerInfo, error) {
	filter := filters.NewArgs()
	filter.Add("label", "runtime.managed=true")
	return r.orchestrator.ListContainer(ctx, filter)
}

// ListAllManagedContainers returns ALL containers including exited/crashed ones.
func (r *dockerRunner) ListAllManagedContainers(ctx context.Context) ([]*ContainerInfo, error) {
	filter := filters.NewArgs()
	filter.Add("label", "runtime.managed=true")
	return r.orchestrator.ListAllContainers(ctx, filter)
}

// HandleCrashedContainer cleans up a crashed container.
// Logs and state sync are handled by the daemon inside the container.
// Returns crash info for the caller.
func (r *dockerRunner) HandleCrashedContainer(ctx context.Context, containerInfo *ContainerInfo) (string, error) {
	r.logger.Info("Handling crashed container",
		"container_id", containerInfo.ContainerID,
		"bot_id", containerInfo.ID,
		"exit_code", containerInfo.ExitCode)

	// Format crash info for the caller
	crashInfo := fmt.Sprintf(`{"level":"error","message":"Bot crashed with exit code %d","exit_code":%d,"timestamp":"%s","bot_id":"%s","container_id":"%s"}`,
		containerInfo.ExitCode,
		containerInfo.ExitCode,
		time.Now().UTC().Format(time.RFC3339),
		containerInfo.ID,
		containerInfo.ContainerID)

	// Remove the crashed container
	if err := r.orchestrator.Remove(ctx, containerInfo.ContainerID); err != nil {
		r.logger.Error("Failed to remove crashed container",
			"container_id", containerInfo.ContainerID,
			"error", err.Error())
	}

	// Remove from managed containers list
	r.removeManagedContainer(containerInfo.ContainerID)

	return crashInfo, nil
}

// startLongRunningContainer creates and starts a container for long-running mode
func (r *dockerRunner) startLongRunningContainer(
	ctx context.Context,
	exec model.Executable,
	config *container.Config,
	hostConfig *container.HostConfig,
	startTime time.Time,
) (*ExecutionResult, error) {
	containerID, err := r.orchestrator.CreateAndStart(ctx, config, hostConfig)
	if err != nil {
		return &ExecutionResult{
			Status:   "error",
			Message:  "Failed to create/start container",
			Error:    err.Error(),
			ExitCode: 1,
			Duration: time.Since(startTime),
		}, nil
	}

	// Track the container
	r.addManagedContainer(containerID, exec, config)
	r.logger.Info("Started long-running container", "container_id", containerID, "bot_id", exec.ID)

	return &ExecutionResult{
		Status:        "running",
		Message:       "Long-running container started successfully",
		ContainerID:   containerID,
		IsLongRunning: true,
		ExitCode:      0,
		Duration:      time.Since(startTime),
	}, nil
}

// startTerminatingContainer runs a container to completion for scheduled/backtest jobs.
// The daemon inside the container handles code download, state sync, and log collection.
func (r *dockerRunner) startTerminatingContainer(
	ctx context.Context,
	exec model.Executable,
	config *container.Config,
	hostConfig *container.HostConfig,
	startTime time.Time,
) (*ExecutionResult, error) {
	// Result file path inside container (can be customized for queries)
	resultFilePath := "/bot/result.json"
	if exec.ResultFilePath != "" {
		resultFilePath = exec.ResultFilePath
	}

	runResult, err := r.orchestrator.RunAndWait(ctx, config, hostConfig, resultFilePath)
	if err != nil {
		return &ExecutionResult{
			Status:   "error",
			Message:  "Failed to run container",
			Error:    err.Error(),
			ExitCode: 1,
			Duration: time.Since(startTime),
		}, nil
	}

	// Determine output based on result file and exit code
	var output string
	var finalStatus string
	var errorMessage string

	if len(runResult.ResultFileContents) > 0 {
		output = string(runResult.ResultFileContents)
		if runResult.ExitCode == 0 {
			finalStatus = "success"
		} else {
			finalStatus = "error"
			errorMessage = fmt.Sprintf("Container exited with code %d", runResult.ExitCode)
		}
	} else {
		if runResult.ExitCode == 0 {
			finalStatus = "success"
			if runResult.Logs != "" {
				output = runResult.Logs
			} else {
				output = `{"status":"success","result":null}`
			}
		} else {
			finalStatus = "error"
			errorMessage = fmt.Sprintf("Container exited with code %d", runResult.ExitCode)
			if runResult.Logs != "" {
				output = runResult.Logs
			} else {
				output = fmt.Sprintf(`{"status":"error","message":"Bot exited with code %d"}`, runResult.ExitCode)
			}
		}
	}

	return &ExecutionResult{
		Status:             finalStatus,
		Message:            "Container run completed",
		Output:             output,
		Error:              errorMessage,
		ExitCode:           runResult.ExitCode,
		ResultFileContents: runResult.ResultFileContents,
		Duration:           time.Since(startTime),
	}, nil
}

func (r *dockerRunner) GetContainerStatus(ctx context.Context, containerID string) (*ContainerStatus, error) {
	return r.orchestrator.GetStatus(ctx, containerID)
}

func (r *dockerRunner) addManagedContainer(
	containerID string,
	exec model.Executable,
	config *container.Config,
) {
	r.containersMutex.Lock()
	defer r.containersMutex.Unlock()

	containerInfo := &ContainerInfo{
		ContainerID: containerID,
		ID:          exec.ID,
		Entrypoint:  exec.Entrypoint,
		Status:      "running",
		StartedAt:   time.Now().Format(time.RFC3339),
		Labels:      config.Labels,
	}

	r.managedContainers[containerID] = containerInfo
	r.logger.Info("Added managed container", "container_id", containerID, "bot_id", exec.ID)
}

func (r *dockerRunner) removeManagedContainer(containerID string) *ContainerInfo {
	r.containersMutex.Lock()
	defer r.containersMutex.Unlock()

	info, exists := r.managedContainers[containerID]
	if !exists {
		r.logger.Info("Attempted to remove non-existent managed container", "container_id", containerID)
		return nil
	}

	delete(r.managedContainers, containerID)
	r.logger.Info("Removed managed container", "container_id", containerID, "bot_id", info.ID)
	return info
}

func (r *dockerRunner) GetContainerLogs(ctx context.Context, containerID string, tail int) (string, error) {
	return r.orchestrator.GetLogs(ctx, containerID, tail)
}

func (r *dockerRunner) GetContainerIP(ctx context.Context, containerID string) (string, error) {
	return r.orchestrator.GetContainerIP(ctx, containerID)
}
