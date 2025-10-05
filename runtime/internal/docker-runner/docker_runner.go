// Package dockerrunner provides a Docker-based execution environment for bots and backtests.
// It orchestrates the entire lifecycle from code download to container execution and log collection.
package dockerrunner

import (
	"bytes"
	"context"
	_ "embed"
	"fmt"
	"os"
	"path/filepath"
	miniologger "runtime/internal/minio-logger"
	"runtime/internal/model"
	"runtime/internal/util"
	"strings"
	"sync"
	"time"

	"github.com/docker/docker/api/types/container"
	"github.com/docker/docker/api/types/filters"
	"github.com/docker/docker/client"
	"github.com/minio/minio-go/v7"
	"github.com/minio/minio-go/v7/pkg/credentials"
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

	// ListManagedContainers returns all containers managed by this runner for a given segment.
	ListManagedContainers(ctx context.Context, segment int32) ([]*ContainerInfo, error)

	// GetContainerLogs retrieves the last N lines of logs from a container.
	GetContainerLogs(ctx context.Context, containerID string, tail int) (string, error)

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
	ContainerID string            `json:"container_id"` // Docker container ID
	ID          string            `json:"id"`           // Bot/backtest ID
	Entrypoint  string            `json:"entrypoint"`   // Entrypoint script filename (e.g., "main.py")
	Status      string            `json:"status"`       // Container status
	StartedAt   string            `json:"started_at"`   // RFC3339 timestamp
	Labels      map[string]string `json:"labels,omitempty"`
	BotDir      string            `json:"bot_dir,omitempty"`    // Host directory path for cleanup
	LastLogTime time.Time         `json:"last_log_time"`        // Last log collection timestamp
	LogsSince   string            `json:"logs_since,omitempty"` // RFC3339Nano for incremental logs
}

// dockerRunner is the concrete implementation of DockerRunner interface.
// It delegates responsibilities to specialized components for better separation of concerns.
type dockerRunner struct {
	// Component dependencies
	orchestrator  ContainerOrchestrator // Handles Docker operations
	codeManager   CodeManager           // Handles code download/extraction
	scriptManager ScriptManager         // Handles entrypoint script generation
	logCollector  LogCollector          // Background log collection for long-running containers
	minioLogger   miniologger.MinIOLogger

	// Configuration
	resultsBucket     string
	config            *DockerRunnerConfig
	minioClient       *minio.Client
	logger            util.Logger
	managedContainers map[string]*ContainerInfo // Tracks active containers
	containersMutex   sync.RWMutex              // Protects managedContainers map
}

// DockerRunnerOptions contains configuration options for creating a DockerRunner.
type DockerRunnerOptions struct {
	Logger util.Logger // Optional logger, defaults to util.DefaultLogger if nil
}

// NewDockerRunner creates a new DockerRunner instance with all required components initialized.
// It loads configuration from environment variables and starts background services like log collection.
func NewDockerRunner(options DockerRunnerOptions) (*dockerRunner, error) {
	// Create Docker client
	dockerClient, err := client.NewClientWithOpts(client.FromEnv, client.WithAPIVersionNegotiation())
	if err != nil {
		return nil, fmt.Errorf("failed to create Docker client: %v", err)
	}

	// Create MinIO client
	config, err := LoadConfigFromEnv()
	if err != nil {
		return nil, fmt.Errorf("failed to load MinIO config: %v", err)
	}

	minioClient, err := minio.New(config.MinIOEndpoint, &minio.Options{
		Creds: credentials.NewStaticV4(
			config.MinIOAccessKeyID,
			config.MinIOSecretAccessKey,
			"",
		),
		Secure: config.MinIOUseSSL,
	})
	if err != nil {
		return nil, fmt.Errorf("failed to create MinIO client: %v", err)
	}

	logger := options.Logger
	if logger == nil {
		logger = &util.DefaultLogger{}
	}

	// Ensure temp directory exists
	if err := os.MkdirAll(config.TempDir, 0755); err != nil {
		return nil, fmt.Errorf("failed to create temp directory: %v", err)
	}

	// Create MinIO logger for bot logs
	minioLogger, err := miniologger.NewMinIOLogger(context.Background())
	if err != nil {
		logger.Info("Warning: Failed to create MinIO logger, bot logs will not be persisted", "error", err.Error())
		// Continue without MinIO logging rather than failing completely
	}

	runner := &dockerRunner{
		codeManager:   NewMinioCodeManager(minioClient, config, logger),
		scriptManager: NewScriptManager(logger),
		orchestrator:  NewDockerOrchestrator(dockerClient, logger),
		minioLogger:   minioLogger,

		minioClient:       minioClient,
		logger:            logger,
		managedContainers: make(map[string]*ContainerInfo),
		resultsBucket:     config.MinioResultsBucket,
		config:            config,
	}

	if runner.minioLogger != nil {
		runner.logCollector = runner.initializeLogCollector()
		runner.logCollector.Start()
	}

	return runner, nil
}

func (r *dockerRunner) isValidRuntime(runtime string) bool {
	validRuntimes := []string{"python3.11", "nodejs20"}
	for _, valid := range validRuntimes {
		if runtime == valid {
			return true
		}
	}
	return false
}

func (r *dockerRunner) getDockerImage(runtime string) string {
	switch runtime {
	case "python3.11":
		return "python:3.11-slim"
	case "nodejs20":
		return "node:20-alpine"
	default:
		return "python:3.11-slim" // fallback
	}
}

func removeDir(dir string) error {
	// Fix permissions to ensure we can delete the directory
	if err := fixDirectoryPermissions(dir); err != nil {
		return fmt.Errorf("failed to fix directory permissions: %v", err)
	}
	if err := os.RemoveAll(dir); err != nil {
		return fmt.Errorf("failed to remove directory: %v", err)
	}
	return nil
}

// fixDirectoryPermissions fixes permissions on a directory to allow cleanup
// This is needed in case the container created files with different permissions
func fixDirectoryPermissions(dir string) error {
	// Check if directory exists before attempting to fix permissions
	if _, err := os.Stat(dir); os.IsNotExist(err) {
		return nil // Directory already cleaned up, nothing to fix
	}

	// Make all files and directories readable/writable by owner
	// Ignore permission errors since we can't always change root-owned files
	filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return nil // Continue walking, ignore errors
		}
		// Set permissions to 755 for directories, 644 for files
		if info.IsDir() {
			os.Chmod(path, 0755) // Ignore error
		} else {
			os.Chmod(path, 0644) // Ignore error
		}
		return nil // Always continue walking
	})
	return nil // Don't return permission errors
}

func (r *dockerRunner) initializeLogCollector() LogCollector {
	getContainersFunc := func() []*ContainerInfo {
		r.containersMutex.RLock()
		defer r.containersMutex.RUnlock()
		containers := make([]*ContainerInfo, 0, len(r.managedContainers))
		for _, info := range r.managedContainers {
			containers = append(containers, info)
		}
		return containers
	}

	return NewLogCollector(
		30*time.Second,
		r.orchestrator,
		r.minioLogger,
		r.logger,
		getContainersFunc,
	)
}

func (r *dockerRunner) Close() error {
	r.logger.Info("Shutting down Docker runner, cleaning up managed containers")

	if r.logCollector != nil {
		r.logCollector.Stop()
	}
	r.cleanupManagedContainers()
	if r.minioLogger != nil {
		r.minioLogger.Close()
	}

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

// buildContainerConfig constructs Docker container configuration using the builder pattern.
// It determines the appropriate shell, sets up volume mounts, and applies resource limits.
func (r *dockerRunner) buildContainerConfig(
	executable model.Executable,
	botDir, imageName string,
) (*container.Config, *container.HostConfig) {
	// Determine shell based on image (Alpine uses /bin/sh, others use /bin/bash)
	shell := "/bin/bash"
	if strings.Contains(imageName, "alpine") {
		shell = "/bin/sh"
	}

	builder := NewContainerBuilder(imageName).
		WithExecutable(executable).
		WithCommand(shell, fmt.Sprintf("/%s/entrypoint.sh", executable.Entrypoint)).
		WithBinds(fmt.Sprintf("%s:/%s", botDir, executable.Entrypoint)).
		WithResources(r.config.MemoryLimitMB, r.config.CPUShares)

	// Long-running containers auto-remove on exit, terminating containers need manual cleanup
	if executable.IsLongRunning {
		builder = builder.WithAutoRemove(true)
	} else {
		builder = builder.WithAutoRemove(false)
	}

	return builder.Build()
}

// StartContainer starts a container based on the executable configuration.
// The execution flow is: validate -> download code -> pull image -> generate script -> start container.
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
	util.LogWorker("Starting long-running bot container - bot_id: %s, entrypoint: %s", executable.ID, entrypointFile)

	// Validate runtime
	if !r.isValidRuntime(executable.Runtime) {
		return &ExecutionResult{
			Status:   "error",
			Message:  fmt.Sprintf("Unsupported runtime: %s", executable.Runtime),
			Error:    fmt.Sprintf("Runtime %s is not supported. Supported runtimes: python3.11, nodejs20", executable.Runtime),
			ExitCode: 1,
			Duration: time.Since(startTime),
		}, nil
	}

	// Download and extract bot code
	botDir, err := r.codeManager.FetchAndExtract(ctx, executable)
	if err != nil {
		return &ExecutionResult{
			Status:   "error",
			Message:  "Failed to download bot code",
			Error:    err.Error(),
			ExitCode: 1,
			Duration: time.Since(startTime),
		}, nil
	}

	// Pull Docker image
	imageName := r.getDockerImage(executable.Runtime)
	if err := r.orchestrator.PullImage(ctx, imageName); err != nil {
		return &ExecutionResult{
			Status:   "error",
			Message:  "Failed to pull Docker image",
			Error:    err.Error(),
			ExitCode: 1,
			Duration: time.Since(startTime),
		}, nil
	}

	// Create entrypoint script
	_, err = r.scriptManager.Create(ctx, executable, botDir)
	if err != nil {
		return &ExecutionResult{
			Status:   "error",
			Message:  "Failed to create entrypoint script",
			Error:    err.Error(),
			ExitCode: 1,
			Duration: time.Since(startTime),
		}, nil
	}

	config, hostConfig := r.buildContainerConfig(executable, botDir, imageName)

	// Create and start container for long-running mode
	if executable.IsLongRunning {
		return r.startLongRunningContainer(ctx, executable, config, hostConfig, botDir, startTime)
	} else {
		return r.startTerminatingContainer(ctx, executable, config, hostConfig, botDir, startTime)
	}
}

// StopContainer stops a specific container
func (r *dockerRunner) StopContainer(
	ctx context.Context,
	containerID string,
	executable model.Executable,
) error {
	r.logger.Info("Stopping container", "container_id", containerID, "bot_id", executable.ID)

	info := r.removeManagedContainer(containerID)
	if info == nil {
		r.logger.Info("Container not found in managed list, may have already been removed", "container_id", containerID)
	}

	if err := r.orchestrator.Stop(ctx, containerID, 30*time.Second); err != nil {
		r.logger.Info("Failed to stop container", "container_id", containerID, "error", err.Error())
	}

	if executable.PersistResults {
		resultPath := fmt.Sprintf("/%s/result.json", executable.Entrypoint)
		contents, err := r.orchestrator.CopyFromContainer(ctx, containerID, resultPath)
		if err == nil {
			err := r.StoreAnalysisResult(ctx, executable.ID, contents)
			if err != nil {
				r.logger.Info("Failed to store analysis result to MinIO", "bot_id", executable.ID, "error", err.Error())
			}
		}
	}

	if info != nil && info.BotDir != "" {
		if err := removeDir(info.BotDir); err != nil {
			r.logger.Info("Failed to clean up bot directory", "bot_dir", info.BotDir, "error", err.Error())
		}
	}

	return nil
}

// ListManagedContainers returns information about managed containers for the specified segment
func (r *dockerRunner) ListManagedContainers(ctx context.Context, segment int32) ([]*ContainerInfo, error) {
	filter := filters.NewArgs()
	filter.Add("label", "runtime.managed=true")
	filter.Add("label", fmt.Sprintf("runtime.segment=%d", segment))
	return r.orchestrator.ListContainer(ctx, filter)
}

// startLongRunningContainer creates and starts a container for long-running mode
func (r *dockerRunner) startLongRunningContainer(
	ctx context.Context,
	exec model.Executable,
	config *container.Config,
	hostConfig *container.HostConfig,
	botDir string,
	startTime time.Time,
) (*ExecutionResult, error) {
	containerID, err := r.orchestrator.CreateAndStart(ctx, config, hostConfig)
	if err != nil {
		if rmErr := removeDir(botDir); rmErr != nil {
			r.logger.Info("Failed to clean up bot directory after container start failure", "bot_dir", botDir, "error", err.Error())
		}

		return &ExecutionResult{
			Status:   "error",
			Message:  "Failed to create/start container",
			Error:    err.Error(),
			ExitCode: 1,
			Duration: time.Since(startTime),
		}, nil
	}

	// If successful, track the container
	r.addManagedContainer(containerID, exec, botDir, config)
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

func (r *dockerRunner) startTerminatingContainer(
	ctx context.Context,
	exec model.Executable,
	config *container.Config,
	hostConfig *container.HostConfig,
	botDir string,
	startTime time.Time,
) (*ExecutionResult, error) {
	// Add logging for debugging short-lived containers
	defer os.RemoveAll(botDir) // Ensure bot directory is cleaned up

	resultFilePath := ""
	if exec.PersistResults {
		resultFilePath = fmt.Sprintf("/%s/result.json", exec.Entrypoint)
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

	if exec.PersistResults && len(runResult.ResultFileContents) > 0 {
		if err := r.StoreAnalysisResult(ctx, exec.ID, runResult.ResultFileContents); err != nil {
			r.logger.Info("Failed to store analysis result to MinIO", "bot_id", exec.ID, "error", err.Error())
			// Continue even if storing analysis fails
		}
	}

	finalStatus := "success"
	errorMessage := ""
	if runResult.ExitCode != 0 {
		finalStatus = "error"
		errorMessage = fmt.Sprintf("Container exited with non-zero status code: %d", runResult.ExitCode)
	}

	return &ExecutionResult{
		Status:             finalStatus,
		Message:            "Container run completed",
		Output:             runResult.Logs,
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
	botDir string,
	config *container.Config,
) {
	r.containersMutex.Lock()
	defer r.containersMutex.Unlock()

	startTime := time.Now()
	containerInfo := &ContainerInfo{
		ContainerID: containerID,
		ID:          exec.ID,
		Entrypoint:  exec.Entrypoint,
		Status:      "running",
		StartedAt:   startTime.Format(time.RFC3339),
		Labels:      config.Labels,
		BotDir:      botDir, // Store for cleanup when container is stopped
		LastLogTime: startTime,
		LogsSince:   startTime.Format(time.RFC3339Nano), // Initialize for incremental log collection
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

// StoreAnalysisResult stores the analysis result as JSON at {bot_id}/analysis.json in the bots bucket
func (r *dockerRunner) StoreAnalysisResult(ctx context.Context, botID string, result []byte) error {
	// Generate file path: {bot_id}/analysis.json
	objectName := fmt.Sprintf("%s/analysis.json", botID)

	// Write JSON to MinIO
	reader := bytes.NewReader(result)
	_, err := r.minioClient.PutObject(ctx, r.resultsBucket, objectName, reader, int64(len(result)), minio.PutObjectOptions{
		ContentType: "application/json",
	})
	if err != nil {
		return fmt.Errorf("failed to write analysis result to MinIO: %v", err)
	}

	return nil
}

func (r *dockerRunner) GetContainerLogs(ctx context.Context, containerID string, tail int) (string, error) {
	return r.orchestrator.GetLogs(ctx, containerID, tail)
}
