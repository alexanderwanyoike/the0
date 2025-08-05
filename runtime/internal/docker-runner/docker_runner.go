package dockerrunner

import (
	"archive/tar"
	"archive/zip"
	"bytes"
	"compress/gzip"
	"context"
	_ "embed"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"path/filepath"
	miniologger "runtime/internal/minio-logger"
	"runtime/internal/model"
	"runtime/internal/util"
	"strconv"
	"strings"
	"sync"
	"time"

	"runtime/internal/docker-runner/entrypoints"

	"github.com/docker/docker/api/types/container"
	"github.com/docker/docker/api/types/filters"
	"github.com/docker/docker/api/types/image"
	"github.com/docker/docker/client"
	"github.com/docker/docker/pkg/stdcopy"
	"github.com/minio/minio-go/v7"
	"github.com/minio/minio-go/v7/pkg/credentials"
)

type DockerRunner interface {
	// Container lifecycle management for long-running bots
	StartContainer(
		ctx context.Context,
		executable model.Executable,
	) (*ExecutionResult, error)
	StopContainer(
		ctx context.Context,
		containerID string,
		executable model.Executable,
	) error
	GetContainerStatus(ctx context.Context, containerID string) (*ContainerStatus, error)
	ListManagedContainers(ctx context.Context, segment int32) ([]*ContainerInfo, error)

	// Container operations
	GetContainerLogs(ctx context.Context, containerID string, tail int) (string, error)

	Close() error
}

type ExecutionResult struct {
	Status   string                 `json:"status"`
	Message  string                 `json:"message"`
	Output   string                 `json:"output"`
	Error    string                 `json:"error,omitempty"`
	ExitCode int                    `json:"exit_code"`
	Duration time.Duration          `json:"duration"`
	Metadata map[string]interface{} `json:"metadata,omitempty"`
	// Long-running bot support
	ContainerID        string `json:"container_id,omitempty"`         // For tracking long-running containers
	IsLongRunning      bool   `json:"is_long_running,omitempty"`      // Indicates if bot should keep running
	ResultFileContents []byte `json:"result_file_contents,omitempty"` // For file-based result parsing
}

// ContainerStatus represents the current status of a container
type ContainerStatus struct {
	ContainerID string            `json:"container_id"`
	Status      string            `json:"status"` // "running", "exited", "paused", "restarting", "dead", "not_found"
	ExitCode    int               `json:"exit_code,omitempty"`
	StartedAt   string            `json:"started_at,omitempty"`
	FinishedAt  string            `json:"finished_at,omitempty"`
	Error       string            `json:"error,omitempty"`
	Labels      map[string]string `json:"labels,omitempty"`
}

// ContainerInfo represents information about a managed container
type ContainerInfo struct {
	ContainerID string            `json:"container_id"`
	ID          string            `json:"id"`
	Entrypoint  string            `json:"entrypoint"`
	Status      string            `json:"status"`
	StartedAt   string            `json:"started_at"`
	Labels      map[string]string `json:"labels,omitempty"`
	BotDir      string            `json:"bot_dir,omitempty"`    // Directory path for cleanup
	LastLogTime time.Time         `json:"last_log_time"`        // Last time logs were collected
	LogsSince   string            `json:"logs_since,omitempty"` // Timestamp for incremental log collection
}

// LogCaptureMetrics tracks metrics for log capture operations
type LogCaptureMetrics struct {
	// Log collection metrics
	LogCollectionAttempts  int64 // Total log collection attempts
	LogCollectionSuccesses int64 // Successful log collections
	LogCollectionFailures  int64 // Failed log collections

	// Log streaming metrics
	LogStreamConnections   int64 // Total log stream connections
	LogStreamReconnections int64 // Stream reconnection attempts
	LogStreamFailures      int64 // Stream connection failures

	// MinIO operation metrics
	MinIOWriteAttempts  int64 // MinIO write attempts
	MinIOWriteSuccesses int64 // Successful MinIO writes
	MinIOWriteFailures  int64 // Failed MinIO writes

	// Container metrics
	ContainersTracked    int64 // Currently tracked containers
	ContainersReconciled int64 // Containers reconciled
	ContainersCleaned    int64 // Containers cleaned up

	// Performance metrics
	LastLogCollectionTime        time.Time
	LastReconciliationTime       time.Time
	AverageLogCollectionDuration time.Duration

	mutex sync.RWMutex // Protects all metrics
}

type dockerRunner struct {
	dockerClient      *client.Client
	resultsBucket     string
	minioClient       *minio.Client
	logger            util.Logger
	tempDir           string
	managedContainers map[string]*ContainerInfo // containerID -> info
	containersMutex   sync.RWMutex              // Protects managedContainers
	minioLogger       miniologger.MinIOLogger   // For persisting bot logs to MinIO
	logCollectorStop  chan bool                 // Channel to stop log collection
	logCollectorDone  chan bool                 // Channel to signal log collection stopped
	metrics           *LogCaptureMetrics        // Metrics for log capture operations
}

type DockerRunnerOptions struct {
	Logger  util.Logger
	TempDir string
}

func NewDockerRunner(options DockerRunnerOptions) (*dockerRunner, error) {
	// Create Docker client
	dockerClient, err := client.NewClientWithOpts(client.FromEnv, client.WithAPIVersionNegotiation())
	if err != nil {
		return nil, fmt.Errorf("failed to create Docker client: %v", err)
	}

	// Create MinIO client
	endpoint := os.Getenv("MINIO_ENDPOINT")
	if endpoint == "" {
		return nil, fmt.Errorf("MINIO_ENDPOINT environment variable is required")
	}

	accessKey := os.Getenv("MINIO_ACCESS_KEY")
	if accessKey == "" {
		return nil, fmt.Errorf("MINIO_ACCESS_KEY environment variable is required")
	}

	secretKey := os.Getenv("MINIO_SECRET_KEY")
	if secretKey == "" {
		return nil, fmt.Errorf("MINIO_SECRET_KEY environment variable is required")
	}

	useSSL := os.Getenv("MINIO_SSL") == "true"

	resultsBucket := os.Getenv("MINIO_BACKTESTS_BUCKET")
	if resultsBucket == "" {
		resultsBucket = "test-results"
	}

	minioClient, err := minio.New(endpoint, &minio.Options{
		Creds:  credentials.NewStaticV4(accessKey, secretKey, ""),
		Secure: useSSL,
	})
	if err != nil {
		return nil, fmt.Errorf("failed to create MinIO client: %v", err)
	}

	tempDir := options.TempDir
	if tempDir == "" {
		tempDir = "/tmp/runtime"
	}

	logger := options.Logger
	if logger == nil {
		logger = &util.DefaultLogger{}
	}

	// Ensure temp directory exists
	if err := os.MkdirAll(tempDir, 0755); err != nil {
		return nil, fmt.Errorf("failed to create temp directory: %v", err)
	}

	// Create MinIO logger for bot logs
	minioLogger, err := miniologger.NewMinIOLogger(context.Background())
	if err != nil {
		logger.Info("Warning: Failed to create MinIO logger, bot logs will not be persisted", "error", err.Error())
		// Continue without MinIO logging rather than failing completely
	}

	runner := &dockerRunner{
		dockerClient:      dockerClient,
		minioClient:       minioClient,
		logger:            logger,
		tempDir:           tempDir,
		managedContainers: make(map[string]*ContainerInfo),
		minioLogger:       minioLogger,
		resultsBucket:     resultsBucket,
		logCollectorStop:  make(chan bool),

		logCollectorDone: make(chan bool),
		metrics:          &LogCaptureMetrics{},
	}

	// Start background log collector for long-running containers
	if minioLogger != nil {
		go runner.startLogCollector()
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

func (r *dockerRunner) downloadAndExtractBotCode(ctx context.Context, executable model.Executable) (string, error) {
	// Use configured bucket for bot code instead of parsing from path
	bucketName := os.Getenv("MINIO_CODE_BUCKET")
	if bucketName == "" {
		bucketName = "custom-bots" // Default bucket for bot code
	}

	// The FilePath is the object path within the bucket
	objectName := executable.FilePath

	// Create bot-specific temp directory
	botDir := filepath.Join(r.tempDir, executable.ID)
	if err := os.MkdirAll(botDir, 0755); err != nil {
		return "", fmt.Errorf("failed to create bot directory: %v", err)
	}

	// Download file from MinIO
	object, err := r.minioClient.GetObject(ctx, bucketName, objectName, minio.GetObjectOptions{})
	if err != nil {
		return "", fmt.Errorf("failed to get object from MinIO: %v", err)
	}
	defer object.Close()

	// Read all data
	data, err := io.ReadAll(object)
	if err != nil {
		return "", fmt.Errorf("failed to read from MinIO: %v", err)
	}

	// Determine file type and extract
	// Try ZIP format first (most common), then fall back to file extension detection
	if strings.HasSuffix(objectName, ".zip") || len(data) >= 4 && string(data[0:4]) == "PK\x03\x04" {
		if err := r.extractZip(data, botDir); err != nil {
			return "", fmt.Errorf("failed to extract zip: %v", err)
		}
	} else if strings.HasSuffix(objectName, ".tar.gz") || strings.HasSuffix(objectName, ".tgz") {
		if err := r.extractTarGz(data, botDir); err != nil {
			return "", fmt.Errorf("failed to extract tar.gz: %v", err)
		}
	} else {
		// Default to ZIP format for files without extension (like our MinIO files)
		if err := r.extractZip(data, botDir); err != nil {
			return "", fmt.Errorf("failed to extract as zip (unsupported archive format): %s, error: %v", objectName, err)
		}
	}

	util.LogWorker("Downloaded and extracted bot code - bot_id: %s, bucket: %s, object: %s", executable.ID, bucketName, objectName)
	return botDir, nil
}

func (r *dockerRunner) extractZip(data []byte, destDir string) error {
	reader := bytes.NewReader(data)
	zipReader, err := zip.NewReader(reader, int64(len(data)))
	if err != nil {
		return err
	}

	for _, file := range zipReader.File {
		path := filepath.Join(destDir, file.Name)

		// Security check: prevent directory traversal
		if !strings.HasPrefix(path, filepath.Clean(destDir)+string(os.PathSeparator)) {
			continue
		}

		if file.FileInfo().IsDir() {
			os.MkdirAll(path, file.FileInfo().Mode())
			continue
		}

		// Create directory if needed
		if err := os.MkdirAll(filepath.Dir(path), 0755); err != nil {
			return err
		}

		fileReader, err := file.Open()
		if err != nil {
			return err
		}

		targetFile, err := os.OpenFile(path, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, file.FileInfo().Mode())
		if err != nil {
			fileReader.Close()
			return err
		}

		_, err = io.Copy(targetFile, fileReader)
		fileReader.Close()
		targetFile.Close()
		if err != nil {
			return err
		}
	}

	return nil
}

func (r *dockerRunner) extractTarGz(data []byte, destDir string) error {
	reader := bytes.NewReader(data)
	gzReader, err := gzip.NewReader(reader)
	if err != nil {
		return err
	}
	defer gzReader.Close()

	tarReader := tar.NewReader(gzReader)

	for {
		header, err := tarReader.Next()
		if err == io.EOF {
			break
		}
		if err != nil {
			return err
		}

		path := filepath.Join(destDir, header.Name)

		// Security check: prevent directory traversal
		if !strings.HasPrefix(path, filepath.Clean(destDir)+string(os.PathSeparator)) {
			continue
		}

		switch header.Typeflag {
		case tar.TypeDir:
			if err := os.MkdirAll(path, os.FileMode(header.Mode)); err != nil {
				return err
			}
		case tar.TypeReg:
			// Create directory if needed
			if err := os.MkdirAll(filepath.Dir(path), 0755); err != nil {
				return err
			}

			file, err := os.OpenFile(path, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, os.FileMode(header.Mode))
			if err != nil {
				return err
			}

			_, err = io.Copy(file, tarReader)
			file.Close()
			if err != nil {
				return err
			}
		}
	}

	return nil
}

func (r *dockerRunner) pullDockerImage(ctx context.Context, imageName string) error {
	util.LogWorker("Pulling Docker image: %s", imageName)

	reader, err := r.dockerClient.ImagePull(ctx, imageName, image.PullOptions{})
	if err != nil {
		return fmt.Errorf("failed to pull image: %v", err)
	}
	defer reader.Close()

	// Read the response to ensure the pull completes
	_, err = io.ReadAll(reader)
	if err != nil {
		return fmt.Errorf("failed to read pull response: %v", err)
	}

	util.LogWorker("Successfully pulled Docker image: %s", imageName)
	return nil
}

func (r *dockerRunner) createEntrypointScript(executable model.Executable, botDir string) (string, error) {
	// Add logging for debugging
	r.logger.Info("DOCKER_RUNNER: Creating entrypoint script", "bot_id", executable.ID)
	r.logger.Info("DOCKER_RUNNER: Entrypoint type", "entrypoint", executable.Entrypoint)
	r.logger.Info("DOCKER_RUNNER: Available EntrypointFiles", "files", executable.EntrypointFiles)

	// Get the script path from entrypoints
	scriptPath, exists := executable.EntrypointFiles[executable.Entrypoint]
	if !exists {
		r.logger.Error("DOCKER_RUNNER: Entrypoint not found", "entrypoint", executable.Entrypoint, "available", executable.EntrypointFiles)
		return "", fmt.Errorf("entrypoint '%s' not found in executable configuration", executable.Entrypoint)
	}

	r.logger.Info("DOCKER_RUNNER: Found script path", "script_path", scriptPath, "entrypoint", executable.Entrypoint)

	runtime := executable.Runtime
	var entrypointScript string

	// Convert config to JSON for passing to the script
	configJSON, err := json.Marshal(executable.Config)
	if err != nil {
		return "", fmt.Errorf("failed to marshal bot config: %v", err)
	}

	codeFactory := entrypoints.NewCodeEntrypointFactory(
		executable.Entrypoint,
		runtime,
	)
	entrypointScript, err = codeFactory.GetCode()
	if err != nil {
		return "", fmt.Errorf("failed to get entrypoint code: %v", err)
	}

	bashFactory := entrypoints.NewBashEntrypointFactory(
		executable.Entrypoint,
		entrypointScript,
		executable.ID,
		string(configJSON),
		scriptPath,
	)

	entrypointScript, err = bashFactory.BuildBashEntrypoint(runtime)
	if err != nil {
		return "", fmt.Errorf("failed to build bash entrypoint: %v", err)
	}

	// Ensure botDir exists before writing script (fixes race condition with cleanup)
	if _, err := os.Stat(botDir); os.IsNotExist(err) {
		if err := os.MkdirAll(botDir, 0755); err != nil {
			return "", fmt.Errorf("failed to create bot directory: %v", err)
		}
	}

	// Write entrypoint script to file
	scriptFile := filepath.Join(botDir, "entrypoint.sh")
	r.logger.Info("DOCKER_RUNNER: Writing entrypoint script", "script_file", scriptFile, "bot_dir", botDir)
	if err := os.WriteFile(scriptFile, []byte(entrypointScript), 0755); err != nil {
		r.logger.Error("DOCKER_RUNNER: Failed to write entrypoint script", "error", err, "script_file", scriptFile)
		return "", fmt.Errorf("failed to write entrypoint script: %v", err)
	}
	r.logger.Info("DOCKER_RUNNER: Successfully wrote entrypoint script", "script_file", scriptFile, "size", len(entrypointScript))

	return scriptFile, nil
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

// getMemoryLimit returns the memory limit for containers from environment or default
func getMemoryLimit() int64 {
	memoryLimitMB := os.Getenv("BOT_MEMORY_LIMIT_MB")
	if memoryLimitMB == "" {
		return 512 * 1024 * 1024 // Default 512MB
	}

	limitMB, err := strconv.Atoi(memoryLimitMB)
	if err != nil {
		return 512 * 1024 * 1024 // Default on error
	}

	return int64(limitMB) * 1024 * 1024
}

// getCPUShares returns the CPU shares for containers from environment or default
func getCPUShares() int64 {
	cpuShares := os.Getenv("BOT_CPU_SHARES")
	if cpuShares == "" {
		return 512 // Default half CPU
	}

	shares, err := strconv.Atoi(cpuShares)
	if err != nil {
		return 512 // Default on error
	}

	return int64(shares)
}

func (r *dockerRunner) Close() error {
	// Stop log collector
	if r.logCollectorStop != nil {
		close(r.logCollectorStop)
		// Wait for log collector to finish
		<-r.logCollectorDone
	}

	// Close MinIO logger
	if r.minioLogger != nil {
		r.minioLogger.Close()
	}

	// MinIO client doesn't require explicit close
	if r.dockerClient != nil {
		return r.dockerClient.Close()
	}
	return nil
}

// StartContainer starts a bot as a long-running container
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
	botDir, err := r.downloadAndExtractBotCode(ctx, executable)
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
	if err := r.pullDockerImage(ctx, imageName); err != nil {
		return &ExecutionResult{
			Status:   "error",
			Message:  "Failed to pull Docker image",
			Error:    err.Error(),
			ExitCode: 1,
			Duration: time.Since(startTime),
		}, nil
	}

	// Create entrypoint script
	_, err = r.createEntrypointScript(executable, botDir)
	if err != nil {
		return &ExecutionResult{
			Status:   "error",
			Message:  "Failed to create entrypoint script",
			Error:    err.Error(),
			ExitCode: 1,
			Duration: time.Since(startTime),
		}, nil
	}

	// Create and start container for long-running mode
	if executable.IsLongRunning {
		result, err := r.createLongRunningContainer(ctx, imageName, botDir, executable, entrypointFile)

		if err != nil {
			return &ExecutionResult{
				Status:   "error",
				Message:  "Failed to create long-running container",
				Error:    err.Error(),
				ExitCode: 1,
				Duration: time.Since(startTime),
			}, nil
		}

		result.Duration = time.Since(startTime)
		util.LogWorker("Long-running bot container started successfully - bot_id: %s, container_id: %s, duration: %v", executable.ID, result.ContainerID, result.Duration)
		return result, nil
	} else {
		result, err := r.createTerminatingContainer(ctx, imageName, botDir, executable, executable.Entrypoint)
		if err != nil {
			return &ExecutionResult{
				Status:   "error",
				Message:  "Failed to create terminating container",
				Error:    err.Error(),
				ExitCode: 1,
				Duration: time.Since(startTime),
			}, nil
		}
		result.Duration = time.Since(startTime)
		util.LogWorker("Short-lived bot container has completed - bot_id: %s, container_id: %s, duration: %v", executable.ID, result.ContainerID, result.Duration)
		return result, nil
	}
}

// StopContainer stops a specific container
func (r *dockerRunner) StopContainer(
	ctx context.Context,
	containerID string,
	executable model.Executable,
) error {
	util.LogWorker("Stopping bot container - container_id: %s", containerID)

	// 1. Get container info before stopping it to retrieve the BotDir
	r.containersMutex.Lock()
	containerInfo, exists := r.managedContainers[containerID]

	// Remove the container from tracking immediately to prevent new operations on it
	if exists {
		delete(r.managedContainers, containerID)
	}

	r.containersMutex.Unlock()

	// If the container wasn't tracked we cant clean up its directory we can however still try to stop it.
	if !exists {
		util.LogWorker("Container not found in managed containers - container_id: %s", containerID)
	}

	// 2. Set a timeout for the stop command.
	// This is the graceful shutdown period.
	timeout := 30
	if err := r.dockerClient.ContainerStop(ctx, containerID, container.StopOptions{Timeout: &timeout}); err != nil {
		// This can happen if the container is already stopped or doesnt exist.
		if !client.IsErrNotFound(err) {
			util.LogWorker("Error sending stop signal to container - container_id: %s, error: %v", containerID, err)
			r.logger.Info("Error sending stop signal to container", "container_id", containerID, "error", err.Error())
		}
	}

	// 3. Wait for the container to fully exit.
	// This call will block until the container's status is "exited" or the context times out.
	waitCtx, cancel := context.WithTimeout(context.Background(), 2*time.Minute)
	defer cancel()

	statusCh, errCh := r.dockerClient.ContainerWait(waitCtx, containerID, container.WaitConditionNotRunning)

	select {
	case err := <-errCh:
		// This error means the wait operation itself failed (e.g., context cancelled, Docker daemon error).
		util.LogWorker("Error waiting for container to stop - container_id: %s, error: %v", containerID, err)
		r.logger.Info("Error waiting for container to stop", "container_id", containerID, "error", err.Error())
		// Don't return here; still attempt cleanup.
	case status := <-statusCh:
		// The container has exited. Log its status code.
		// Try to copy result file before cleanup
		if executable.PersistResults {
			if analysisResult, err := r.storeAnalysis(ctx, executable, containerID); err != nil {
				util.LogWorker("Failed to store analysis for container - container_id: %s, error: %v", containerID, err)
				r.logger.Info("Failed to store analysis", "container_id", containerID, "error", err.Error())
			} else {
				util.LogWorker("Successfully stored analysis for container - container_id: %s, analysis: %s", containerID, string(analysisResult))
			}
		}
		util.LogWorker("Container stopped successfully with exit code - container_id: %s, exit_code: %d", containerID, status.StatusCode)
	}

	// 4. Now that the container has stopped, safely clean up its host directory.
	// Since AutoRemove is true, the Docker daemon handles removing the container itself.
	if containerInfo != nil && containerInfo.BotDir != "" {
		util.LogWorker("Cleaning up bot directory - container_id: %s, bot_dir: %s", containerID, containerInfo.BotDir)

		if err := fixDirectoryPermissions(containerInfo.BotDir); err != nil {
			util.LogWorker("Failed to fix directory permissions - container_id: %s, bot_dir: %s, error: %v", containerID, containerInfo.BotDir, err)
			r.logger.Info("Failed to fix directory permissions", "container_id", containerID, "bot_dir", containerInfo.BotDir, "error", err.Error())
		}

		if err := os.RemoveAll(containerInfo.BotDir); err != nil {
			util.LogWorker("Failed to remove bot directory - container_id: %s, bot_dir: %s, error: %v", containerID, containerInfo.BotDir, err)
			r.logger.Info("Failed to remove bot directory", "container_id", containerID, "bot_dir", containerInfo.BotDir, "error", err.Error())
		} else {
			util.LogWorker("Successfully cleaned up bot directory - container_id: %s, bot_dir: %s", containerID, containerInfo.BotDir)
		}
	}

	util.LogWorker("Bot container stopped and cleaned up successfully - container_id: %s", containerID)
	return nil
}

func (r *dockerRunner) storeAnalysis(ctx context.Context, executable model.Executable, containerID string) ([]byte, error) {
	var resultFileContents []byte
	if resultBytes, err := r.CopyFromContainer(
		ctx,
		containerID,
		fmt.Sprintf("/%s/result.json", executable.Entrypoint),
	); err == nil {
		resultFileContents = resultBytes
		r.logger.Info("Successfully copied result file from the container %s", containerID)

		// Store analysis result if MinIO logger is available and we have result data
		if r.minioLogger != nil && len(resultFileContents) > 0 {
			// Parse the result JSON to store as analysis
			var analysisResult map[string]interface{}
			if err := json.Unmarshal(resultFileContents, &analysisResult); err == nil {
				if err := r.StoreAnalysisResult(ctx, executable.ID, analysisResult); err != nil {
					r.logger.Info("Failed to store analysis to MinIO", "error", err.Error())
					return nil, fmt.Errorf("failed to store analysis result: %v", err)
				} else {
					r.logger.Info("Successfully stored analysis to MinIO", "backtest_id", executable.ID)
					return resultFileContents, nil
				}
			} else {
				r.logger.Info("Failed to parse result JSON for analysis storage", "error", err.Error())
				return nil, fmt.Errorf("failed to parse result JSON: %v", err)
			}
		}
	} else {
		r.logger.Info("Failed to copy result file from workload %s on container %s", executable.ID, containerID)
		return nil, fmt.Errorf("failed to copy result file from container %s: %v", containerID, err)
	}
	r.logger.Info("No result file found in container %s for executable %s", containerID, executable.ID)
	return nil, fmt.Errorf("no result file found in container %s for executable %s", containerID, executable.ID)
}

// CopyFromContainer copies a file from the container and returns its contents
func (r *dockerRunner) CopyFromContainer(ctx context.Context, containerID, srcPath string) ([]byte, error) {
	reader, _, err := r.dockerClient.CopyFromContainer(ctx, containerID, srcPath)
	if err != nil {
		return nil, fmt.Errorf("failed to copy from container: %v", err)
	}
	defer reader.Close()

	// Read the tar archive
	tarReader := tar.NewReader(reader)

	// We expect a single file, so read the first entry
	_, err = tarReader.Next()
	if err != nil {
		return nil, fmt.Errorf("failed to read tar entry: %v", err)
	}

	// Read the file contents
	fileContents, err := io.ReadAll(tarReader)
	if err != nil {
		return nil, fmt.Errorf("failed to read file contents: %v", err)
	}

	return fileContents, nil
}

// GetContainerStatus returns the current status of a container
func (r *dockerRunner) GetContainerStatus(ctx context.Context, containerID string) (*ContainerStatus, error) {
	r.logger.Info("Getting container status", "container_id", containerID)
	if r.dockerClient == nil {
		return &ContainerStatus{
			ContainerID: containerID,
			Status:      "not_found",
			Error:       "docker client is not initialized",
		}, nil
	}

	inspect, err := r.dockerClient.ContainerInspect(ctx, containerID)
	r.logger.Info("Container inspect result", "container_id", containerID, "inspect", inspect)
	if err != nil {
		return &ContainerStatus{
			ContainerID: containerID,
			Status:      "not_found",
			Error:       err.Error(),
		}, nil
	}

	status := &ContainerStatus{
		ContainerID: containerID,
		StartedAt:   inspect.State.StartedAt,
		FinishedAt:  inspect.State.FinishedAt,
		Labels:      inspect.Config.Labels,
	}

	if inspect.State.Running {
		status.Status = "running"
	} else if inspect.State.Paused {
		status.Status = "paused"
	} else if inspect.State.Restarting {
		status.Status = "restarting"
	} else if inspect.State.Dead {
		status.Status = "dead"
	} else {
		status.Status = "exited"
		status.ExitCode = inspect.State.ExitCode
	}

	r.logger.Info("Container status retrieved", "container_id", containerID, "status", status.Status)

	return status, nil
}

// ListManagedContainers returns information about managed containers for the specified segment
func (r *dockerRunner) ListManagedContainers(ctx context.Context, segment int32) ([]*ContainerInfo, error) {
	// Query the docker daemon for containers with the "runtime.managed" label and specific segment
	labelFilter := filters.Arg("label", "runtime.managed=true")
	segmentFilter := filters.Arg("label", fmt.Sprintf("runtime.segment=%d", segment))
	containers, err := r.dockerClient.ContainerList(ctx, container.ListOptions{
		Filters: filters.NewArgs(labelFilter, segmentFilter),
	})
	if err != nil {
		return nil, fmt.Errorf("failed to list managed containers from docker: %v", err)
	}

	result := make([]*ContainerInfo, 0, len(containers))
	for _, container := range containers {
		botId := container.Labels["runtime.id"]
		if botId == "" {
			continue // Skip containers without bot ID label
		}
		entrypoint := container.Labels["runtime.entrypoint"]
		result = append(result, &ContainerInfo{
			ContainerID: container.ID,
			ID:          botId,
			Entrypoint:  entrypoint,
			Status:      container.State,
		})
	}
	return result, nil
}

// GetContainerLogs retrieves logs from a container
func (r *dockerRunner) GetContainerLogs(ctx context.Context, containerID string, tail int) (string, error) {
	logOptions := container.LogsOptions{
		ShowStdout: true,
		ShowStderr: true,
		Tail:       fmt.Sprintf("%d", tail),
		Follow:     false, // Don't follow, just get current logs
		Timestamps: false, // Don't include timestamps for cleaner output
	}

	logs, err := r.dockerClient.ContainerLogs(ctx, containerID, logOptions)
	if err != nil {
		return "", fmt.Errorf("failed to get container logs: %v", err)
	}
	defer logs.Close()

	var stdout, stderr bytes.Buffer
	_, err = stdcopy.StdCopy(&stdout, &stderr, logs)
	if err != nil {
		return "", fmt.Errorf("failed to demux container logs: %v", err)
	}

	// Combine stdout and stderr for the final output
	output := stdout.String() + stderr.String()
	return output, nil
}

// createLongRunningContainer creates and starts a container for long-running mode
func (r *dockerRunner) createLongRunningContainer(ctx context.Context, imageName, botDir string, executable model.Executable, entrypoint string) (*ExecutionResult, error) {
	// Determine shell based on image
	shell := "/bin/bash"
	if strings.Contains(imageName, "alpine") {
		shell = "/bin/sh"
	}

	// Create container config with labels for identification
	config := &container.Config{
		Image:      imageName,
		Cmd:        []string{shell, fmt.Sprintf("/%s/entrypoint.sh", executable.Entrypoint)},
		WorkingDir: "/tmp", // Start in /tmp to avoid getcwd issues, script will cd to entrypoint dir
		Env: []string{
			fmt.Sprintf("ID=%s", executable.ID),
			fmt.Sprintf("CONFIG=%s", func() string {
				configJSON, _ := json.Marshal(executable.Config)
				return string(configJSON)
			}()),
			fmt.Sprintf("ENTRYPOINT_TYPE=%s", entrypoint),
			fmt.Sprintf("CODE_MOUNT_DIR=%s", executable.Entrypoint),
			"PYTHONDONTWRITEBYTECODE=1", // Prevent Python from creating __pycache__ directories
		},
		Labels: map[string]string{
			"runtime.id":         executable.ID,
			"runtime.entrypoint": entrypoint,
			"runtime.managed":    "true",
			"runtime.segment":    fmt.Sprintf("%d", executable.Segment),
		},
		// Enable signal handling to allow graceful shutdown
		StopSignal: "SIGTERM",
	}

	volumeMount := fmt.Sprintf("%s:/%s", botDir, executable.Entrypoint)
	r.logger.Info("DOCKER_RUNNER: Configuring volume mount", "mount", volumeMount, "bot_dir", botDir, "entrypoint", executable.Entrypoint)

	hostConfig := &container.HostConfig{
		Binds: []string{
			volumeMount,
		},
		// Use host networking to bypass gvisor network isolation (this is if your running the bot in GKE (GKEWarden shiznit))
		// This automatically inherits the pod's DNS configuration
		NetworkMode: "host",
		AutoRemove:  true,
		Resources: container.Resources{
			Memory:    getMemoryLimit(),
			CPUShares: getCPUShares(),
		},
	}

	resp, err := r.dockerClient.ContainerCreate(ctx, config, hostConfig, nil, nil, "")
	if err != nil {
		return nil, fmt.Errorf("failed to create container: %v", err)
	}

	// Start container
	if err := r.dockerClient.ContainerStart(ctx, resp.ID, container.StartOptions{}); err != nil {
		return nil, fmt.Errorf("failed to start container: %v", err)
	}

	// Add to managed containers
	startTime := time.Now()
	containerInfo := &ContainerInfo{
		ContainerID: resp.ID,
		ID:          executable.ID,
		Entrypoint:  entrypoint,
		Status:      "running",
		StartedAt:   startTime.Format(time.RFC3339),
		Labels:      config.Labels,
		BotDir:      botDir, // Store for cleanup when container is stopped
		LastLogTime: startTime,
		LogsSince:   startTime.Format(time.RFC3339Nano), // Initialize for incremental log collection
	}

	r.containersMutex.Lock()
	r.managedContainers[resp.ID] = containerInfo
	r.containersMutex.Unlock()

	return &ExecutionResult{
		Status:        "running",
		Message:       "Long-running container started successfully",
		ContainerID:   resp.ID,
		IsLongRunning: true,
	}, nil
}

func (r *dockerRunner) createTerminatingContainer(ctx context.Context, imageName, directory string, executable model.Executable, entrypoint string) (*ExecutionResult, error) {
	// Add logging for debugging short-lived containers
	r.logger.Info("DOCKER_RUNNER: Creating terminating container", "bot_id", executable.ID, "directory", directory, "entrypoint", entrypoint)
	// Determine shell based on image
	shell := "/bin/bash"
	if strings.Contains(imageName, "alpine") {
		shell = "/bin/sh"
	}

	// Create container config with labels for identification
	config := &container.Config{
		Image:      imageName,
		Cmd:        []string{shell, fmt.Sprintf("/%s/entrypoint.sh", entrypoint)},
		WorkingDir: "/tmp", // Start in /tmp to avoid getcwd issues, script will cd to entrypoint dir
		Env: []string{
			fmt.Sprintf("ID=%s", executable.ID),
			fmt.Sprintf("CONFIG=%s", func() string {
				configJSON, _ := json.Marshal(executable.Config)
				return string(configJSON)
			}()),
			fmt.Sprintf("ENTRYPOINT_TYPE=%s", entrypoint),
			fmt.Sprintf("CODE_MOUNT_DIR=%s", executable.Entrypoint),
			"PYTHONDONTWRITEBYTECODE=1", // Prevent Python from creating __pycache__ directories
		},
		Labels: map[string]string{
			"runtime.id":         executable.ID,
			"runtime.entrypoint": entrypoint,
			"runtime.managed":    "true",
			"runtime.segment":    fmt.Sprintf("%d", executable.Segment),
		},
		// Enable signal handling to allow graceful shutdown
		StopSignal: "SIGTERM",
	}

	volumeMount := fmt.Sprintf("%s:/%s", directory, executable.Entrypoint)
	r.logger.Info("DOCKER_RUNNER: Configuring terminating container mount", "mount", volumeMount, "directory", directory, "entrypoint", executable.Entrypoint)

	hostConfig := &container.HostConfig{
		Binds: []string{
			volumeMount,
		},
		// Use host networking to bypass gvisor network isolation
		// This automatically inherits the pod's DNS configuration
		NetworkMode: "host",
		AutoRemove:  false, // We explictly set this to false as we need to grab the analysis results
		Resources: container.Resources{
			Memory:    getMemoryLimit(),
			CPUShares: getCPUShares(),
		},
	}

	resp, err := r.dockerClient.ContainerCreate(ctx, config, hostConfig, nil, nil, "")
	if err != nil {
		return nil, fmt.Errorf("failed to create container: %v", err)
	}

	// Start container
	if err := r.dockerClient.ContainerStart(ctx, resp.ID, container.StartOptions{}); err != nil {
		return nil, fmt.Errorf("failed to start container: %v", err)
	}

	r.logger.Info("Container started, waiting for completion: id %s container id %s", executable.ID, resp.ID)

	// Wait for container to complete
	statusCh, errCh := r.dockerClient.ContainerWait(ctx, resp.ID, container.WaitConditionNotRunning)

	select {
	case err := <-errCh:
		if err != nil {
			return &ExecutionResult{
				Status:   "error",
				Message:  "Container wait failed",
				Error:    err.Error(),
				ExitCode: 1,
			}, nil
		}
	case status := <-statusCh:
		r.logger.Info("Container completed", "container_id", resp.ID, "exit_code", status.StatusCode)

		// Get logs
		logs, err := r.GetContainerLogs(ctx, resp.ID, 1000)
		if err != nil {
			logs = fmt.Sprintf("Failed to get logs: %v", err)
		}

		// Log container output for debugging
		r.logger.Info("Container logs", "container_id", resp.ID, "exit_code", status.StatusCode, "logs", logs)

		// Store logs if MinIO logger is available
		if r.minioLogger != nil {
			if err := r.minioLogger.AppendBotLogs(ctx, executable.ID, logs); err != nil {
				r.logger.Info("Failed to store logs to MinIO", "error", err.Error())
			}
		}

		// Try to copy result file before cleanup
		var resultFileContents []byte
		if executable.PersistResults {
			r.logger.Info("Copying result file from container", "container_id", resp.ID)
			resultFileContents, err = r.storeAnalysis(ctx, executable, resp.ID)
			if err != nil {
				r.logger.Info("Warning: Failed to store analysis", "error", err.Error())
			}
		} else {
			r.logger.Info("Not persisting results, skipping result file copy")
			resultFileContents = nil // No result file to copy if not persisting results
		}

		// We can now safely remove the container
		if err := r.dockerClient.ContainerRemove(ctx, resp.ID, container.RemoveOptions{
			Force:         true,
			RemoveVolumes: true, // Remove any volumes created by the container
		}); err != nil {
			r.logger.Info("Warning: Failed to remove container", "container_id", resp.ID, "error", err.Error())
		} else {
			r.logger.Info("Container removed successfully", "container_id", resp.ID)
		}

		// Clean up backtest directory
		if err := fixDirectoryPermissions(directory); err != nil {
			r.logger.Info("Warning: Failed to fix directory permissions", "error", err.Error())
		}
		if err := os.RemoveAll(directory); err != nil {
			r.logger.Info("Warning: Failed to remove backtest directory", "error", err.Error())
		}

		if status.StatusCode == 0 {
			return &ExecutionResult{
				Status:             "success",
				Message:            "Backtest completed successfully",
				Output:             logs,
				ExitCode:           int(status.StatusCode),
				ResultFileContents: resultFileContents,
			}, nil
		} else {
			return &ExecutionResult{
				Status:             "error",
				Message:            fmt.Sprintf("Backtest failed with exit code %d", status.StatusCode),
				Output:             logs,
				Error:              fmt.Sprintf("Container exited with code %d", status.StatusCode),
				ExitCode:           int(status.StatusCode),
				ResultFileContents: resultFileContents,
			}, nil
		}
	case <-ctx.Done():
		return &ExecutionResult{
			Status:   "error",
			Message:  "Backtest timed out",
			Error:    "Context timeout or cancellation",
			ExitCode: 124, // Timeout exit code
		}, nil
	}

	return &ExecutionResult{
		Status:   "error",
		Message:  "Unexpected execution path",
		Error:    "Should not reach here",
		ExitCode: 1,
	}, nil
}

// startLogCollector runs in the background to periodically collect logs from long-running containers
func (r *dockerRunner) startLogCollector() {
	defer close(r.logCollectorDone)

	// Get log collection interval from environment variable, default to 30 seconds
	logInterval := 30 * time.Second
	if intervalStr := os.Getenv("BOT_LOG_COLLECTION_INTERVAL_SECONDS"); intervalStr != "" {
		if interval, err := strconv.Atoi(intervalStr); err == nil && interval > 0 {
			logInterval = time.Duration(interval) * time.Second
		}
	}

	r.logger.Info("Starting log collector", "interval", logInterval)
	ticker := time.NewTicker(logInterval)
	defer ticker.Stop()

	for {
		select {
		case <-r.logCollectorStop:
			r.logger.Info("Log collector stopped")
			return
		case <-ticker.C:
			r.collectLogsFromRunningContainers()
		}
	}
}

// collectLogsFromRunningContainers collects incremental logs from all managed running containers
func (r *dockerRunner) collectLogsFromRunningContainers() {
	r.containersMutex.RLock()
	containers := make([]*ContainerInfo, 0, len(r.managedContainers))
	for _, containerInfo := range r.managedContainers {
		// Create a copy to avoid race conditions
		info := *containerInfo
		containers = append(containers, &info)
	}
	r.containersMutex.RUnlock()

	for _, containerInfo := range containers {
		err := r.collectContainerLogs(containerInfo)

		if err != nil {
			r.logger.Info("Failed to collect logs from container after retries",
				"container_id", containerInfo.ContainerID,
				"id", containerInfo.ID,
				"error", err.Error())
		}
	}
}

// collectContainerLogs collects incremental logs from a specific container and saves to MinIO
func (r *dockerRunner) collectContainerLogs(containerInfo *ContainerInfo) error {
	r.IncrementLogCollectionAttempt()
	start := time.Now()

	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()

	// Check if container is still running
	status, err := r.GetContainerStatus(ctx, containerInfo.ContainerID)
	if err != nil {
		r.IncrementLogCollectionFailure()
		return fmt.Errorf("failed to get container status: %v", err)
	}

	// If container is not running, stop log collection immediately
	if status.Status != "running" {
		r.logger.Info("Container no longer running, stopping log collection",
			"container_id", containerInfo.ContainerID,
			"status", status.Status)
		return nil
	}

	// Prepare log options for incremental collection
	logOptions := container.LogsOptions{
		ShowStdout: true,
		ShowStderr: true,
		Timestamps: true,
	}

	// Use since timestamp for incremental collection
	if containerInfo.LogsSince != "" {
		logOptions.Since = containerInfo.LogsSince
	} else {
		// For first collection, get logs since container start
		logOptions.Since = containerInfo.StartedAt
	}

	logs, err := r.dockerClient.ContainerLogs(ctx, containerInfo.ContainerID, logOptions)
	if err != nil {
		r.IncrementLogCollectionFailure()
		return fmt.Errorf("failed to get container logs: %v", err)
	}
	defer logs.Close()

	// Use the Docker SDK's demuxer instead of manual parsing
	var stdout, stderr bytes.Buffer
	_, err = stdcopy.StdCopy(&stdout, &stderr, logs)
	if err != nil {
		r.IncrementLogCollectionFailure()
		return fmt.Errorf("failed to demux container logs: %v", err)
	}

	// Combine stdout and stderr for the final output
	output := stdout.String() + stderr.String()

	if output == "" {
		// No new logs
		return nil
	}
	// Save logs to MinIO
	r.IncrementMinIOWriteAttempt()
	if err := r.minioLogger.AppendBotLogs(ctx, containerInfo.ID, output); err != nil {
		r.IncrementMinIOWriteFailure()
		r.IncrementLogCollectionFailure()
		return fmt.Errorf("failed to save logs to MinIO: %v", err)
	}
	r.IncrementMinIOWriteSuccess()

	// Update last log collection time
	currentTime := time.Now()
	r.containersMutex.Lock()
	if managedContainer, exists := r.managedContainers[containerInfo.ContainerID]; exists {
		managedContainer.LastLogTime = currentTime
		managedContainer.LogsSince = currentTime.Format(time.RFC3339Nano)
	}
	r.containersMutex.Unlock()

	r.logger.Info("Collected and saved logs to MinIO",
		"container_id", containerInfo.ContainerID,
		"id", containerInfo.ID,
		"log_size", len(output))

	// Record successful collection
	r.IncrementLogCollectionSuccess()
	r.UpdateAverageLogCollectionDuration(time.Since(start))

	return nil
}

// StoreAnalysisResult stores the analysis result as JSON at {bot_id}/analysis.json in the bots bucket
func (r *dockerRunner) StoreAnalysisResult(ctx context.Context, botID string, result map[string]interface{}) error {
	// Generate file path: {bot_id}/analysis.json (bucket is already 'bots')
	objectName := fmt.Sprintf("%s/analysis.json", botID)

	// Convert result to JSON first (this can fail with invalid data)
	jsonData, err := json.Marshal(result)
	if err != nil {
		return fmt.Errorf("failed to marshal analysis result: %v", err)
	}

	if r.minioClient == nil {
		return fmt.Errorf("MinIO client is not initialized")
	}

	// Write JSON to MinIO
	reader := bytes.NewReader(jsonData)
	_, err = r.minioClient.PutObject(ctx, r.resultsBucket, objectName, reader, int64(len(jsonData)), minio.PutObjectOptions{
		ContentType: "application/json",
	})
	if err != nil {
		return fmt.Errorf("failed to write analysis result to MinIO: %v", err)
	}

	return nil
}

// --- Metrics Methods ---

// IncrementLogCollectionAttempt increments the log collection attempt counter
func (r *dockerRunner) IncrementLogCollectionAttempt() {
	r.metrics.mutex.Lock()
	defer r.metrics.mutex.Unlock()
	r.metrics.LogCollectionAttempts++
}

// IncrementLogCollectionSuccess increments the log collection success counter
func (r *dockerRunner) IncrementLogCollectionSuccess() {
	r.metrics.mutex.Lock()
	defer r.metrics.mutex.Unlock()
	r.metrics.LogCollectionSuccesses++
	r.metrics.LastLogCollectionTime = time.Now()
}

// IncrementLogCollectionFailure increments the log collection failure counter
func (r *dockerRunner) IncrementLogCollectionFailure() {
	r.metrics.mutex.Lock()
	defer r.metrics.mutex.Unlock()
	r.metrics.LogCollectionFailures++
}

// IncrementLogStreamConnection increments the log stream connection counter
func (r *dockerRunner) IncrementLogStreamConnection() {
	r.metrics.mutex.Lock()
	defer r.metrics.mutex.Unlock()
	r.metrics.LogStreamConnections++
}

// IncrementLogStreamReconnection increments the log stream reconnection counter
func (r *dockerRunner) IncrementLogStreamReconnection() {
	r.metrics.mutex.Lock()
	defer r.metrics.mutex.Unlock()
	r.metrics.LogStreamReconnections++
}

// IncrementLogStreamFailure increments the log stream failure counter
func (r *dockerRunner) IncrementLogStreamFailure() {
	r.metrics.mutex.Lock()
	defer r.metrics.mutex.Unlock()
	r.metrics.LogStreamFailures++
}

// IncrementMinIOWriteAttempt increments the MinIO write attempt counter
func (r *dockerRunner) IncrementMinIOWriteAttempt() {
	r.metrics.mutex.Lock()
	defer r.metrics.mutex.Unlock()
	r.metrics.MinIOWriteAttempts++
}

// IncrementMinIOWriteSuccess increments the MinIO write success counter
func (r *dockerRunner) IncrementMinIOWriteSuccess() {
	r.metrics.mutex.Lock()
	defer r.metrics.mutex.Unlock()
	r.metrics.MinIOWriteSuccesses++
}

// IncrementMinIOWriteFailure increments the MinIO write failure counter
func (r *dockerRunner) IncrementMinIOWriteFailure() {
	r.metrics.mutex.Lock()
	defer r.metrics.mutex.Unlock()
	r.metrics.MinIOWriteFailures++
}

// UpdateContainersTracked updates the currently tracked containers count
func (r *dockerRunner) UpdateContainersTracked(count int64) {
	r.metrics.mutex.Lock()
	defer r.metrics.mutex.Unlock()
	r.metrics.ContainersTracked = count
}

// IncrementContainersReconciled increments the containers reconciled counter
func (r *dockerRunner) IncrementContainersReconciled() {
	r.metrics.mutex.Lock()
	defer r.metrics.mutex.Unlock()
	r.metrics.ContainersReconciled++
	r.metrics.LastReconciliationTime = time.Now()
}

// IncrementContainersCleaned increments the containers cleaned counter
func (r *dockerRunner) IncrementContainersCleaned() {
	r.metrics.mutex.Lock()
	defer r.metrics.mutex.Unlock()
	r.metrics.ContainersCleaned++
}

// UpdateAverageLogCollectionDuration updates the average log collection duration
func (r *dockerRunner) UpdateAverageLogCollectionDuration(duration time.Duration) {
	r.metrics.mutex.Lock()
	defer r.metrics.mutex.Unlock()

	// Simple moving average (could be enhanced with more sophisticated calculation)
	if r.metrics.AverageLogCollectionDuration == 0 {
		r.metrics.AverageLogCollectionDuration = duration
	} else {
		r.metrics.AverageLogCollectionDuration = (r.metrics.AverageLogCollectionDuration + duration) / 2
	}
}

// GetMetrics returns a copy of current metrics
func (r *dockerRunner) GetMetrics() LogCaptureMetrics {
	r.metrics.mutex.RLock()
	defer r.metrics.mutex.RUnlock()

	// Return a copy without the mutex to avoid lock value copying
	return LogCaptureMetrics{
		LogCollectionAttempts:        r.metrics.LogCollectionAttempts,
		LogCollectionSuccesses:       r.metrics.LogCollectionSuccesses,
		LogCollectionFailures:        r.metrics.LogCollectionFailures,
		LogStreamConnections:         r.metrics.LogStreamConnections,
		LogStreamReconnections:       r.metrics.LogStreamReconnections,
		LogStreamFailures:            r.metrics.LogStreamFailures,
		MinIOWriteAttempts:           r.metrics.MinIOWriteAttempts,
		MinIOWriteSuccesses:          r.metrics.MinIOWriteSuccesses,
		MinIOWriteFailures:           r.metrics.MinIOWriteFailures,
		ContainersTracked:            r.metrics.ContainersTracked,
		ContainersReconciled:         r.metrics.ContainersReconciled,
		ContainersCleaned:            r.metrics.ContainersCleaned,
		LastLogCollectionTime:        r.metrics.LastLogCollectionTime,
		LastReconciliationTime:       r.metrics.LastReconciliationTime,
		AverageLogCollectionDuration: r.metrics.AverageLogCollectionDuration,
		// Note: mutex is intentionally excluded from the copy
	}
}

// LogMetricsSummary logs a summary of current metrics
func (r *dockerRunner) LogMetricsSummary() {
	metrics := r.GetMetrics()

	r.logger.Info("Log Capture Metrics Summary",
		"log_collection_attempts", metrics.LogCollectionAttempts,
		"log_collection_successes", metrics.LogCollectionSuccesses,
		"log_collection_failures", metrics.LogCollectionFailures,
		"log_stream_connections", metrics.LogStreamConnections,
		"log_stream_reconnections", metrics.LogStreamReconnections,
		"log_stream_failures", metrics.LogStreamFailures,
		"minio_write_attempts", metrics.MinIOWriteAttempts,
		"minio_write_successes", metrics.MinIOWriteSuccesses,
		"minio_write_failures", metrics.MinIOWriteFailures,
		"containers_tracked", metrics.ContainersTracked,
		"containers_reconciled", metrics.ContainersReconciled,
		"containers_cleaned", metrics.ContainersCleaned,
		"average_log_collection_duration", metrics.AverageLogCollectionDuration,
		"last_log_collection", metrics.LastLogCollectionTime,
		"last_reconciliation", metrics.LastReconciliationTime,
	)
}
