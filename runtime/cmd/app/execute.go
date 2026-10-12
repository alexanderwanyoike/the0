package main

import (
	"context"
	"fmt"
	"io"
	"os"
	"os/exec"
	"os/signal"
	"path/filepath"
	"syscall"
	"time"

	"github.com/spf13/cobra"

	"runtime/internal/constants"
	"runtime/internal/daemon"
	"runtime/internal/execute"
	"runtime/internal/launch"
	"runtime/internal/query"
	"runtime/internal/util"
)

var executeCmd = &cobra.Command{
	Use:   "execute",
	Short: "Execute a bot inside a container",
	Long: `Unified bot execution command for Docker and K8s containers.
Handles code download, state management, sync, query server, and bot execution.

Environment Variables (required):
  BOT_ID          - Unique bot identifier
  CODE_FILE       - MinIO path to code.zip
  RUNTIME         - Runtime name (python3.11, nodejs22, dotnet8, etc.)
  ENTRYPOINT      - Bot entrypoint file (e.g., main.py)
  BOT_CONFIG      - JSON configuration for the bot

Environment Variables (optional):
  QUERY_ENTRYPOINT - Query entrypoint for realtime bots
  BOT_TYPE         - "realtime" or "scheduled"
  QUERY_PARAMS     - JSON params for query mode
  IS_SCHEDULED     - "true" for scheduled bots

Execution Modes:
  runtime execute                          - Docker mode: spawns sync + query server subprocesses
  runtime execute --skip-sync              - K8s mode: skip sync (runs as sidecar)
  runtime execute --skip-query-server      - K8s mode: skip query server (runs as sidecar)
  runtime execute --query /status          - Ephemeral query execution
  runtime execute --query-server-only      - K8s query sidecar mode`,
	Run: func(cmd *cobra.Command, args []string) {
		os.Exit(runExecute())
	},
}

var executeFlags *launch.Flags

func init() {
	executeFlags = launch.BindFlags(executeCmd.Flags())
	rootCmd.AddCommand(executeCmd)
}

// loadExecuteConfig reads the launch this container was started with.
func loadExecuteConfig() (*launch.Settings, error) {
	cfg, err := launch.Read(executeFlags, os.LookupEnv)
	if err != nil {
		return nil, err
	}
	return &cfg, nil
}

// runExecute is the main entry point for bot execution.
func runExecute() int {
	logger := &util.DefaultLogger{}
	logger.Info("Execute command starting...")

	cfg, err := loadExecuteConfig()
	if err != nil {
		logger.Info("Configuration error: %v", err)
		return 1
	}

	logger.Info("Bot ID: %s, Runtime: %s, Entrypoint: %s", cfg.BotID, cfg.Runtime, cfg.Entrypoint)

	switch cfg.Mode {
	case launch.Server:
		return runQueryServerOnly(cfg, logger)
	case launch.Query:
		return runEphemeralQuery(cfg, logger)
	default:
		return runBot(cfg, logger)
	}
}

// runBot executes a bot with optional sync and query server subprocesses.
func runBot(cfg *launch.Settings, logger *util.DefaultLogger) int {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Setup signal handling
	sigCh := make(chan os.Signal, 1)
	signal.Notify(sigCh, syscall.SIGTERM, syscall.SIGINT)

	// Track child processes and sync runner for cleanup
	var queryCmd *exec.Cmd
	var syncRunner *daemon.SyncRunner

	// Cleanup function
	cleanup := func() {
		logger.Info("Cleaning up...")
		// Stop sync runner (performs final sync)
		if syncRunner != nil {
			syncRunner.Stop()
		}
		// Send SIGTERM to query server for graceful shutdown
		if queryCmd != nil && queryCmd.Process != nil {
			if err := queryCmd.Process.Signal(syscall.SIGTERM); err != nil {
				if err.Error() != "os: process already finished" {
					logger.Info("Failed to send SIGTERM to query process: %v", err)
				}
			}
			// Give process time to clean up
			time.Sleep(1 * time.Second)
			// Force kill if still running
			if err := queryCmd.Process.Kill(); err != nil {
				if err.Error() != "os: process already finished" {
					logger.Info("Failed to kill query process: %v", err)
				}
			}
		}
	}

	// Handle signals
	go func() {
		sig := <-sigCh
		logger.Info("Received signal: %v", sig)
		cleanup()
		cancel()
	}()

	// Step 1: Download code
	if err := downloadCode(ctx, cfg, logger); err != nil {
		logger.Info("Failed to download code: %v", err)
		return 1
	}

	// Step 1.5: Ensure binaries are executable for compiled runtimes
	if err := execute.EnsureExecutable(cfg); err != nil {
		logger.Info("Warning: failed to set execute permissions: %v", err)
		// Continue anyway - may work if permissions are already set
	}

	// Step 2: Download state
	if err := downloadState(ctx, cfg, logger); err != nil {
		// State may not exist on first run - log but continue
		logger.Info("No existing state or download failed: %v", err)
	}

	// Step 2.5: Ensure logs directory exists
	if err := os.MkdirAll(cfg.LogsPath, 0755); err != nil {
		logger.Info("Failed to create logs directory: %v", err)
		// Continue anyway - logs may not work but bot should run
	}

	// Step 3: Start sync runner in-process (unless skipped for K8s)
	if !cfg.SkipSync {
		syncRunner = daemon.NewSyncRunner(daemon.SyncOptions{
			BotID:     cfg.BotID,
			StatePath: cfg.StatePath,
			LogsPath:  cfg.LogsPath,
			WatchDone: func() string {
				if cfg.IsScheduled {
					return constants.DefaultDoneFilePath
				}
				return ""
			}(),
		}, logger)
		syncRunner.Start()
		// Wait for sync runner to initialize (with timeout)
		if err := syncRunner.WaitReady(10 * time.Second); err != nil {
			logger.Info("Sync runner failed to initialize: %v", err)
			// Continue anyway - bot should run even if sync fails
		}
		defer syncRunner.Stop()
	} else {
		logger.Info("Skipping sync (--skip-sync flag set)")
	}

	// Step 4: Start query server subprocess (for realtime bots, unless skipped)
	if cfg.BotType == "realtime" && cfg.QueryEntrypoint != "" && !cfg.SkipQueryServer {
		var err error
		queryCmd, err = startQueryServerProcess(cfg, logger)
		if err != nil {
			logger.Info("Failed to start query server: %v", err)
			// Query server failure shouldn't prevent bot from running
		}
		defer func() {
			if queryCmd != nil && queryCmd.Process != nil {
				queryCmd.Process.Signal(syscall.SIGTERM)
				queryCmd.Wait()
			}
		}()
	} else if cfg.SkipQueryServer && cfg.BotType == "realtime" && cfg.QueryEntrypoint != "" {
		logger.Info("Skipping query server subprocess (--skip-query-server flag set)")
	}

	// Step 5: Execute the bot (blocks)
	exitCode := executeBotProcess(ctx, cfg, logger)

	// Step 6: Write done file for scheduled bots
	if cfg.IsScheduled {
		doneFile := constants.DefaultDoneFilePath
		if err := os.WriteFile(doneFile, []byte(fmt.Sprintf("%d", exitCode)), 0644); err != nil {
			logger.Info("Failed to write done file: %v", err)
		}
	}

	// Step 7: Cleanup (sync runner performs final sync)
	cleanup()

	return exitCode
}

// ephemeralQueryEntrypoint returns the file a query runs, which is only ever
// the query entrypoint (see query.ErrNoQueryEntrypoint).
func ephemeralQueryEntrypoint(cfg *launch.Settings) (string, error) {
	if cfg.QueryEntrypoint == "" {
		return "", fmt.Errorf("QUERY_ENTRYPOINT is not set: %w", query.ErrNoQueryEntrypoint)
	}
	return cfg.QueryEntrypoint, nil
}

// runEphemeralQuery runs a single query and exits.
// Queries run directly without wrappers - the SDK's query.run() handles everything.
func runEphemeralQuery(cfg *launch.Settings, logger *util.DefaultLogger) int {
	ctx := context.Background()

	logger.Info("Running ephemeral query: %s", cfg.QueryPath)

	entrypoint, err := ephemeralQueryEntrypoint(cfg)
	if err != nil {
		logger.Info("Refusing query: %v", err)
		return 1
	}

	// Download code
	if err := downloadCode(ctx, cfg, logger); err != nil {
		logger.Info("Failed to download code: %v", err)
		return 1
	}

	// Download state (read-only for queries)
	if err := downloadState(ctx, cfg, logger); err != nil {
		logger.Info("No existing state: %v", err)
	}

	// Create query result directory
	if err := os.MkdirAll("/query", 0755); err != nil {
		logger.Info("Failed to create query directory: %v", err)
		return 1
	}

	exitCode := executeQueryProcess(ctx, cfg, entrypoint, logger)
	logger.Info("Query process completed with exit code: %d", exitCode)

	// If QUERY_RESULT_KEY is set, upload the result for the query server to retrieve.
	if cfg.QueryResultKey != "" {
		logger.Info("Uploading query result to MinIO key: %s", cfg.QueryResultKey)
		if err := uploadQueryResult(ctx, cfg, logger); err != nil {
			logger.Info("Failed to upload query result: %v", err)
			return 1
		}
	}

	return exitCode
}

// uploadQueryResult uploads the query result file to MinIO.
func uploadQueryResult(ctx context.Context, cfg *launch.Settings, logger *util.DefaultLogger) error {
	storage := execute.NewStorageOperations(logger)
	return storage.UploadQueryResult(ctx, cfg.QueryResultKey, "/query/result.json")
}

// runQueryServerOnly runs only the query server (for K8s sidecar mode).
// Query servers run directly without wrappers - the SDK handles everything.
func runQueryServerOnly(cfg *launch.Settings, logger *util.DefaultLogger) int {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	logger.Info("Running in query-server-only mode")

	// Setup signal handling
	sigCh := make(chan os.Signal, 1)
	signal.Notify(sigCh, syscall.SIGTERM, syscall.SIGINT)

	go func() {
		sig := <-sigCh
		logger.Info("Received signal: %v", sig)
		cancel()
	}()

	// Get code: wait for shared volume or download
	if err := ensureCode(ctx, cfg, logger); err != nil {
		logger.Info("Failed to get code: %v", err)
		return 1
	}

	// Download state
	if err := downloadState(ctx, cfg, logger); err != nil {
		logger.Info("No existing state: %v", err)
	}

	// Run the query server as the main process (no wrapper needed)
	if cfg.QueryEntrypoint == "" {
		logger.Info("QUERY_ENTRYPOINT not set, cannot run query server")
		return 1
	}

	return executeQueryProcess(ctx, cfg, cfg.QueryEntrypoint, logger)
}

// codeWaitTimeout is how long to wait for code on a shared volume before falling back to download.
const codeWaitTimeout = 60 * time.Second

// ensureCode waits for code to appear on a shared volume (K8s sidecar mode) or downloads it.
// In K8s, the bot container extracts code to a shared volume. The query-server waits for
// the entrypoint file to appear rather than re-extracting (which causes "text file busy").
func ensureCode(ctx context.Context, cfg *launch.Settings, logger *util.DefaultLogger) error {
	// If we have a query entrypoint, wait for it to appear on the shared volume
	if cfg.QueryEntrypoint != "" {
		entrypointPath := filepath.Join(cfg.CodePath, cfg.QueryEntrypoint)
		deadline := time.Now().Add(codeWaitTimeout)

		for time.Now().Before(deadline) {
			if _, err := os.Stat(entrypointPath); err == nil {
				logger.Info("Code available at %s", cfg.CodePath)
				return nil
			}
			select {
			case <-ctx.Done():
				return ctx.Err()
			case <-time.After(1 * time.Second):
			}
		}
		logger.Info("Code not found after %v, downloading", codeWaitTimeout)
	}

	return downloadCode(ctx, cfg, logger)
}

// downloadCode downloads and extracts bot code from MinIO.
func downloadCode(ctx context.Context, cfg *launch.Settings, logger *util.DefaultLogger) error {
	storage := execute.NewStorageOperations(logger)
	return storage.DownloadCode(ctx, cfg.CodeFile, cfg.CodePath)
}

// downloadState downloads bot state from MinIO.
func downloadState(ctx context.Context, cfg *launch.Settings, logger *util.DefaultLogger) error {
	storage := execute.NewStorageOperations(logger)
	return storage.DownloadState(ctx, cfg.BotID, cfg.StatePath)
}

// startQueryServerProcess starts the query server as a subprocess.
// Query servers run directly without wrappers - the SDK handles everything.
func startQueryServerProcess(cfg *launch.Settings, logger *util.DefaultLogger) (*exec.Cmd, error) {
	cmd := execute.BuildQueryCommand(cfg.Runtime, cfg.QueryEntrypoint, cfg.CodePath)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	cmd.Env = execute.BuildBotEnv(cfg)

	logger.Info("Starting query server subprocess: %s %s", cfg.Runtime, cfg.QueryEntrypoint)
	if err := cmd.Start(); err != nil {
		return nil, fmt.Errorf("failed to start query server: %w", err)
	}

	logger.Info("Query server subprocess started (PID: %d)", cmd.Process.Pid)
	return cmd, nil
}

// executeBotProcess executes the main bot process.
func executeBotProcess(ctx context.Context, cfg *launch.Settings, logger *util.DefaultLogger) int {
	// Set up log file for persistence (in addition to stdout/stderr)
	logFile := setupLogFile(cfg.LogsPath, logger)
	if logFile != nil {
		defer logFile.Close()
	}

	return executeProcessWithLogFile(ctx, cfg, cfg.Entrypoint, logger, logFile)
}

// setupLogFile creates a log file for bot output persistence.
func setupLogFile(logsPath string, logger *util.DefaultLogger) *os.File {
	return setupLogFileWithDeps(logsPath, logger, execute.NewDefaultDependencies())
}

// setupLogFileWithDeps creates a log file using injectable dependencies.
func setupLogFileWithDeps(logsPath string, logger *util.DefaultLogger, deps execute.Dependencies) *os.File {
	logFilePath := filepath.Join(logsPath, "bot.log")
	file, err := deps.FileWriter.Create(logFilePath)
	if err != nil {
		logger.Info("Failed to create log file: %v", err)
		return nil
	}
	// Type assert to *os.File for compatibility
	if osFile, ok := file.(*os.File); ok {
		return osFile
	}
	logger.Info("FileWriter.Create() did not return *os.File")
	return nil
}

// executeProcessWithLogFile executes a process, optionally writing output to a log file.
func executeProcessWithLogFile(ctx context.Context, cfg *launch.Settings, entrypoint string, logger *util.DefaultLogger, logFile *os.File) int {
	return executeProcessWithDeps(ctx, cfg, entrypoint, logger, logFile, execute.NewDefaultDependencies())
}

// executeProcessWithDeps executes a process with injectable dependencies for testing.
func executeProcessWithDeps(ctx context.Context, cfg *launch.Settings, entrypoint string, logger *util.DefaultLogger, logFile *os.File, deps execute.Dependencies) int {
	cmd := execute.BuildBotCommand(cfg.Runtime, entrypoint, cfg.CodePath)

	// Set up output: both stdout and optionally log file
	var stderr io.Writer = os.Stderr
	if logFile != nil {
		cmd.Stdout = io.MultiWriter(os.Stdout, logFile)
		stderr = io.MultiWriter(os.Stderr, logFile)
	} else {
		cmd.Stdout = os.Stdout
	}
	cmd.Stderr = stderr
	cmd.Env = execute.BuildBotEnv(cfg)

	// A result.json shipped in the code, or left by an earlier container in the
	// same pod (an emptyDir outlives a container restart), must not decide this run.
	resultPath := execute.BotResultPath(cfg.CodePath)
	if err := execute.ClearBotResult(resultPath); err != nil {
		logger.Info("Failed to remove stale result file: %v", err)
	}

	logger.Info("Executing: %s (runtime: %s, entrypoint: %s)", cmd.Path, cfg.Runtime, entrypoint)

	// Run with context for cancellation support using injected dependency
	if err := deps.ProcessExecutor.Start(cmd); err != nil {
		logger.Info("Failed to start process: %v", err)
		// Write error to log file so it persists to MinIO
		if logFile != nil {
			logFile.WriteString(fmt.Sprintf(`{"level":"error","message":"Failed to start process: %v","timestamp":"%s"}`+"\n",
				err, time.Now().UTC().Format(time.RFC3339)))
		}
		return 1
	}

	// Wait for process or context cancellation
	done := make(chan error, 1)
	go func() {
		done <- deps.ProcessExecutor.Wait(cmd)
	}()

	select {
	case <-ctx.Done():
		logger.Info("Context cancelled, terminating process")
		deps.ProcessExecutor.Kill(cmd)
		return 1
	case err := <-done:
		exitCode := 0
		if err != nil {
			if exitErr, ok := err.(*exec.ExitError); ok {
				exitCode = exitErr.ExitCode()
			} else {
				logger.Info("Process error: %v", err)
				exitCode = 1
			}
		}
		outcome := execute.SettleRun(exitCode, resultPath)
		if outcome.Failure != "" {
			if _, err := stderr.Write(execute.RunFailedLine(outcome.Failure, time.Now())); err != nil {
				logger.Info("Failed to write the run summary: %v", err)
			}
		}
		return outcome.ExitCode
	}
}

// executeQueryProcess executes a query process directly (no wrapper).
// Queries use the SDK's query.run() which handles everything internally.
func executeQueryProcess(ctx context.Context, cfg *launch.Settings, entrypoint string, logger *util.DefaultLogger) int {
	return executeQueryProcessWithDeps(ctx, cfg, entrypoint, logger, execute.NewDefaultDependencies())
}

// executeQueryProcessWithDeps executes a query process with injectable dependencies.
func executeQueryProcessWithDeps(ctx context.Context, cfg *launch.Settings, entrypoint string, logger *util.DefaultLogger, deps execute.Dependencies) int {
	cmd := execute.BuildQueryCommand(cfg.Runtime, entrypoint, cfg.CodePath)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	cmd.Env = execute.BuildBotEnv(cfg)

	logger.Info("Executing query: %s (runtime: %s, entrypoint: %s)", cmd.Path, cfg.Runtime, entrypoint)

	// Run with context for cancellation support using injected dependency
	if err := deps.ProcessExecutor.Start(cmd); err != nil {
		logger.Info("Failed to start query process: %v", err)
		return 1
	}

	// Wait for process or context cancellation
	done := make(chan error, 1)
	go func() {
		done <- deps.ProcessExecutor.Wait(cmd)
	}()

	select {
	case <-ctx.Done():
		logger.Info("Context cancelled, terminating query process")
		deps.ProcessExecutor.Kill(cmd)
		return 1
	case err := <-done:
		if err != nil {
			if exitErr, ok := err.(*exec.ExitError); ok {
				return exitErr.ExitCode()
			}
			logger.Info("Query process error: %v", err)
			return 1
		}
		return 0
	}
}
