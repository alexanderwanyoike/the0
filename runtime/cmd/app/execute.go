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
	"runtime/internal/execute"
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
  RUNTIME         - Runtime name (python3.11, nodejs20, dotnet8, etc.)
  ENTRYPOINT      - Bot entrypoint file (e.g., main.py)
  BOT_CONFIG      - JSON configuration for the bot

Environment Variables (optional):
  QUERY_ENTRYPOINT - Query entrypoint for realtime bots
  BOT_TYPE         - "realtime" or "scheduled"
  QUERY_PATH       - If set, run in ephemeral query mode
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

var (
	executeQueryPath       string
	executeQueryServerOnly bool
	executeSkipSync        bool
	executeSkipQueryServer bool
)

func init() {
	executeCmd.Flags().StringVar(&executeQueryPath, "query", "", "Execute a query (ephemeral mode) with the given path")
	executeCmd.Flags().BoolVar(&executeQueryServerOnly, "query-server-only", false, "Run only the query server (for K8s sidecar mode)")
	executeCmd.Flags().BoolVar(&executeSkipSync, "skip-sync", false, "Skip starting sync subprocess (for K8s where sync is a sidecar)")
	executeCmd.Flags().BoolVar(&executeSkipQueryServer, "skip-query-server", false, "Skip starting query server subprocess (for K8s where query is a sidecar)")
	rootCmd.AddCommand(executeCmd)
}

// loadExecuteConfig loads configuration from environment variables.
func loadExecuteConfig() (*execute.Config, error) {
	cfg := &execute.Config{
		BotID:           os.Getenv("BOT_ID"),
		CodeFile:        os.Getenv("CODE_FILE"),
		Runtime:         os.Getenv("RUNTIME"),
		Entrypoint:      os.Getenv("ENTRYPOINT"),
		BotConfig:       os.Getenv("BOT_CONFIG"),
		BotType:         os.Getenv("BOT_TYPE"),
		QueryEntrypoint: os.Getenv("QUERY_ENTRYPOINT"),
		QueryPath:       os.Getenv("QUERY_PATH"),
		QueryParams:     os.Getenv("QUERY_PARAMS"),
		QueryResultKey:  os.Getenv("QUERY_RESULT_KEY"),
		CodePath:        getEnv("CODE_PATH", constants.DefaultCodePath),
		StatePath:       getEnv("STATE_PATH", constants.DefaultStatePath),
		LogsPath:        getEnv("LOGS_PATH", constants.DefaultLogsPath),
		IsScheduled:     os.Getenv("IS_SCHEDULED") == "true",
	}

	// Override query path from CLI flag if provided
	if executeQueryPath != "" {
		cfg.QueryPath = executeQueryPath
	}

	// Validate required fields
	if cfg.BotID == "" {
		return nil, fmt.Errorf("BOT_ID environment variable is required")
	}
	if cfg.Runtime == "" {
		return nil, fmt.Errorf("RUNTIME environment variable is required")
	}
	if cfg.Entrypoint == "" {
		return nil, fmt.Errorf("ENTRYPOINT environment variable is required")
	}

	return cfg, nil
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

	// Mode: Query server only (K8s sidecar)
	if executeQueryServerOnly {
		return runQueryServerOnly(cfg, logger)
	}

	// Mode: Ephemeral query execution
	if cfg.QueryPath != "" {
		return runEphemeralQuery(cfg, logger)
	}

	// Mode: Normal bot execution
	return runBot(cfg, logger)
}

// runBot executes a bot with optional sync and query server subprocesses.
func runBot(cfg *execute.Config, logger *util.DefaultLogger) int {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Setup signal handling
	sigCh := make(chan os.Signal, 1)
	signal.Notify(sigCh, syscall.SIGTERM, syscall.SIGINT)

	// Track child processes for cleanup
	var syncCmd, queryCmd *exec.Cmd

	// Cleanup function
	cleanup := func() {
		logger.Info("Cleaning up child processes...")
		// Send SIGTERM first for graceful shutdown
		if syncCmd != nil && syncCmd.Process != nil {
			if err := syncCmd.Process.Signal(syscall.SIGTERM); err != nil {
				// "process already finished" is expected if it exited naturally
				if err.Error() != "os: process already finished" {
					logger.Info("Failed to send SIGTERM to sync process: %v", err)
				}
			}
		}
		if queryCmd != nil && queryCmd.Process != nil {
			if err := queryCmd.Process.Signal(syscall.SIGTERM); err != nil {
				if err.Error() != "os: process already finished" {
					logger.Info("Failed to send SIGTERM to query process: %v", err)
				}
			}
		}
		// Give processes time to clean up
		time.Sleep(3 * time.Second)
		// Force kill if still running
		if syncCmd != nil && syncCmd.Process != nil {
			if err := syncCmd.Process.Kill(); err != nil {
				if err.Error() != "os: process already finished" {
					logger.Info("Failed to kill sync process: %v", err)
				}
			}
		}
		if queryCmd != nil && queryCmd.Process != nil {
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

	// Step 3: Start sync subprocess (unless skipped for K8s)
	if !executeSkipSync {
		var err error
		syncCmd, err = startSyncProcess(cfg, logger)
		if err != nil {
			logger.Info("Failed to start sync process: %v", err)
			return 1
		}
		defer func() {
			if syncCmd.Process != nil {
				syncCmd.Process.Signal(syscall.SIGTERM)
				syncCmd.Wait()
			}
		}()
	} else {
		logger.Info("Skipping sync subprocess (--skip-sync flag set)")
	}

	// Step 4: Start query server subprocess (for realtime bots, unless skipped)
	if cfg.BotType == "realtime" && cfg.QueryEntrypoint != "" && !executeSkipQueryServer {
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
	} else if executeSkipQueryServer && cfg.BotType == "realtime" && cfg.QueryEntrypoint != "" {
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

	// Step 7: Cleanup
	cleanup()

	return exitCode
}

// runEphemeralQuery runs a single query and exits.
// Queries run directly without wrappers - the SDK's query.run() handles everything.
func runEphemeralQuery(cfg *execute.Config, logger *util.DefaultLogger) int {
	ctx := context.Background()

	logger.Info("Running ephemeral query: %s", cfg.QueryPath)

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

	// Execute query using the query entrypoint or main entrypoint
	// Queries run directly without wrappers
	entrypoint := cfg.Entrypoint
	if cfg.QueryEntrypoint != "" {
		entrypoint = cfg.QueryEntrypoint
	}

	exitCode := executeQueryProcess(ctx, cfg, entrypoint, logger)
	logger.Info("Query process completed with exit code: %d", exitCode)

	// If QUERY_RESULT_KEY is set (K8s mode), upload result to MinIO
	if cfg.QueryResultKey != "" {
		logger.Info("Uploading query result to MinIO key: %s", cfg.QueryResultKey)
		if err := uploadQueryResult(ctx, cfg, logger); err != nil {
			logger.Info("Failed to upload query result: %v", err)
			// Don't fail the whole query if upload fails
		}
	}

	return exitCode
}

// uploadQueryResult uploads the query result file to MinIO.
func uploadQueryResult(ctx context.Context, cfg *execute.Config, logger *util.DefaultLogger) error {
	storage := execute.NewStorageOperations(logger)
	return storage.UploadQueryResult(ctx, cfg.QueryResultKey, "/query/result.json")
}

// runQueryServerOnly runs only the query server (for K8s sidecar mode).
// Query servers run directly without wrappers - the SDK handles everything.
func runQueryServerOnly(cfg *execute.Config, logger *util.DefaultLogger) int {
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

	// Download code (may already be available via shared volume)
	if err := downloadCode(ctx, cfg, logger); err != nil {
		logger.Info("Failed to download code: %v", err)
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

// downloadCode downloads and extracts bot code from MinIO.
func downloadCode(ctx context.Context, cfg *execute.Config, logger *util.DefaultLogger) error {
	storage := execute.NewStorageOperations(logger)
	return storage.DownloadCode(ctx, cfg.CodeFile, cfg.CodePath)
}

// downloadState downloads bot state from MinIO.
func downloadState(ctx context.Context, cfg *execute.Config, logger *util.DefaultLogger) error {
	storage := execute.NewStorageOperations(logger)
	return storage.DownloadState(ctx, cfg.BotID, cfg.StatePath)
}

// startSyncProcess starts the daemon sync as a subprocess.
func startSyncProcess(cfg *execute.Config, logger *util.DefaultLogger) (*exec.Cmd, error) {
	args := []string{
		"daemon", "sync",
		"--bot-id", cfg.BotID,
		"--state-path", cfg.StatePath,
		"--logs-path", cfg.LogsPath,
	}

	if cfg.IsScheduled {
		args = append(args, "--watch-done", constants.DefaultDoneFilePath)
	}

	cmd := exec.Command("/app/runtime", args...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	cmd.Env = os.Environ()

	logger.Info("Starting sync subprocess: %v", args)
	if err := cmd.Start(); err != nil {
		return nil, fmt.Errorf("failed to start sync: %w", err)
	}

	logger.Info("Sync subprocess started (PID: %d)", cmd.Process.Pid)
	return cmd, nil
}

// startQueryServerProcess starts the query server as a subprocess.
// Query servers run directly without wrappers - the SDK handles everything.
func startQueryServerProcess(cfg *execute.Config, logger *util.DefaultLogger) (*exec.Cmd, error) {
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
func executeBotProcess(ctx context.Context, cfg *execute.Config, logger *util.DefaultLogger) int {
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
func executeProcessWithLogFile(ctx context.Context, cfg *execute.Config, entrypoint string, logger *util.DefaultLogger, logFile *os.File) int {
	return executeProcessWithDeps(ctx, cfg, entrypoint, logger, logFile, execute.NewDefaultDependencies())
}

// executeProcessWithDeps executes a process with injectable dependencies for testing.
func executeProcessWithDeps(ctx context.Context, cfg *execute.Config, entrypoint string, logger *util.DefaultLogger, logFile *os.File, deps execute.Dependencies) int {
	cmd := execute.BuildBotCommand(cfg.Runtime, entrypoint, cfg.CodePath)

	// Set up output: both stdout and optionally log file
	if logFile != nil {
		cmd.Stdout = io.MultiWriter(os.Stdout, logFile)
		cmd.Stderr = io.MultiWriter(os.Stderr, logFile)
	} else {
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
	}
	cmd.Env = execute.BuildBotEnv(cfg)

	logger.Info("Executing: %s (runtime: %s, entrypoint: %s)", cmd.Path, cfg.Runtime, entrypoint)

	// Run with context for cancellation support using injected dependency
	if err := deps.ProcessExecutor.Start(cmd); err != nil {
		logger.Info("Failed to start process: %v", err)
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
		if err != nil {
			if exitErr, ok := err.(*exec.ExitError); ok {
				return exitErr.ExitCode()
			}
			logger.Info("Process error: %v", err)
			return 1
		}
		return 0
	}
}

// executeQueryProcess executes a query process directly (no wrapper).
// Queries use the SDK's query.run() which handles everything internally.
func executeQueryProcess(ctx context.Context, cfg *execute.Config, entrypoint string, logger *util.DefaultLogger) int {
	return executeQueryProcessWithDeps(ctx, cfg, entrypoint, logger, execute.NewDefaultDependencies())
}

// executeQueryProcessWithDeps executes a query process with injectable dependencies.
func executeQueryProcessWithDeps(ctx context.Context, cfg *execute.Config, entrypoint string, logger *util.DefaultLogger, deps execute.Dependencies) int {
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
