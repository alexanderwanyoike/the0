package daemon

import (
	"context"
	"fmt"
	"os"
	"os/signal"
	"syscall"
	"time"

	"runtime/internal/constants"
	miniologger "runtime/internal/minio-logger"
	"runtime/internal/runtime/storage"
	"runtime/internal/util"
)

// Syncer defines the interface for sync components.
type Syncer interface {
	Sync(ctx context.Context) bool
}

// SyncOptions configures the sync command.
type SyncOptions struct {
	BotID     string
	StatePath string        // State directory to sync (default: /state)
	LogsPath  string        // Logs directory to sync (default: /var/the0/logs)
	Interval  time.Duration // Sync interval (default: 60s)
	WatchDone string        // Path to done file - when it appears, sync and exit (for scheduled bots)
}

// Sync periodically uploads state and logs to MinIO.
// If opts.WatchDone is set, exits when done file appears (for scheduled bots).
// Otherwise runs indefinitely as a sidecar container.
func Sync(ctx context.Context, opts SyncOptions) error {
	var logger util.Logger = util.NewLogger()

	// Set defaults
	if opts.StatePath == "" {
		opts.StatePath = constants.DefaultStatePath
	}
	if opts.LogsPath == "" {
		opts.LogsPath = constants.DefaultLogsPath
	}
	if opts.Interval == 0 {
		opts.Interval = 60 * time.Second
	}

	logger.Info("Daemon sync starting", "bot_id", opts.BotID, "interval", opts.Interval)

	// Load config
	cfg, err := storage.LoadConfigFromEnv()
	if err != nil {
		return fmt.Errorf("failed to load config: %w", err)
	}

	// Create MinIO client
	minioClient, err := storage.NewMinioClient(cfg)
	if err != nil {
		return fmt.Errorf("failed to create MinIO client: %w", err)
	}

	// Initialize syncers
	var syncers []Syncer

	// State syncer
	stateManager := storage.NewStateManager(minioClient, cfg, logger)
	stateSyncer := NewStateSyncer(opts.BotID, opts.StatePath, stateManager, logger)
	syncers = append(syncers, stateSyncer)
	logger.Info("State syncer initialized", "path", opts.StatePath)

	// Logs syncer (optional - may fail if MinIO bucket doesn't exist)
	var logsSyncer *LogsSyncer
	if opts.LogsPath != "" {
		logUploader, err := miniologger.NewMinIOLogger(ctx, miniologger.MinioLoggerOptions{
			Endpoint:   cfg.Endpoint,
			AccessKey:  cfg.AccessKey,
			SecretKey:  cfg.SecretKey,
			UseSSL:     cfg.UseSSL,
			LogsBucket: "bot-logs",
		})
		if err != nil {
			logger.Info("Failed to create log uploader, logs will not be synced", "error", err.Error())
		} else {
			logsSyncer = NewLogsSyncer(opts.BotID, opts.LogsPath, logUploader, logger)
			if logsSyncer != nil {
				syncers = append(syncers, logsSyncer)
				logger.Info("Logs syncer initialized", "path", opts.LogsPath)
			}
		}
	}

	// Perform all sync operations
	doSync := func() {
		for _, syncer := range syncers {
			syncer.Sync(ctx)
		}
	}

	// Cleanup function
	cleanup := func() {
		if logsSyncer != nil {
			logsSyncer.Close()
		}
	}

	// Sidecar mode: run with periodic sync
	sigCh := make(chan os.Signal, 1)
	signal.Notify(sigCh, syscall.SIGTERM, syscall.SIGINT)

	ticker := time.NewTicker(opts.Interval)
	defer ticker.Stop()

	// Check for done file (for scheduled bots)
	checkDone := func() bool {
		if opts.WatchDone == "" {
			return false
		}
		_, err := os.Stat(opts.WatchDone)
		return err == nil
	}

	// Done file check ticker (check every 2 seconds)
	var doneTicker *time.Ticker
	if opts.WatchDone != "" {
		doneTicker = time.NewTicker(2 * time.Second)
		defer doneTicker.Stop()
		logger.Info("Watching for done file", "path", opts.WatchDone)
	}

	// Main loop
	for {
		select {
		case <-ticker.C:
			doSync()
		case <-func() <-chan time.Time {
			if doneTicker != nil {
				return doneTicker.C
			}
			return nil
		}():
			if checkDone() {
				logger.Info("Done file detected, performing final sync")
				doSync()
				cleanup()
				logger.Info("Daemon sync shutdown complete (bot finished)")
				return nil
			}
		case <-ctx.Done():
			logger.Info("Context cancelled, performing final sync")
			doSync()
			cleanup()
			logger.Info("Daemon sync shutdown complete")
			return nil
		case sig := <-sigCh:
			logger.Info("Received signal, performing final sync", "signal", sig.String())
			doSync()
			cleanup()
			logger.Info("Daemon sync shutdown complete")
			return nil
		}
	}
}
