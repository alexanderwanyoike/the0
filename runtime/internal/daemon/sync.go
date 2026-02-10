package daemon

import (
	"context"
	"fmt"
	"os"
	"os/signal"
	"path/filepath"
	"sync"
	"syscall"
	"time"

	"runtime/internal/constants"
	"runtime/internal/k8s/health"
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
	BotID         string
	StatePath     string        // State directory to sync (default: /state)
	LogsPath      string        // Logs directory to sync (default: /var/the0/logs)
	Interval      time.Duration // Backup sync interval - fsnotify handles immediate syncs (default: 60s)
	WatchDone     string        // Path to done file - when it appears, sync and exit (for scheduled bots)
	ReadinessPort int           // Port for health/readiness HTTP server (0 = disabled, used for K8s sidecar)
}

// SyncRunner runs sync in-process as a goroutine with proper lifecycle management.
// This avoids subprocess timing issues - the runner signals when ready.
type SyncRunner struct {
	opts      SyncOptions
	logger    util.Logger
	ctx       context.Context
	cancel    context.CancelFunc
	wg        sync.WaitGroup
	ready     chan struct{}
	initErr   error
	syncers   []Syncer
	syncMu    sync.Mutex
	cleanup   func()
}

// NewSyncRunner creates a new in-process sync runner.
func NewSyncRunner(opts SyncOptions, logger util.Logger) *SyncRunner {
	if logger == nil {
		logger = util.NewLogger()
	}
	ctx, cancel := context.WithCancel(context.Background())
	return &SyncRunner{
		opts:   opts,
		logger: logger,
		ctx:    ctx,
		cancel: cancel,
		ready:  make(chan struct{}),
	}
}

// Start begins the sync runner in a goroutine. Returns immediately.
// Call WaitReady() to block until initialization is complete.
func (r *SyncRunner) Start() {
	r.wg.Add(1)
	go r.run()
}

// WaitReady blocks until the sync runner is initialized or returns error if init failed.
func (r *SyncRunner) WaitReady(timeout time.Duration) error {
	select {
	case <-r.ready:
		return r.initErr
	case <-time.After(timeout):
		return fmt.Errorf("sync runner initialization timeout")
	}
}

// Stop gracefully shuts down the sync runner, performing a final sync.
func (r *SyncRunner) Stop() {
	r.cancel()
	r.wg.Wait()
}

// DoSync triggers an immediate sync (thread-safe).
func (r *SyncRunner) DoSync() {
	r.doSyncWithContext(r.ctx)
}

// doSyncWithContext performs sync with a specific context.
func (r *SyncRunner) doSyncWithContext(ctx context.Context) {
	r.syncMu.Lock()
	defer r.syncMu.Unlock()
	for _, syncer := range r.syncers {
		syncer.Sync(ctx)
	}
}

func (r *SyncRunner) run() {
	defer r.wg.Done()

	// Set defaults
	if r.opts.StatePath == "" {
		r.opts.StatePath = constants.DefaultStatePath
	}
	if r.opts.LogsPath == "" {
		r.opts.LogsPath = constants.DefaultLogsPath
	}
	if r.opts.Interval == 0 {
		r.opts.Interval = 60 * time.Second
	}

	r.logger.Info("Sync runner starting", "bot_id", r.opts.BotID)

	// Load config
	cfg, err := storage.LoadConfigFromEnv()
	if err != nil {
		r.initErr = fmt.Errorf("failed to load config: %w", err)
		close(r.ready)
		return
	}

	// Create MinIO client
	minioClient, err := storage.NewMinioClient(cfg)
	if err != nil {
		r.initErr = fmt.Errorf("failed to create MinIO client: %w", err)
		close(r.ready)
		return
	}

	// State syncer
	stateManager := storage.NewStateManager(minioClient, cfg, r.logger)
	stateSyncer := NewStateSyncer(r.opts.BotID, r.opts.StatePath, stateManager, r.logger)
	r.syncers = append(r.syncers, stateSyncer)

	// Logs syncer
	var logsSyncer *LogsSyncer
	var logPublisher LogPublisher
	if r.opts.LogsPath != "" {
		// NATS publisher (optional — for real-time log streaming)
		if natsURL := os.Getenv("NATS_URL"); natsURL != "" {
			np, err := NewNATSPublisher(natsURL, r.logger)
			if err != nil {
				r.logger.Info("Failed to create NATS publisher, continuing without", "error", err.Error())
			} else {
				logPublisher = np
			}
		}

		logUploader, err := miniologger.NewMinIOLogger(r.ctx, miniologger.MinioLoggerOptions{
			Endpoint:   cfg.Endpoint,
			AccessKey:  cfg.AccessKey,
			SecretKey:  cfg.SecretKey,
			UseSSL:     cfg.UseSSL,
			LogsBucket: "bot-logs",
		})
		if err != nil {
			r.logger.Info("Failed to create log uploader", "error", err.Error())
		} else {
			logsSyncer = NewLogsSyncer(r.opts.BotID, r.opts.LogsPath, logUploader, logPublisher, r.logger)
			if logsSyncer != nil {
				r.syncers = append(r.syncers, logsSyncer)
			}
		}
	}

	r.cleanup = func() {
		if logsSyncer != nil {
			logsSyncer.Close()
		}
		if logPublisher != nil {
			logPublisher.Close()
		}
	}

	// Create file watcher
	fileWatcher, err := NewFileWatcher(FileWatcherConfig{
		Logger:   r.logger,
		OnChange: r.DoSync,
		Debounce: 200 * time.Millisecond,
	})
	if err != nil {
		r.logger.Info("Failed to create file watcher: %v", err)
	} else {
		if r.opts.LogsPath != "" {
			os.MkdirAll(r.opts.LogsPath, 0755)
			fileWatcher.Watch(r.opts.LogsPath)
		}
		if r.opts.StatePath != "" {
			os.MkdirAll(r.opts.StatePath, 0755)
			fileWatcher.Watch(r.opts.StatePath)
		}
		go fileWatcher.Start()
		defer fileWatcher.Stop()
	}

	// Signal ready - initialization complete
	r.logger.Info("Sync runner ready", "bot_id", r.opts.BotID)
	close(r.ready)

	// Backup sync ticker
	ticker := time.NewTicker(r.opts.Interval)
	defer ticker.Stop()

	// Main loop
	for {
		select {
		case <-ticker.C:
			r.DoSync()
		case <-r.ctx.Done():
			r.logger.Info("Sync runner stopping, performing final sync")
			// Use fresh context for final sync since r.ctx is canceled
			finalCtx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
			r.doSyncWithContext(finalCtx)
			cancel()
			if r.cleanup != nil {
				r.cleanup()
			}
			r.logger.Info("Sync runner stopped")
			return
		}
	}
}

// Sync uses fsnotify to watch for file changes and immediately syncs to MinIO.
// Also performs periodic backup syncs in case fsnotify misses events.
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
		opts.Interval = 60 * time.Second // Backup interval, fsnotify handles immediate syncs
	}

	// Start health server for K8s readiness probes (if port specified)
	var healthServer *health.Server
	if opts.ReadinessPort > 0 {
		healthServer = health.NewServer(opts.ReadinessPort)
		healthServer.Start()
		defer func() {
			shutdownCtx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
			defer cancel()
			healthServer.Stop(shutdownCtx)
		}()
		logger.Info("Health server started", "port", opts.ReadinessPort)
	}

	logger.Info("Daemon sync starting with fsnotify", "bot_id", opts.BotID, "backup_interval", opts.Interval)

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
	var logPublisher LogPublisher
	if opts.LogsPath != "" {
		// NATS publisher (optional — for real-time log streaming)
		if natsURL := os.Getenv("NATS_URL"); natsURL != "" {
			np, err := NewNATSPublisher(natsURL, logger)
			if err != nil {
				logger.Info("Failed to create NATS publisher, continuing without", "error", err.Error())
			} else {
				logPublisher = np
			}
		}

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
			logsSyncer = NewLogsSyncer(opts.BotID, opts.LogsPath, logUploader, logPublisher, logger)
			if logsSyncer != nil {
				syncers = append(syncers, logsSyncer)
				logger.Info("Logs syncer initialized", "path", opts.LogsPath)
			}
		}
	}

	// Mutex to prevent concurrent syncs
	var syncMu sync.Mutex

	// Perform all sync operations (thread-safe)
	doSync := func() {
		syncMu.Lock()
		defer syncMu.Unlock()
		for _, syncer := range syncers {
			syncer.Sync(ctx)
		}
	}

	// Cleanup function
	cleanup := func() {
		if logsSyncer != nil {
			logsSyncer.Close()
		}
		if logPublisher != nil {
			logPublisher.Close()
		}
	}

	// Create file watcher for immediate change detection
	fileWatcher, err := NewFileWatcher(FileWatcherConfig{
		Logger:   logger,
		OnChange: doSync,
		Debounce: 200 * time.Millisecond, // Quick debounce for responsiveness
	})
	if err != nil {
		logger.Info("Failed to create file watcher, falling back to polling only: %v", err)
	} else {
		// Watch logs directory
		if opts.LogsPath != "" {
			if err := os.MkdirAll(opts.LogsPath, 0755); err == nil {
				if err := fileWatcher.Watch(opts.LogsPath); err != nil {
					logger.Info("Failed to watch logs path: %v", err)
				} else {
					logger.Info("Watching logs directory for changes", "path", opts.LogsPath)
				}
			}
		}

		// Watch state directory
		if opts.StatePath != "" {
			if err := os.MkdirAll(opts.StatePath, 0755); err == nil {
				if err := fileWatcher.Watch(opts.StatePath); err != nil {
					logger.Info("Failed to watch state path: %v", err)
				} else {
					logger.Info("Watching state directory for changes", "path", opts.StatePath)
				}
			}
		}

		// Watch done file's parent directory (for scheduled bots)
		if opts.WatchDone != "" {
			doneDir := filepath.Dir(opts.WatchDone)
			if err := os.MkdirAll(doneDir, 0755); err == nil {
				if err := fileWatcher.Watch(doneDir); err != nil {
					logger.Info("Failed to watch done file directory: %v", err)
				} else {
					logger.Info("Watching for done file", "path", opts.WatchDone)
				}
			}
		}

		// Start the file watcher
		go fileWatcher.Start()
		defer fileWatcher.Stop()
	}

	// Signal readiness after initialization is complete
	if healthServer != nil {
		healthServer.SetReady(true)
		logger.Info("Sync daemon ready")
	}

	// Signal handling
	sigCh := make(chan os.Signal, 1)
	signal.Notify(sigCh, syscall.SIGTERM, syscall.SIGINT)

	// Backup sync ticker (in case fsnotify misses events)
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

	// Done file check ticker (faster check since fsnotify may miss it)
	var doneTicker *time.Ticker
	if opts.WatchDone != "" {
		doneTicker = time.NewTicker(500 * time.Millisecond) // Check every 500ms as backup
		defer doneTicker.Stop()
	}

	// Main loop
	for {
		select {
		case <-ticker.C:
			// Backup periodic sync
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
