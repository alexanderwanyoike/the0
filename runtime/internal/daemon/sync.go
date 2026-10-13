// Package daemon syncs a bot container's state and logs to storage, either
// in-process for runtime execute or as the Kubernetes sync sidecar.
package daemon

import (
	"context"
	"fmt"
	"os"
	"os/signal"
	"sync"
	"syscall"
	"time"

	"runtime/internal/constants"
	"runtime/internal/k8s/health"
	"runtime/internal/launch"
	miniologger "runtime/internal/minio-logger"
	"runtime/internal/runtime/storage"
	"runtime/internal/util"
)

// Syncer defines the interface for sync components.
type Syncer interface {
	Sync(ctx context.Context) bool
	// FinalSyncTimeout returns the context deadline to use for the final
	// sync on shutdown. Each syncer knows its own upload timeout; the
	// shutdown path creates a fresh per-syncer context so serialized
	// syncers don't starve each other from a shared parent deadline.
	FinalSyncTimeout() time.Duration
}

// SyncOptions configures the sync command.
type SyncOptions struct {
	BotID         string
	StatePath     string        // State directory to sync (default: /state)
	LogsPath      string        // Logs directory to sync (default: /var/the0/logs)
	Interval      time.Duration // Backup sync interval - fsnotify handles immediate syncs (default: 60s)
	ReadinessPort int           // Port for health/readiness HTTP server (0 = disabled, used for K8s sidecar)
}

// SyncRunner runs sync in-process as a goroutine with proper lifecycle management.
// This avoids subprocess timing issues - the runner signals when ready.
type SyncRunner struct {
	opts    SyncOptions
	logger  util.Logger
	ctx     context.Context
	cancel  context.CancelFunc
	wg      sync.WaitGroup
	ready   chan struct{}
	initErr error
	syncers []Syncer
	syncMu  sync.Mutex
	cleanup func()
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
		logUploader, err := miniologger.NewMinIOLogger(r.ctx, miniologger.MinioLoggerOptions{
			Endpoint:   cfg.Endpoint,
			AccessKey:  cfg.AccessKey,
			SecretKey:  cfg.SecretKey,
			UseSSL:     cfg.UseSSL,
			LogsBucket: cfg.LogsBucket,
		})
		if err != nil {
			r.logger.Info("Failed to create log uploader", "error", err.Error())
		} else {
			logPublisher = newLogPublisherFromEnv(r.logger)
			logsSyncer = NewLogsSyncer(r.opts.BotID, r.opts.LogsPath, logUploader, logPublisher, r.logger)
			if logsSyncer != nil {
				r.syncers = append(r.syncers, logsSyncer)
			}
		}
	}

	r.cleanup = func() {
		if logsSyncer != nil {
			if err := logsSyncer.Close(); err != nil {
				r.logger.Info("Failed to close logs syncer", "error", err.Error())
			}
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
			// Each syncer gets its own fresh context sized to its upload
			// timeout. This prevents serialized syncers from starving each
			// other via a shared parent deadline.
			r.syncMu.Lock()
			for _, syncer := range r.syncers {
				ctx, cancel := context.WithTimeout(context.Background(), syncer.FinalSyncTimeout())
				syncer.Sync(ctx)
				cancel()
			}
			r.syncMu.Unlock()
			if r.cleanup != nil {
				r.cleanup()
			}
			r.logger.Info("Sync runner stopped")
			return
		}
	}
}

// newLogPublisherFromEnv creates a LogPublisher from NATS_URL env var.
// Returns nil if NATS_URL is not set or connection fails.
func newLogPublisherFromEnv(logger util.Logger) LogPublisher {
	natsURL := launch.ReadNATSURL(os.LookupEnv)
	if natsURL == "" {
		return nil
	}
	np, err := NewNATSPublisher(natsURL, logger)
	if err != nil {
		logger.Info("Failed to create NATS publisher, continuing without", "error", err.Error())
		return nil
	}
	return np
}

// Sync uses fsnotify to watch for file changes and immediately syncs to MinIO.
// Also performs periodic backup syncs in case fsnotify misses events.
// It runs as the sync sidecar until it is cancelled or signalled.
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
		logUploader, err := miniologger.NewMinIOLogger(ctx, miniologger.MinioLoggerOptions{
			Endpoint:   cfg.Endpoint,
			AccessKey:  cfg.AccessKey,
			SecretKey:  cfg.SecretKey,
			UseSSL:     cfg.UseSSL,
			LogsBucket: cfg.LogsBucket,
		})
		if err != nil {
			logger.Info("Failed to create log uploader, logs will not be synced", "error", err.Error())
		} else {
			logPublisher = newLogPublisherFromEnv(logger)
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
			if err := logsSyncer.Close(); err != nil {
				logger.Info("Failed to close logs syncer", "error", err.Error())
			}
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

	// Main loop
	for {
		select {
		case <-ticker.C:
			// Backup periodic sync
			doSync()

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
