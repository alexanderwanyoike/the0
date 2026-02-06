package daemon

import (
	"os"
	"path/filepath"
	"sync"
	"time"

	"github.com/fsnotify/fsnotify"
	"runtime/internal/util"
)

// FileWatcher watches files and directories for changes using fsnotify.
// When changes are detected, it triggers a callback.
type FileWatcher struct {
	watcher    *fsnotify.Watcher
	logger     util.Logger
	onChange   func()
	debounce   time.Duration
	mu         sync.Mutex
	lastEvent  time.Time
	stopCh     chan struct{}
	stoppedCh  chan struct{}
	started    bool
	startedMu  sync.Mutex
}

// FileWatcherConfig configures the file watcher.
type FileWatcherConfig struct {
	Logger   util.Logger
	OnChange func()           // Callback when files change
	Debounce time.Duration    // Debounce period to avoid rapid syncs (default: 500ms)
}

// NewFileWatcher creates a new file watcher.
func NewFileWatcher(cfg FileWatcherConfig) (*FileWatcher, error) {
	watcher, err := fsnotify.NewWatcher()
	if err != nil {
		return nil, err
	}

	if cfg.Logger == nil {
		cfg.Logger = &util.DefaultLogger{}
	}
	if cfg.Debounce == 0 {
		cfg.Debounce = 500 * time.Millisecond
	}

	return &FileWatcher{
		watcher:   watcher,
		logger:    cfg.Logger,
		onChange:  cfg.OnChange,
		debounce:  cfg.Debounce,
		stopCh:    make(chan struct{}),
		stoppedCh: make(chan struct{}),
	}, nil
}

// Watch adds a path to the watcher.
// If the path is a directory, it watches for changes in the directory.
// If the path doesn't exist, it watches the parent directory for creation.
func (fw *FileWatcher) Watch(path string) error {
	// If path exists, watch it directly
	if _, err := os.Stat(path); err == nil {
		return fw.watcher.Add(path)
	}

	// Path doesn't exist - watch parent directory for creation
	parent := filepath.Dir(path)
	if _, err := os.Stat(parent); err == nil {
		fw.logger.Info("Path %s doesn't exist, watching parent %s", path, parent)
		return fw.watcher.Add(parent)
	}

	// Parent doesn't exist either - create it and watch
	if err := os.MkdirAll(parent, 0755); err != nil {
		return err
	}
	fw.logger.Info("Created and watching directory %s", parent)
	return fw.watcher.Add(parent)
}

// Start begins watching for file changes.
// This should be called in a goroutine.
func (fw *FileWatcher) Start() {
	fw.startedMu.Lock()
	fw.started = true
	fw.startedMu.Unlock()

	defer close(fw.stoppedCh)

	for {
		select {
		case event, ok := <-fw.watcher.Events:
			if !ok {
				return
			}
			fw.handleEvent(event)

		case err, ok := <-fw.watcher.Errors:
			if !ok {
				return
			}
			fw.logger.Info("Watcher error: %v", err)

		case <-fw.stopCh:
			return
		}
	}
}

// handleEvent processes a file system event with debouncing.
func (fw *FileWatcher) handleEvent(event fsnotify.Event) {
	// Only trigger on write/create events (not chmod, rename, etc.)
	if event.Op&(fsnotify.Write|fsnotify.Create) == 0 {
		return
	}

	fw.mu.Lock()
	now := time.Now()
	shouldTrigger := now.Sub(fw.lastEvent) >= fw.debounce
	if shouldTrigger {
		fw.lastEvent = now
	}
	fw.mu.Unlock()

	if shouldTrigger && fw.onChange != nil {
		fw.logger.Info("File change detected: %s (op: %s)", event.Name, event.Op)
		fw.onChange()
	}
}

// Stop stops the file watcher.
func (fw *FileWatcher) Stop() error {
	fw.startedMu.Lock()
	wasStarted := fw.started
	fw.startedMu.Unlock()

	close(fw.stopCh)

	// Only wait for stoppedCh if Start() was called
	if wasStarted {
		<-fw.stoppedCh
	}

	return fw.watcher.Close()
}
