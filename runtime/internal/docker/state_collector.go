// Package docker provides the StateCollector component.
//
// StateCollector is a background service that periodically syncs state from
// long-running containers to MinIO. This ensures state is persisted even if
// the container crashes, and allows live state queries via the API.
package docker

import (
	"context"
	"runtime/internal/util"
	"sync"
	"time"
)

// StateCollector manages background state syncing from running containers.
type StateCollector interface {
	// Start begins the background state sync service.
	Start()

	// Stop gracefully stops the state sync service.
	Stop()

	// SyncState manually triggers a state sync for a specific bot.
	SyncState(botID string, stateDir string) error
}

// stateCollector implements StateCollector with periodic state syncing.
type stateCollector struct {
	// Dependencies
	stateManager      StateManager
	logger            util.Logger
	getContainersFunc func() []*ContainerInfo

	// Configuration
	interval time.Duration

	// Internal state
	ticker    *time.Ticker
	stopCh    chan struct{}
	doneCh    chan struct{}
	startOnce sync.Once
	stopOnce  sync.Once
	started   bool
	mu        sync.Mutex
}

// NewStateCollector creates a new StateCollector that syncs state at the specified interval.
func NewStateCollector(
	interval time.Duration,
	stateManager StateManager,
	logger util.Logger,
	getContainersFunc func() []*ContainerInfo,
) StateCollector {
	return &stateCollector{
		stateManager:      stateManager,
		logger:            logger,
		getContainersFunc: getContainersFunc,
		interval:          interval,
		stopCh:            make(chan struct{}),
		doneCh:            make(chan struct{}),
	}
}

// Start begins the background goroutine that periodically syncs state.
func (sc *stateCollector) Start() {
	sc.startOnce.Do(func() {
		sc.mu.Lock()
		sc.started = true
		sc.ticker = time.NewTicker(sc.interval)
		sc.mu.Unlock()

		sc.logger.Info("State Collector: Starting state sync service", "interval", sc.interval.String())
		go func() {
			defer close(sc.doneCh)
			for {
				select {
				case <-sc.ticker.C:
					sc.syncAllState()
				case <-sc.stopCh:
					sc.logger.Info("State Collector: Stopping state sync service")
					sc.ticker.Stop()
					return
				}
			}
		}()
	})
}

// Stop signals the background goroutine to stop and waits for it to finish.
func (sc *stateCollector) Stop() {
	sc.mu.Lock()
	wasStarted := sc.started
	sc.mu.Unlock()

	sc.stopOnce.Do(func() {
		close(sc.stopCh)
	})

	if wasStarted {
		<-sc.doneCh
	}
}

// syncAllState syncs state for all managed containers.
func (sc *stateCollector) syncAllState() {
	containers := sc.getContainersFunc()
	if len(containers) == 0 {
		return
	}

	sc.logger.Info("State Collector: Syncing state for containers", "count", len(containers))

	var wg sync.WaitGroup
	for _, containerInfo := range containers {
		if containerInfo.StateDir == "" {
			continue
		}
		wg.Add(1)
		go func(info *ContainerInfo) {
			defer wg.Done()
			sc.syncContainerState(info)
		}(containerInfo)
	}
	wg.Wait()
}

// syncContainerState syncs state for a single container.
func (sc *stateCollector) syncContainerState(containerInfo *ContainerInfo) {
	start := time.Now()

	if err := sc.SyncState(containerInfo.ID, containerInfo.StateDir); err != nil {
		sc.logger.Info("State Collector: Failed to sync state",
			"bot_id", containerInfo.ID,
			"error", err.Error())
		return
	}

	sc.logger.Info("State Collector: State synced",
		"bot_id", containerInfo.ID,
		"duration", time.Since(start).String())
}

// SyncState uploads state for a specific bot to MinIO.
func (sc *stateCollector) SyncState(botID string, stateDir string) error {
	if sc.stateManager == nil {
		return nil
	}

	ctx, cancel := context.WithTimeout(context.Background(), 60*time.Second)
	defer cancel()

	return sc.stateManager.UploadState(ctx, botID, stateDir)
}
