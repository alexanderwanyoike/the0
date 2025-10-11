// Package dockerrunner provides the LogCollector component.
//
// LogCollector is a background service that periodically collects logs from
// long-running containers and stores them in MinIO. It runs on a configurable
// interval and handles each container independently to prevent blocking.
package dockerrunner

import (
	"context"
	miniologger "runtime/internal/minio-logger"
	"runtime/internal/util"
	"time"
)

// LogCollector manages background log collection from running containers.
type LogCollector interface {
	// Start begins the background log collection service.
	Start()

	// Stop gracefully stops the log collection service.
	Stop()
}

// logCollector implements LogCollector with periodic log collection.
type logCollector struct {
	// Dependencies
	orchestrator      ContainerOrchestrator
	minioLogger       miniologger.MinIOLogger
	logger            util.Logger
	getContainersFunc func() []*ContainerInfo

	// Internal state
	ticker *time.Ticker
	stopCh chan bool
	doneCh chan bool
}

// NewLogCollector creates a new LogCollector that collects logs at the specified interval.
func NewLogCollector(
	interval time.Duration,
	orchestrator ContainerOrchestrator,
	minioLogger miniologger.MinIOLogger,
	logger util.Logger,
	getContainersFunc func() []*ContainerInfo,
) LogCollector {
	return &logCollector{
		orchestrator:      orchestrator,
		minioLogger:       minioLogger,
		logger:            logger,
		getContainersFunc: getContainersFunc,
		ticker:            time.NewTicker(interval),
		stopCh:            make(chan bool),
		doneCh:            make(chan bool),
	}
}

// Start begins the background goroutine that periodically collects logs.
func (lc *logCollector) Start() {
	lc.logger.Info("Log Collector: Starting log collection service")
	go func() {
		defer close(lc.doneCh)
		for {
			select {
			case <-lc.ticker.C:
				lc.collectAllLogs()
			case <-lc.stopCh:
				lc.logger.Info("Log Collector: Stopping log collection service")
				lc.ticker.Stop()
				return
			}
		}
	}()
}

// Stop signals the background goroutine to stop and waits for it to finish.
func (lc *logCollector) Stop() {
	close(lc.stopCh)
	<-lc.doneCh
}

// collectAllLogs fetches logs from all managed containers concurrently.
func (lc *logCollector) collectAllLogs() {
	containers := lc.getContainersFunc()
	if len(containers) == 0 {
		lc.logger.Info("Log Collector: No managed containers to collect logs from")
		return
	}
	for _, containerInfo := range containers {
		// Process each container in its own goroutine to prevent a slow container from blocking others
		go lc.collectAndStoreLogs(containerInfo)
	}
}

// collectAndStoreLogs fetches logs from a single container and stores them in MinIO.
func (lc *logCollector) collectAndStoreLogs(containerInfo *ContainerInfo) {
	lc.logger.Info("Log Collector: Collecting logs", "container_id", containerInfo.ID, "bot_id", containerInfo.ID)
	start := time.Now()
	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()

	// Delegate log fetching to the orchestrator for incremental collection
	logs, err := lc.orchestrator.GetLogs(ctx, containerInfo.ContainerID, 100)
	if err != nil {
		lc.logger.Info("Log Collector: Failed to get logs", "container_id", containerInfo.ContainerID, "error", err.Error())
		return
	}

	if len(logs) == 0 {
		// No new logs to process
		return
	}

	if err := lc.minioLogger.AppendBotLogs(ctx, containerInfo.ID, logs); err != nil {
		lc.logger.Info("Log Collector: Failed to store logs in MinIO", "bot_id", containerInfo.ID, "error", err.Error())
		return
	}

	lc.logger.Info("Log Collector: Successfully collected and stored logs",
		"container_id", containerInfo.ContainerID,
		"bot_id", containerInfo.ID,
		"duration", time.Since(start).String(),
		"log_size", len(logs))
}
