package gc

import (
	"context"
	"fmt"
	"strings"

	"runtime/internal/util"
)

// MinIOClient abstracts the MinIO operations needed by the GC.
type MinIOClient interface {
	ListObjectNames(ctx context.Context, bucket, prefix string) ([]string, error)
	RemoveObject(ctx context.Context, bucket, name string) error
}

// BotStore abstracts the database query for existing bot IDs.
type BotStore interface {
	GetAllBotIDs(ctx context.Context) (map[string]bool, error)
}

// RunResult contains the outcome of a GC sweep.
type RunResult struct {
	OrphanedBots   int
	DeletedObjects int
}

// GarbageCollector removes MinIO artifacts for bots that no longer exist.
type GarbageCollector struct {
	minio       MinIOClient
	store       BotStore
	logsBucket  string
	stateBucket string
	logger      util.Logger
}

// NewGarbageCollector creates a new GarbageCollector.
func NewGarbageCollector(minio MinIOClient, store BotStore, logsBucket, stateBucket string, logger util.Logger) *GarbageCollector {
	if logger == nil {
		logger = &util.NopLogger{}
	}
	return &GarbageCollector{
		minio:       minio,
		store:       store,
		logsBucket:  logsBucket,
		stateBucket: stateBucket,
		logger:      logger,
	}
}

// Run performs a single GC sweep: finds orphaned bot IDs in MinIO and deletes their artifacts.
func (gc *GarbageCollector) Run(ctx context.Context) (*RunResult, error) {
	// 1. Collect bot IDs from MinIO
	minioBotIDs, err := gc.collectMinIOBotIDs(ctx)
	if err != nil {
		return nil, fmt.Errorf("failed to list MinIO artifacts: %w", err)
	}

	if len(minioBotIDs) == 0 {
		gc.logger.Info("GC: no bot artifacts found in MinIO")
		return &RunResult{}, nil
	}

	// 2. Get existing bot IDs from database
	existingIDs, err := gc.store.GetAllBotIDs(ctx)
	if err != nil {
		return nil, fmt.Errorf("failed to query existing bots: %w", err)
	}

	// 3. Find orphans
	var orphanIDs []string
	for id := range minioBotIDs {
		if !existingIDs[id] {
			orphanIDs = append(orphanIDs, id)
		}
	}

	if len(orphanIDs) == 0 {
		gc.logger.Info("GC: no orphaned artifacts found (%d bots checked)", len(minioBotIDs))
		return &RunResult{}, nil
	}

	gc.logger.Info("GC: found %d orphaned bot(s), cleaning up...", len(orphanIDs))

	// 4. Delete artifacts for each orphan
	totalDeleted := 0
	for _, botID := range orphanIDs {
		deleted := gc.cleanupBot(ctx, botID)
		totalDeleted += deleted
	}

	gc.logger.Info("GC: deleted %d objects for %d orphaned bot(s)", totalDeleted, len(orphanIDs))

	return &RunResult{
		OrphanedBots:   len(orphanIDs),
		DeletedObjects: totalDeleted,
	}, nil
}

// collectMinIOBotIDs lists all unique bot IDs that have artifacts in MinIO.
func (gc *GarbageCollector) collectMinIOBotIDs(ctx context.Context) (map[string]bool, error) {
	ids := make(map[string]bool)

	// List log objects: logs/{botID}/{date}.log
	logObjects, err := gc.minio.ListObjectNames(ctx, gc.logsBucket, "logs/")
	if err != nil {
		return nil, err
	}
	for _, name := range logObjects {
		// Extract botID from "logs/{botID}/..."
		parts := strings.SplitN(strings.TrimPrefix(name, "logs/"), "/", 2)
		if len(parts) >= 1 && parts[0] != "" {
			ids[parts[0]] = true
		}
	}

	// List state objects: {botID}/state.tar.gz
	stateObjects, err := gc.minio.ListObjectNames(ctx, gc.stateBucket, "")
	if err != nil {
		return nil, err
	}
	for _, name := range stateObjects {
		parts := strings.SplitN(name, "/", 2)
		if len(parts) >= 1 && parts[0] != "" {
			ids[parts[0]] = true
		}
	}

	return ids, nil
}

// cleanupBot deletes all MinIO artifacts for a single bot. Returns the number of objects deleted.
func (gc *GarbageCollector) cleanupBot(ctx context.Context, botID string) int {
	deleted := 0

	// Delete logs
	logPrefix := fmt.Sprintf("logs/%s/", botID)
	logObjects, err := gc.minio.ListObjectNames(ctx, gc.logsBucket, logPrefix)
	if err != nil {
		gc.logger.Error("GC: failed to list logs for bot %s: %v", botID, err)
	} else {
		for _, name := range logObjects {
			if err := gc.minio.RemoveObject(ctx, gc.logsBucket, name); err != nil {
				gc.logger.Error("GC: failed to delete %s: %v", name, err)
			} else {
				deleted++
			}
		}
	}

	// Delete state
	stateName := fmt.Sprintf("%s/state.tar.gz", botID)
	if err := gc.minio.RemoveObject(ctx, gc.stateBucket, stateName); err != nil {
		gc.logger.Error("GC: failed to delete state for bot %s: %v", botID, err)
	} else {
		deleted++
	}

	return deleted
}
