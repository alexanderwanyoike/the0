package gc

import (
	"context"
	"fmt"
	"strings"
	"time"

	"runtime/internal/util"
)

// ObjectInfo holds metadata for a single MinIO object.
type ObjectInfo struct {
	Name         string
	LastModified time.Time
}

// IncompleteUploadInfo holds metadata for a single incomplete multipart upload.
type IncompleteUploadInfo struct {
	Key       string
	UploadID  string
	Initiated time.Time
}

// MinIOClient abstracts the MinIO operations needed by the GC.
type MinIOClient interface {
	ListObjectNames(ctx context.Context, bucket, prefix string) ([]string, error)
	ListObjectsWithInfo(ctx context.Context, bucket, prefix string) ([]ObjectInfo, error)
	RemoveObject(ctx context.Context, bucket, name string) error
	ListIncompleteUploads(ctx context.Context, bucket, prefix string) ([]IncompleteUploadInfo, error)
	AbortIncompleteUpload(ctx context.Context, bucket, key, uploadID string) error
}

// BotStore abstracts the database query for existing bot IDs.
type BotStore interface {
	GetAllBotIDs(ctx context.Context) (map[string]bool, error)
}

// RunResult contains the outcome of a GC sweep.
type RunResult struct {
	OrphanedBots            int
	DeletedObjects          int
	StaleTempFiles          int
	AbortedMultipartUploads int
}

// GarbageCollector removes MinIO artifacts for bots that no longer exist.
type GarbageCollector struct {
	minio       MinIOClient
	store       BotStore
	logsBucket  string
	stateBucket string
	logger      util.Logger
}

// GarbageCollectorOptions configures a GarbageCollector.
type GarbageCollectorOptions struct {
	MinIO       MinIOClient
	Store       BotStore
	LogsBucket  string
	StateBucket string
	Logger      util.Logger
}

// NewGarbageCollector creates a new GarbageCollector.
func NewGarbageCollector(opts GarbageCollectorOptions) *GarbageCollector {
	if opts.Logger == nil {
		opts.Logger = &util.NopLogger{}
	}
	return &GarbageCollector{
		minio:       opts.MinIO,
		store:       opts.Store,
		logsBucket:  opts.LogsBucket,
		stateBucket: opts.StateBucket,
		logger:      opts.Logger,
	}
}

// staleTempFileAge is the minimum age for a temp file to be considered stale.
// Temp files older than this are assumed to be leaked from a previous bug or
// crash and are safe to delete.
const staleTempFileAge = 1 * time.Hour

// staleIncompleteUploadAge is the minimum age for an incomplete multipart
// upload to be considered stale. Comfortably exceeds the longest sync upload
// timeout (state: 180s) so no in-flight upload can ever be older.
const staleIncompleteUploadAge = 1 * time.Hour

// Run performs a single GC sweep: cleans up stale temp files, then finds
// orphaned bot IDs in MinIO and deletes their artifacts.
func (gc *GarbageCollector) Run(ctx context.Context) (*RunResult, error) {
	result := &RunResult{}

	// 0. Clean up stale temp files (leaked by the old fire-and-forget goroutine bug)
	result.StaleTempFiles = gc.cleanupStaleTempFiles(ctx)

	// 0a. Clean up stale incomplete multipart uploads on both buckets. These
	// leak when an upload context is cancelled mid-stream (e.g. tail latency
	// hitting the per-sync timeout, pod OOM, etc.) without an abort reaching
	// the server. Filtered by Initiated age so we never touch an in-flight op.
	result.AbortedMultipartUploads += gc.cleanupStaleIncompleteUploads(ctx, gc.logsBucket)
	result.AbortedMultipartUploads += gc.cleanupStaleIncompleteUploads(ctx, gc.stateBucket)

	// 1. Collect bot IDs from MinIO
	minioBotIDs, err := gc.collectMinIOBotIDs(ctx)
	if err != nil {
		return nil, fmt.Errorf("failed to list MinIO artifacts: %w", err)
	}

	if len(minioBotIDs) == 0 {
		gc.logger.Info("GC: no bot artifacts found in MinIO")
		return result, nil
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
		return result, nil
	}

	gc.logger.Info("GC: found %d orphaned bot(s), cleaning up...", len(orphanIDs))

	// 4. Delete artifacts for each orphan
	totalDeleted := 0
	for _, botID := range orphanIDs {
		deleted := gc.cleanupBot(ctx, botID)
		totalDeleted += deleted
	}

	gc.logger.Info("GC: deleted %d objects for %d orphaned bot(s)", totalDeleted, len(orphanIDs))

	result.OrphanedBots = len(orphanIDs)
	result.DeletedObjects = totalDeleted
	return result, nil
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
		// Accept only logs/{botID}/{file}
		parts := strings.SplitN(strings.TrimPrefix(name, "logs/"), "/", 2)
		if len(parts) == 2 && parts[0] != "" && parts[1] != "" {
			ids[parts[0]] = true
		}
	}

	// List state objects: {botID}/state.tar.gz
	stateObjects, err := gc.minio.ListObjectNames(ctx, gc.stateBucket, "")
	if err != nil {
		return nil, err
	}
	for _, name := range stateObjects {
		// Accept only {botID}/state.tar.gz
		if !strings.HasSuffix(name, "/state.tar.gz") {
			continue
		}
		parts := strings.SplitN(name, "/", 2)
		if len(parts) == 2 && parts[0] != "" {
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

// cleanupStaleIncompleteUploads lists incomplete multipart uploads in the
// given bucket and aborts any whose Initiated timestamp is older than
// staleIncompleteUploadAge. Errors are logged, not returned, because this
// is best-effort and should not block the rest of the sweep. Returns the
// number of uploads successfully aborted.
func (gc *GarbageCollector) cleanupStaleIncompleteUploads(ctx context.Context, bucket string) int {
	if bucket == "" {
		return 0
	}
	uploads, err := gc.minio.ListIncompleteUploads(ctx, bucket, "")
	if err != nil {
		gc.logger.Error("GC: failed to list incomplete uploads in %s: %v", bucket, err)
		return 0
	}

	cutoff := time.Now().Add(-staleIncompleteUploadAge)
	aborted := 0

	for _, u := range uploads {
		if !u.Initiated.Before(cutoff) {
			continue
		}
		if err := gc.minio.AbortIncompleteUpload(ctx, bucket, u.Key, u.UploadID); err != nil {
			gc.logger.Error("GC: failed to abort incomplete upload %s (%s/%s): %v", u.UploadID, bucket, u.Key, err)
			continue
		}
		aborted++
	}

	if aborted > 0 {
		gc.logger.Info("GC: aborted %d stale incomplete upload(s) in %s", aborted, bucket)
	}

	return aborted
}

// cleanupStaleTempFiles lists all objects under logs/ and deletes any temp
// files (containing ".tmp-" in the name) that are older than staleTempFileAge.
// Returns the number of deleted objects. Errors are logged, not returned,
// because temp cleanup is best-effort and should not block the orphan sweep.
func (gc *GarbageCollector) cleanupStaleTempFiles(ctx context.Context) int {
	objects, err := gc.minio.ListObjectsWithInfo(ctx, gc.logsBucket, "logs/")
	if err != nil {
		gc.logger.Error("GC: failed to list objects for temp cleanup: %v", err)
		return 0
	}

	cutoff := time.Now().Add(-staleTempFileAge)
	deleted := 0

	for _, obj := range objects {
		// Check the filename portion only, not the full path, to avoid
		// matching bot IDs that happen to contain ".tmp-"
		parts := strings.Split(obj.Name, "/")
		filename := parts[len(parts)-1]
		if !strings.HasPrefix(filename, ".tmp-") {
			continue
		}
		if obj.LastModified.Before(cutoff) {
			if err := gc.minio.RemoveObject(ctx, gc.logsBucket, obj.Name); err != nil {
				gc.logger.Error("GC: failed to delete stale temp file %s: %v", obj.Name, err)
			} else {
				deleted++
			}
		}
	}

	if deleted > 0 {
		gc.logger.Info("GC: deleted %d stale temp file(s)", deleted)
	}

	return deleted
}
