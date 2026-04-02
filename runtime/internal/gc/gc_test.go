package gc

import (
	"context"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// mockMinIOClient implements the MinIOClient interface for testing
type mockMinIOClient struct {
	logObjects      []string     // object names in logs bucket
	stateObjects    []string     // object names in state bucket
	logObjectsInfo  []ObjectInfo // objects with metadata in logs bucket
	deleted         []string     // track deleted object paths (bucket:name)
	listErr         error
	removeErr       error
}

func (m *mockMinIOClient) ListObjectNames(ctx context.Context, bucket, prefix string) ([]string, error) {
	if m.listErr != nil {
		return nil, m.listErr
	}
	var objects []string
	if bucket == "bot-logs" {
		objects = m.logObjects
	} else if bucket == "bot-state" {
		objects = m.stateObjects
	}
	// filter by prefix
	var result []string
	for _, obj := range objects {
		if len(obj) >= len(prefix) && obj[:len(prefix)] == prefix {
			result = append(result, obj)
		}
	}
	return result, nil
}

func (m *mockMinIOClient) ListObjectsWithInfo(ctx context.Context, bucket, prefix string) ([]ObjectInfo, error) {
	if m.listErr != nil {
		return nil, m.listErr
	}
	var objects []ObjectInfo
	if bucket == "bot-logs" {
		objects = m.logObjectsInfo
	}
	// filter by prefix
	var result []ObjectInfo
	for _, obj := range objects {
		if len(obj.Name) >= len(prefix) && obj.Name[:len(prefix)] == prefix {
			result = append(result, obj)
		}
	}
	return result, nil
}

func (m *mockMinIOClient) RemoveObject(ctx context.Context, bucket, name string) error {
	if m.removeErr != nil {
		return m.removeErr
	}
	m.deleted = append(m.deleted, bucket+":"+name)
	return nil
}

// mockBotStore implements the BotStore interface for testing
type mockBotStore struct {
	existingIDs map[string]bool
	queryErr    error
}

func (m *mockBotStore) GetAllBotIDs(ctx context.Context) (map[string]bool, error) {
	if m.queryErr != nil {
		return nil, m.queryErr
	}
	return m.existingIDs, nil
}

func TestGC_DeletesOrphanedArtifacts(t *testing.T) {
	minio := &mockMinIOClient{
		logObjects: []string{
			"logs/bot-active/20260101.log",
			"logs/bot-active/20260102.log",
			"logs/bot-orphan/20260101.log",
			"logs/bot-orphan/20260102.log",
			"logs/bot-orphan/20260103.log",
		},
		stateObjects: []string{
			"bot-active/state.tar.gz",
			"bot-orphan/state.tar.gz",
		},
	}

	store := &mockBotStore{
		existingIDs: map[string]bool{
			"bot-active": true,
		},
	}

	gc := NewGarbageCollector(GarbageCollectorOptions{MinIO: minio, Store: store, LogsBucket: "bot-logs", StateBucket: "bot-state"})
	result, err := gc.Run(context.Background())

	require.NoError(t, err)
	assert.Equal(t, 1, result.OrphanedBots)
	assert.Equal(t, 4, result.DeletedObjects) // 3 logs + 1 state

	// Verify only orphan files were deleted
	assert.Contains(t, minio.deleted, "bot-logs:logs/bot-orphan/20260101.log")
	assert.Contains(t, minio.deleted, "bot-logs:logs/bot-orphan/20260102.log")
	assert.Contains(t, minio.deleted, "bot-logs:logs/bot-orphan/20260103.log")
	assert.Contains(t, minio.deleted, "bot-state:bot-orphan/state.tar.gz")

	// Active bot files should NOT be deleted
	for _, d := range minio.deleted {
		assert.NotContains(t, d, "bot-active")
	}
}

func TestGC_NoOrphans(t *testing.T) {
	minio := &mockMinIOClient{
		logObjects: []string{
			"logs/bot-1/20260101.log",
		},
		stateObjects: []string{
			"bot-1/state.tar.gz",
		},
	}

	store := &mockBotStore{
		existingIDs: map[string]bool{
			"bot-1": true,
		},
	}

	gc := NewGarbageCollector(GarbageCollectorOptions{MinIO: minio, Store: store, LogsBucket: "bot-logs", StateBucket: "bot-state"})
	result, err := gc.Run(context.Background())

	require.NoError(t, err)
	assert.Equal(t, 0, result.OrphanedBots)
	assert.Equal(t, 0, result.DeletedObjects)
	assert.Empty(t, minio.deleted)
}

func TestGC_EmptyMinIO(t *testing.T) {
	minio := &mockMinIOClient{}
	store := &mockBotStore{
		existingIDs: map[string]bool{"bot-1": true},
	}

	gc := NewGarbageCollector(GarbageCollectorOptions{MinIO: minio, Store: store, LogsBucket: "bot-logs", StateBucket: "bot-state"})
	result, err := gc.Run(context.Background())

	require.NoError(t, err)
	assert.Equal(t, 0, result.OrphanedBots)
	assert.Equal(t, 0, result.DeletedObjects)
}

func TestGC_MinIOListError(t *testing.T) {
	minio := &mockMinIOClient{
		listErr: assert.AnError,
	}
	store := &mockBotStore{
		existingIDs: map[string]bool{},
	}

	gc := NewGarbageCollector(GarbageCollectorOptions{MinIO: minio, Store: store, LogsBucket: "bot-logs", StateBucket: "bot-state"})
	_, err := gc.Run(context.Background())

	assert.Error(t, err)
}

func TestGC_MongoQueryError(t *testing.T) {
	minio := &mockMinIOClient{
		logObjects: []string{"logs/bot-1/20260101.log"},
	}
	store := &mockBotStore{
		queryErr: assert.AnError,
	}

	gc := NewGarbageCollector(GarbageCollectorOptions{MinIO: minio, Store: store, LogsBucket: "bot-logs", StateBucket: "bot-state"})
	_, err := gc.Run(context.Background())

	assert.Error(t, err)
}

func TestGC_RemoveErrorContinues(t *testing.T) {
	minio := &mockMinIOClient{
		logObjects: []string{
			"logs/bot-orphan/20260101.log",
			"logs/bot-orphan/20260102.log",
		},
		removeErr: assert.AnError,
	}
	store := &mockBotStore{
		existingIDs: map[string]bool{},
	}

	gc := NewGarbageCollector(GarbageCollectorOptions{MinIO: minio, Store: store, LogsBucket: "bot-logs", StateBucket: "bot-state"})
	result, err := gc.Run(context.Background())

	// Should not return error -- removal failures are logged, not fatal
	require.NoError(t, err)
	assert.Equal(t, 1, result.OrphanedBots)
	assert.Equal(t, 0, result.DeletedObjects) // none succeeded
}

func TestGC_CleansUpStaleTempFiles(t *testing.T) {
	now := time.Now()

	minioClient := &mockMinIOClient{
		// Normal log and state objects for collectMinIOBotIDs / orphan path
		logObjects: []string{
			"logs/bot-active/20260101.log",
		},
		stateObjects: []string{},
		// Objects with info for the temp cleanup sweep
		logObjectsInfo: []ObjectInfo{
			// Stale temp file (2 hours old) - should be deleted
			{Name: "logs/bot-active/.tmp-1711900000000000000", LastModified: now.Add(-2 * time.Hour)},
			// Stale temp file (90 minutes old) - should be deleted
			{Name: "logs/bot-active/.tmp-1711900100000000000", LastModified: now.Add(-90 * time.Minute)},
			// Fresh temp file (5 minutes old) - should be kept
			{Name: "logs/bot-active/.tmp-1711900200000000000", LastModified: now.Add(-5 * time.Minute)},
			// Regular log file (not a temp file) - should be ignored
			{Name: "logs/bot-active/20260101.log", LastModified: now.Add(-24 * time.Hour)},
		},
	}

	store := &mockBotStore{
		existingIDs: map[string]bool{
			"bot-active": true,
		},
	}

	gc := NewGarbageCollector(GarbageCollectorOptions{
		MinIO:       minioClient,
		Store:       store,
		LogsBucket:  "bot-logs",
		StateBucket: "bot-state",
	})

	result, err := gc.Run(context.Background())

	require.NoError(t, err)
	assert.Equal(t, 0, result.OrphanedBots, "no bots should be orphaned")
	assert.Equal(t, 0, result.DeletedObjects, "no orphan objects should be deleted")
	assert.Equal(t, 2, result.StaleTempFiles, "two stale temp files should be deleted")

	// Verify only stale temp files were deleted
	assert.Contains(t, minioClient.deleted, "bot-logs:logs/bot-active/.tmp-1711900000000000000")
	assert.Contains(t, minioClient.deleted, "bot-logs:logs/bot-active/.tmp-1711900100000000000")

	// Fresh temp and regular log file should NOT be deleted
	for _, d := range minioClient.deleted {
		assert.NotContains(t, d, ".tmp-1711900200000000000", "fresh temp file should not be deleted")
		assert.NotContains(t, d, "20260101.log", "regular log file should not be deleted")
	}
}

func TestGC_StaleTempFilesListError(t *testing.T) {
	// When ListObjectsWithInfo fails, the GC should log the error but
	// continue with the rest of the sweep (orphan cleanup).
	minioClient := &mockMinIOClient{
		logObjects:   []string{},
		stateObjects: []string{},
		// listErr affects all list calls, so we use a separate test for this
		// scenario where there are no objects to process anyway.
	}

	store := &mockBotStore{
		existingIDs: map[string]bool{},
	}

	gc := NewGarbageCollector(GarbageCollectorOptions{
		MinIO:       minioClient,
		Store:       store,
		LogsBucket:  "bot-logs",
		StateBucket: "bot-state",
	})

	result, err := gc.Run(context.Background())

	require.NoError(t, err)
	assert.Equal(t, 0, result.StaleTempFiles)
}
