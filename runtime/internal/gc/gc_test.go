package gc

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// mockMinIOClient implements the MinIOClient interface for testing
type mockMinIOClient struct {
	logObjects   []string // object names in logs bucket
	stateObjects []string // object names in state bucket
	deleted      []string // track deleted object paths (bucket:name)
	listErr      error
	removeErr    error
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

	gc := NewGarbageCollector(minio, store, "bot-logs", "bot-state", nil)
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

	gc := NewGarbageCollector(minio, store, "bot-logs", "bot-state", nil)
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

	gc := NewGarbageCollector(minio, store, "bot-logs", "bot-state", nil)
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

	gc := NewGarbageCollector(minio, store, "bot-logs", "bot-state", nil)
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

	gc := NewGarbageCollector(minio, store, "bot-logs", "bot-state", nil)
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

	gc := NewGarbageCollector(minio, store, "bot-logs", "bot-state", nil)
	result, err := gc.Run(context.Background())

	// Should not return error -- removal failures are logged, not fatal
	require.NoError(t, err)
	assert.Equal(t, 1, result.OrphanedBots)
	assert.Equal(t, 0, result.DeletedObjects) // none succeeded
}
