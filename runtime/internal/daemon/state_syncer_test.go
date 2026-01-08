package daemon

import (
	"context"
	"os"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"runtime/internal/util"
)

// MockStateManager implements storage.StateManager for testing.
type MockStateManager struct {
	UploadCalls []struct {
		BotID     string
		StatePath string
	}
	UploadError      error
	DownloadError    error
	DeleteError      error
	StateExistsValue bool
	ExistsError      error
}

func (m *MockStateManager) UploadState(ctx context.Context, botID, statePath string) error {
	m.UploadCalls = append(m.UploadCalls, struct {
		BotID     string
		StatePath string
	}{BotID: botID, StatePath: statePath})
	return m.UploadError
}

func (m *MockStateManager) DownloadState(ctx context.Context, botID, statePath string) error {
	return m.DownloadError
}

func (m *MockStateManager) DeleteState(ctx context.Context, botID string) error {
	return m.DeleteError
}

func (m *MockStateManager) StateExists(ctx context.Context, botID string) (bool, error) {
	return m.StateExistsValue, m.ExistsError
}

func TestNewStateSyncer(t *testing.T) {
	mockManager := &MockStateManager{}
	syncer := NewStateSyncer("bot-1", "/state", mockManager, &util.DefaultLogger{})

	require.NotNil(t, syncer)
	assert.Equal(t, "bot-1", syncer.botID)
	assert.Equal(t, "/state", syncer.statePath)
	assert.Empty(t, syncer.lastHash)
}

func TestStateSyncer_Sync_NonExistentDirectory(t *testing.T) {
	mockManager := &MockStateManager{}
	syncer := NewStateSyncer("bot-1", "/non-existent-path-12345", mockManager, &util.DefaultLogger{})

	synced := syncer.Sync(context.Background())
	assert.False(t, synced, "should return false for non-existent directory")
	assert.Empty(t, mockManager.UploadCalls)
}

func TestStateSyncer_Sync_EmptyDirectory(t *testing.T) {
	tmpDir := t.TempDir()
	mockManager := &MockStateManager{}
	syncer := NewStateSyncer("bot-1", tmpDir, mockManager, &util.DefaultLogger{})

	synced := syncer.Sync(context.Background())
	assert.False(t, synced, "should return false for empty directory")
	assert.Empty(t, mockManager.UploadCalls)
}

func TestStateSyncer_Sync_FirstSync(t *testing.T) {
	tmpDir := t.TempDir()

	// Create state file
	stateFile := filepath.Join(tmpDir, "state.json")
	require.NoError(t, os.WriteFile(stateFile, []byte(`{"key": "value"}`), 0644))

	mockManager := &MockStateManager{}
	syncer := NewStateSyncer("bot-1", tmpDir, mockManager, &util.DefaultLogger{})

	synced := syncer.Sync(context.Background())
	assert.True(t, synced, "should return true for first sync")
	require.Len(t, mockManager.UploadCalls, 1)
	assert.Equal(t, "bot-1", mockManager.UploadCalls[0].BotID)
	assert.Equal(t, tmpDir, mockManager.UploadCalls[0].StatePath)
}

func TestStateSyncer_Sync_NoChanges(t *testing.T) {
	tmpDir := t.TempDir()

	// Create state file
	stateFile := filepath.Join(tmpDir, "state.json")
	require.NoError(t, os.WriteFile(stateFile, []byte(`{"key": "value"}`), 0644))

	mockManager := &MockStateManager{}
	syncer := NewStateSyncer("bot-1", tmpDir, mockManager, &util.DefaultLogger{})

	// First sync
	syncer.Sync(context.Background())
	require.Len(t, mockManager.UploadCalls, 1)

	// Second sync without changes
	synced := syncer.Sync(context.Background())
	assert.False(t, synced, "should return false when no changes")
	assert.Len(t, mockManager.UploadCalls, 1, "should not upload when no changes")
}

func TestStateSyncer_Sync_ContentChanged(t *testing.T) {
	tmpDir := t.TempDir()

	// Create state file
	stateFile := filepath.Join(tmpDir, "state.json")
	require.NoError(t, os.WriteFile(stateFile, []byte(`{"key": "value1"}`), 0644))

	mockManager := &MockStateManager{}
	syncer := NewStateSyncer("bot-1", tmpDir, mockManager, &util.DefaultLogger{})

	// First sync
	syncer.Sync(context.Background())
	require.Len(t, mockManager.UploadCalls, 1)

	// Modify content
	require.NoError(t, os.WriteFile(stateFile, []byte(`{"key": "value2"}`), 0644))

	// Second sync should detect change
	synced := syncer.Sync(context.Background())
	assert.True(t, synced, "should return true when content changed")
	assert.Len(t, mockManager.UploadCalls, 2)
}

func TestStateSyncer_Sync_NewFileAdded(t *testing.T) {
	tmpDir := t.TempDir()

	// Create initial state file
	stateFile := filepath.Join(tmpDir, "state.json")
	require.NoError(t, os.WriteFile(stateFile, []byte(`{"key": "value"}`), 0644))

	mockManager := &MockStateManager{}
	syncer := NewStateSyncer("bot-1", tmpDir, mockManager, &util.DefaultLogger{})

	// First sync
	syncer.Sync(context.Background())
	require.Len(t, mockManager.UploadCalls, 1)

	// Add new file
	newFile := filepath.Join(tmpDir, "data.json")
	require.NoError(t, os.WriteFile(newFile, []byte(`{"data": "value"}`), 0644))

	// Second sync should detect change
	synced := syncer.Sync(context.Background())
	assert.True(t, synced, "should return true when new file added")
	assert.Len(t, mockManager.UploadCalls, 2)
}

func TestStateSyncer_Sync_UploadError(t *testing.T) {
	tmpDir := t.TempDir()

	// Create state file
	stateFile := filepath.Join(tmpDir, "state.json")
	require.NoError(t, os.WriteFile(stateFile, []byte(`{"key": "value"}`), 0644))

	mockManager := &MockStateManager{
		UploadError: assert.AnError,
	}
	syncer := NewStateSyncer("bot-1", tmpDir, mockManager, &util.DefaultLogger{})

	synced := syncer.Sync(context.Background())
	assert.False(t, synced, "should return false on upload error")
	assert.Empty(t, syncer.lastHash, "should not update lastHash on error")
}

func TestStateSyncer_Sync_NestedDirectories(t *testing.T) {
	tmpDir := t.TempDir()

	// Create nested structure
	nestedDir := filepath.Join(tmpDir, "subdir", "nested")
	require.NoError(t, os.MkdirAll(nestedDir, 0755))
	require.NoError(t, os.WriteFile(filepath.Join(tmpDir, "root.json"), []byte(`{}`), 0644))
	require.NoError(t, os.WriteFile(filepath.Join(nestedDir, "deep.json"), []byte(`{}`), 0644))

	mockManager := &MockStateManager{}
	syncer := NewStateSyncer("bot-1", tmpDir, mockManager, &util.DefaultLogger{})

	synced := syncer.Sync(context.Background())
	assert.True(t, synced, "should sync nested directories")
	require.Len(t, mockManager.UploadCalls, 1)
}

func TestStateSyncer_HashDirectory_ConsistentOrdering(t *testing.T) {
	tmpDir := t.TempDir()

	// Create multiple files
	require.NoError(t, os.WriteFile(filepath.Join(tmpDir, "a.json"), []byte(`{}`), 0644))
	require.NoError(t, os.WriteFile(filepath.Join(tmpDir, "b.json"), []byte(`{}`), 0644))
	require.NoError(t, os.WriteFile(filepath.Join(tmpDir, "c.json"), []byte(`{}`), 0644))

	syncer := NewStateSyncer("bot-1", tmpDir, nil, &util.DefaultLogger{})

	// Hash should be consistent across multiple calls
	hash1, err := syncer.hashDirectory()
	require.NoError(t, err)

	hash2, err := syncer.hashDirectory()
	require.NoError(t, err)

	assert.Equal(t, hash1, hash2, "hash should be consistent")
	assert.NotEmpty(t, hash1)
}
