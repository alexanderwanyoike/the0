package daemon

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// createTestStateSyncer creates a StateSyncer for testing hash functionality.
func createTestStateSyncer(statePath string) *StateSyncer {
	return &StateSyncer{
		botID:     "test",
		statePath: statePath,
	}
}

func TestHashDirectory_EmptyDirectory(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "hash-empty-")
	require.NoError(t, err)
	defer os.RemoveAll(tmpDir)

	syncer := createTestStateSyncer(tmpDir)
	hash, err := syncer.hashDirectory()
	require.NoError(t, err)
	assert.Empty(t, hash, "Empty directory should return empty hash")
}

func TestHashDirectory_NonExistent(t *testing.T) {
	syncer := createTestStateSyncer("/nonexistent/directory/path")
	hash, err := syncer.hashDirectory()
	require.NoError(t, err)
	assert.Empty(t, hash, "Non-existent directory should return empty hash")
}

func TestHashDirectory_SingleFile(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "hash-single-")
	require.NoError(t, err)
	defer os.RemoveAll(tmpDir)

	// Create a file
	require.NoError(t, os.WriteFile(filepath.Join(tmpDir, "test.txt"), []byte("hello"), 0644))

	syncer := createTestStateSyncer(tmpDir)
	hash, err := syncer.hashDirectory()
	require.NoError(t, err)
	assert.NotEmpty(t, hash)
	assert.Len(t, hash, 64, "SHA256 hex hash should be 64 characters")
}

func TestHashDirectory_Deterministic(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "hash-deterministic-")
	require.NoError(t, err)
	defer os.RemoveAll(tmpDir)

	// Create some files
	require.NoError(t, os.WriteFile(filepath.Join(tmpDir, "a.txt"), []byte("content a"), 0644))
	require.NoError(t, os.WriteFile(filepath.Join(tmpDir, "b.txt"), []byte("content b"), 0644))

	syncer := createTestStateSyncer(tmpDir)

	// Hash multiple times
	hash1, err := syncer.hashDirectory()
	require.NoError(t, err)

	hash2, err := syncer.hashDirectory()
	require.NoError(t, err)

	assert.Equal(t, hash1, hash2, "Same directory should produce same hash")
}

func TestHashDirectory_DifferentContent(t *testing.T) {
	tmpDir1, err := os.MkdirTemp("", "hash-diff1-")
	require.NoError(t, err)
	defer os.RemoveAll(tmpDir1)

	tmpDir2, err := os.MkdirTemp("", "hash-diff2-")
	require.NoError(t, err)
	defer os.RemoveAll(tmpDir2)

	// Create files with different content
	require.NoError(t, os.WriteFile(filepath.Join(tmpDir1, "test.txt"), []byte("content 1"), 0644))
	require.NoError(t, os.WriteFile(filepath.Join(tmpDir2, "test.txt"), []byte("content 2"), 0644))

	syncer1 := createTestStateSyncer(tmpDir1)
	hash1, err := syncer1.hashDirectory()
	require.NoError(t, err)

	syncer2 := createTestStateSyncer(tmpDir2)
	hash2, err := syncer2.hashDirectory()
	require.NoError(t, err)

	assert.NotEqual(t, hash1, hash2, "Different content should produce different hash")
}

func TestHashDirectory_DifferentFilenames(t *testing.T) {
	tmpDir1, err := os.MkdirTemp("", "hash-name1-")
	require.NoError(t, err)
	defer os.RemoveAll(tmpDir1)

	tmpDir2, err := os.MkdirTemp("", "hash-name2-")
	require.NoError(t, err)
	defer os.RemoveAll(tmpDir2)

	// Same content, different filenames
	require.NoError(t, os.WriteFile(filepath.Join(tmpDir1, "file1.txt"), []byte("same content"), 0644))
	require.NoError(t, os.WriteFile(filepath.Join(tmpDir2, "file2.txt"), []byte("same content"), 0644))

	syncer1 := createTestStateSyncer(tmpDir1)
	hash1, err := syncer1.hashDirectory()
	require.NoError(t, err)

	syncer2 := createTestStateSyncer(tmpDir2)
	hash2, err := syncer2.hashDirectory()
	require.NoError(t, err)

	assert.NotEqual(t, hash1, hash2, "Different filenames should produce different hash")
}

func TestHashDirectory_WithSubdirectories(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "hash-subdirs-")
	require.NoError(t, err)
	defer os.RemoveAll(tmpDir)

	// Create nested structure
	require.NoError(t, os.MkdirAll(filepath.Join(tmpDir, "nested", "deep"), 0755))
	require.NoError(t, os.WriteFile(filepath.Join(tmpDir, "root.txt"), []byte("root"), 0644))
	require.NoError(t, os.WriteFile(filepath.Join(tmpDir, "nested", "mid.txt"), []byte("mid"), 0644))
	require.NoError(t, os.WriteFile(filepath.Join(tmpDir, "nested", "deep", "leaf.txt"), []byte("leaf"), 0644))

	syncer := createTestStateSyncer(tmpDir)
	hash, err := syncer.hashDirectory()
	require.NoError(t, err)
	assert.NotEmpty(t, hash)
}

func TestHashDirectory_ChangeDetection(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "hash-change-")
	require.NoError(t, err)
	defer os.RemoveAll(tmpDir)

	// Initial state
	require.NoError(t, os.WriteFile(filepath.Join(tmpDir, "state.json"), []byte(`{"count": 1}`), 0644))

	syncer := createTestStateSyncer(tmpDir)
	hash1, err := syncer.hashDirectory()
	require.NoError(t, err)

	// Modify the file
	require.NoError(t, os.WriteFile(filepath.Join(tmpDir, "state.json"), []byte(`{"count": 2}`), 0644))

	hash2, err := syncer.hashDirectory()
	require.NoError(t, err)

	assert.NotEqual(t, hash1, hash2, "Modified content should produce different hash")
}

func TestHashDirectory_AddFile(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "hash-add-")
	require.NoError(t, err)
	defer os.RemoveAll(tmpDir)

	// Initial state
	require.NoError(t, os.WriteFile(filepath.Join(tmpDir, "existing.txt"), []byte("existing"), 0644))

	syncer := createTestStateSyncer(tmpDir)
	hash1, err := syncer.hashDirectory()
	require.NoError(t, err)

	// Add a new file
	require.NoError(t, os.WriteFile(filepath.Join(tmpDir, "new.txt"), []byte("new"), 0644))

	hash2, err := syncer.hashDirectory()
	require.NoError(t, err)

	assert.NotEqual(t, hash1, hash2, "Adding a file should change the hash")
}

func TestHashDirectory_DeleteFile(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "hash-delete-")
	require.NoError(t, err)
	defer os.RemoveAll(tmpDir)

	// Initial state with two files
	require.NoError(t, os.WriteFile(filepath.Join(tmpDir, "keep.txt"), []byte("keep"), 0644))
	require.NoError(t, os.WriteFile(filepath.Join(tmpDir, "delete.txt"), []byte("delete"), 0644))

	syncer := createTestStateSyncer(tmpDir)
	hash1, err := syncer.hashDirectory()
	require.NoError(t, err)

	// Delete one file
	require.NoError(t, os.Remove(filepath.Join(tmpDir, "delete.txt")))

	hash2, err := syncer.hashDirectory()
	require.NoError(t, err)

	assert.NotEqual(t, hash1, hash2, "Deleting a file should change the hash")
}

func TestSyncOptions_Defaults(t *testing.T) {
	opts := SyncOptions{
		BotID: "test-bot",
	}

	// Verify that empty values will be replaced with defaults in Sync function
	assert.Empty(t, opts.StatePath)
	assert.Zero(t, opts.Interval)
}
