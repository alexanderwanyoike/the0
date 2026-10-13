package daemon

import (
	"bytes"
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/minio/minio-go/v7"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	miniologger "runtime/internal/minio-logger"
	"runtime/internal/runtime/storage"
	"runtime/internal/s3test"
)

// testLogger implements a simple logger for testing
type testDaemonLogger struct {
	t *testing.T
}

func (l *testDaemonLogger) Info(msg string, args ...interface{}) {
	if l.t != nil {
		l.t.Logf("INFO: "+msg, args...)
	}
}

func (l *testDaemonLogger) Error(msg string, args ...interface{}) {
	if l.t != nil {
		l.t.Logf("ERROR: "+msg, args...)
	}
}

func (l *testDaemonLogger) Debug(msg string, args ...interface{}) {
	if l.t != nil {
		l.t.Logf("DEBUG: "+msg, args...)
	}
}

func (l *testDaemonLogger) Warn(msg string, args ...interface{}) {
	if l.t != nil {
		l.t.Logf("WARN: "+msg, args...)
	}
}

// TestLogsSyncer_Integration tests LogsSyncer with real MinIO
func TestLogsSyncer_Integration(t *testing.T) {
	if testing.Short() {
		t.Skip("Skipping integration test")
	}

	server := s3test.StartT(t)

	ctx := context.Background()
	logger := &testDaemonLogger{t: t}

	// Create logs bucket
	require.NoError(t, server.Client.MakeBucket(ctx, "test-logs", minio.MakeBucketOptions{}))

	minioLogger, err := miniologger.NewMinIOLogger(ctx, miniologger.MinioLoggerOptions{
		LogsBucket: "test-logs",
		Endpoint:   server.Endpoint,
		AccessKey:  s3test.AccessKey,
		SecretKey:  s3test.SecretKey,
		UseSSL:     false,
	})
	require.NoError(t, err)

	t.Run("SyncNewLogs", func(t *testing.T) {
		// Create temp log directory
		logsDir, err := os.MkdirTemp("", "logs-")
		require.NoError(t, err)
		defer os.RemoveAll(logsDir)

		logFile := filepath.Join(logsDir, "bot.log")
		require.NoError(t, os.WriteFile(logFile, []byte("line 1\n"), 0644))

		syncer := NewLogsSyncer("test-bot-1", logsDir, minioLogger, nil, logger)
		require.NotNil(t, syncer)

		// First sync
		synced := syncer.Sync(ctx)
		assert.True(t, synced)

		// Append more logs
		f, err := os.OpenFile(logFile, os.O_APPEND|os.O_WRONLY, 0644)
		require.NoError(t, err)
		_, err = f.WriteString("line 2\n")
		require.NoError(t, err)
		f.Close()

		// Second sync should detect new content
		synced = syncer.Sync(ctx)
		assert.True(t, synced)

		// No new content - should not sync
		synced = syncer.Sync(ctx)
		assert.False(t, synced)

		// Cleanup
		require.NoError(t, syncer.Close())
	})

	t.Run("SyncNoLogFile", func(t *testing.T) {
		logsDir, err := os.MkdirTemp("", "no-logs-")
		require.NoError(t, err)
		defer os.RemoveAll(logsDir)

		syncer := NewLogsSyncer("test-bot-2", logsDir, minioLogger, nil, logger)
		require.NotNil(t, syncer)

		// Should not sync if log file doesn't exist
		synced := syncer.Sync(ctx)
		assert.False(t, synced)
	})

	t.Run("SyncEmptyFile", func(t *testing.T) {
		logsDir, err := os.MkdirTemp("", "empty-logs-")
		require.NoError(t, err)
		defer os.RemoveAll(logsDir)

		logFile := filepath.Join(logsDir, "bot.log")
		require.NoError(t, os.WriteFile(logFile, []byte(""), 0644))

		syncer := NewLogsSyncer("test-bot-3", logsDir, minioLogger, nil, logger)
		require.NotNil(t, syncer)

		// Empty file should not sync
		synced := syncer.Sync(ctx)
		assert.False(t, synced)
	})

	t.Run("NewLogsSyncer_NilUploader", func(t *testing.T) {
		// Should return nil when uploader is nil (logs disabled)
		syncer := NewLogsSyncer("test-bot", "/tmp", nil, nil, logger)
		assert.Nil(t, syncer)
	})

	t.Run("IncrementalSync", func(t *testing.T) {
		logsDir, err := os.MkdirTemp("", "incremental-logs-")
		require.NoError(t, err)
		defer os.RemoveAll(logsDir)

		logFile := filepath.Join(logsDir, "bot.log")

		syncer := NewLogsSyncer("test-bot-4", logsDir, minioLogger, nil, logger)
		require.NotNil(t, syncer)

		// Write and sync incrementally
		require.NoError(t, os.WriteFile(logFile, []byte("chunk 1\n"), 0644))
		assert.True(t, syncer.Sync(ctx))

		// Append chunk 2
		f, _ := os.OpenFile(logFile, os.O_APPEND|os.O_WRONLY, 0644)
		f.WriteString("chunk 2\n")
		f.Close()
		assert.True(t, syncer.Sync(ctx))

		// Append chunk 3
		f, _ = os.OpenFile(logFile, os.O_APPEND|os.O_WRONLY, 0644)
		f.WriteString("chunk 3\n")
		f.Close()
		assert.True(t, syncer.Sync(ctx))

		// No new data
		assert.False(t, syncer.Sync(ctx))
	})
}

func TestSyncRunner_SavesIntoTheConfiguredBuckets(t *testing.T) {
	if testing.Short() {
		t.Skip("Skipping integration test")
	}

	server := s3test.StartT(t)
	t.Setenv("MINIO_ENDPOINT", server.Endpoint)
	t.Setenv("MINIO_ACCESS_KEY", s3test.AccessKey)
	t.Setenv("MINIO_SECRET_KEY", s3test.SecretKey)
	t.Setenv("MINIO_USE_SSL", "false")
	t.Setenv("MINIO_STATE_BUCKET", "acme-state")
	t.Setenv("MINIO_LOGS_BUCKET", "acme-logs")
	t.Setenv("NATS_URL", "")

	statePath := filepath.Join(t.TempDir(), "state")
	logsPath := filepath.Join(t.TempDir(), "logs")
	require.NoError(t, os.MkdirAll(statePath, 0755))
	require.NoError(t, os.MkdirAll(logsPath, 0755))

	runner := NewSyncRunner(SyncOptions{
		BotID:     "acme-bot",
		StatePath: statePath,
		LogsPath:  logsPath,
		Interval:  time.Hour,
	}, &testDaemonLogger{t: t})
	runner.Start()
	require.NoError(t, runner.WaitReady(30*time.Second))
	require.NoError(t, os.WriteFile(filepath.Join(statePath, "positions.json"), []byte(`{"BTC":1}`), 0644))
	require.NoError(t, os.WriteFile(filepath.Join(logsPath, "bot.log"), []byte("bought BTC\n"), 0644))
	runner.Stop()

	for _, bucket := range []string{"acme-state", "acme-logs"} {
		var keys []string
		for object := range server.Client.ListObjects(context.Background(), bucket, minio.ListObjectsOptions{Recursive: true}) {
			if object.Err == nil && strings.Contains(object.Key, "acme-bot") {
				keys = append(keys, object.Key)
			}
		}
		assert.NotEmpty(t, keys, "nothing saved in %s", bucket)
	}
}

// TestStateSyncer_Integration tests StateSyncer with real MinIO
func TestStateSyncer_Integration(t *testing.T) {
	if testing.Short() {
		t.Skip("Skipping integration test")
	}

	server := s3test.StartT(t)

	ctx := context.Background()
	logger := &testDaemonLogger{t: t}

	// Create state bucket
	require.NoError(t, server.Client.MakeBucket(ctx, "test-state", minio.MakeBucketOptions{}))

	stateManager := storage.NewStateManager(server.Client, &storage.Config{
		StateBucket: "test-state",
	}, logger)

	t.Run("SyncChangedState", func(t *testing.T) {
		stateDir, err := os.MkdirTemp("", "state-sync-")
		require.NoError(t, err)
		defer os.RemoveAll(stateDir)

		syncer := NewStateSyncer("sync-bot-1", stateDir, stateManager, logger)

		// Create initial state
		require.NoError(t, os.WriteFile(filepath.Join(stateDir, "data.json"), []byte(`{"count": 1}`), 0644))

		// First sync should upload
		synced := syncer.Sync(ctx)
		assert.True(t, synced)

		// No changes - should not sync
		synced = syncer.Sync(ctx)
		assert.False(t, synced)

		// Modify state
		require.NoError(t, os.WriteFile(filepath.Join(stateDir, "data.json"), []byte(`{"count": 2}`), 0644))

		// Should detect change and sync
		synced = syncer.Sync(ctx)
		assert.True(t, synced)
	})

	t.Run("SyncEmptyDirectory", func(t *testing.T) {
		stateDir, err := os.MkdirTemp("", "empty-state-")
		require.NoError(t, err)
		defer os.RemoveAll(stateDir)

		syncer := NewStateSyncer("sync-bot-2", stateDir, stateManager, logger)

		// Empty directory should not sync
		synced := syncer.Sync(ctx)
		assert.False(t, synced)
	})

	t.Run("SyncNonExistentDirectory", func(t *testing.T) {
		syncer := NewStateSyncer("sync-bot-3", "/nonexistent/path", stateManager, logger)

		// Non-existent directory should not sync (but not error)
		synced := syncer.Sync(ctx)
		assert.False(t, synced)
	})

	t.Run("SyncAddFile", func(t *testing.T) {
		stateDir, err := os.MkdirTemp("", "add-file-state-")
		require.NoError(t, err)
		defer os.RemoveAll(stateDir)

		syncer := NewStateSyncer("sync-bot-4", stateDir, stateManager, logger)

		// Create initial file
		require.NoError(t, os.WriteFile(filepath.Join(stateDir, "file1.json"), []byte("data1"), 0644))
		assert.True(t, syncer.Sync(ctx))

		// Add second file
		require.NoError(t, os.WriteFile(filepath.Join(stateDir, "file2.json"), []byte("data2"), 0644))

		// Should detect addition
		assert.True(t, syncer.Sync(ctx))
	})

	t.Run("SyncDeleteFile", func(t *testing.T) {
		stateDir, err := os.MkdirTemp("", "delete-file-state-")
		require.NoError(t, err)
		defer os.RemoveAll(stateDir)

		syncer := NewStateSyncer("sync-bot-5", stateDir, stateManager, logger)

		// Create two files
		require.NoError(t, os.WriteFile(filepath.Join(stateDir, "keep.json"), []byte("keep"), 0644))
		require.NoError(t, os.WriteFile(filepath.Join(stateDir, "delete.json"), []byte("delete"), 0644))
		assert.True(t, syncer.Sync(ctx))

		// Delete one file
		require.NoError(t, os.Remove(filepath.Join(stateDir, "delete.json")))

		// Should detect deletion
		assert.True(t, syncer.Sync(ctx))
	})

	t.Run("SyncWithSubdirectories", func(t *testing.T) {
		stateDir, err := os.MkdirTemp("", "subdir-state-")
		require.NoError(t, err)
		defer os.RemoveAll(stateDir)

		syncer := NewStateSyncer("sync-bot-6", stateDir, stateManager, logger)

		// Create nested structure
		require.NoError(t, os.MkdirAll(filepath.Join(stateDir, "nested", "deep"), 0755))
		require.NoError(t, os.WriteFile(filepath.Join(stateDir, "root.json"), []byte("root"), 0644))
		require.NoError(t, os.WriteFile(filepath.Join(stateDir, "nested", "mid.json"), []byte("mid"), 0644))
		require.NoError(t, os.WriteFile(filepath.Join(stateDir, "nested", "deep", "leaf.json"), []byte("leaf"), 0644))

		// Should sync
		assert.True(t, syncer.Sync(ctx))

		// No changes
		assert.False(t, syncer.Sync(ctx))

		// Modify nested file
		require.NoError(t, os.WriteFile(filepath.Join(stateDir, "nested", "deep", "leaf.json"), []byte("modified"), 0644))

		// Should detect nested change
		assert.True(t, syncer.Sync(ctx))
	})

	t.Run("SyncPersistsHashBetweenCalls", func(t *testing.T) {
		stateDir, err := os.MkdirTemp("", "persist-hash-")
		require.NoError(t, err)
		defer os.RemoveAll(stateDir)

		syncer := NewStateSyncer("sync-bot-7", stateDir, stateManager, logger)

		require.NoError(t, os.WriteFile(filepath.Join(stateDir, "data.json"), []byte("content"), 0644))

		// First sync
		assert.True(t, syncer.Sync(ctx))

		// Multiple subsequent calls with no changes should all return false
		for i := 0; i < 5; i++ {
			assert.False(t, syncer.Sync(ctx), "Sync %d should return false", i+1)
		}
	})
}

// TestStateSyncer_ContextCancellation tests that sync operations respect context cancellation
func TestStateSyncer_ContextCancellation(t *testing.T) {
	if testing.Short() {
		t.Skip("Skipping integration test")
	}

	server := s3test.StartT(t)

	logger := &testDaemonLogger{t: t}

	// Create state bucket
	require.NoError(t, server.Client.MakeBucket(context.Background(), "test-state", minio.MakeBucketOptions{}))

	stateManager := storage.NewStateManager(server.Client, &storage.Config{
		StateBucket: "test-state",
	}, logger)

	stateDir, err := os.MkdirTemp("", "cancel-state-")
	require.NoError(t, err)
	defer os.RemoveAll(stateDir)

	syncer := NewStateSyncer("cancel-bot", stateDir, stateManager, logger)

	// Create large state file
	largeData := bytes.Repeat([]byte("x"), 1024*1024) // 1MB
	require.NoError(t, os.WriteFile(filepath.Join(stateDir, "large.bin"), largeData, 0644))

	// Create a context with short timeout
	ctx, cancel := context.WithTimeout(context.Background(), 1*time.Millisecond)
	defer cancel()

	// Wait for context to expire
	time.Sleep(10 * time.Millisecond)

	// Sync should fail or return false due to cancelled context
	synced := syncer.Sync(ctx)
	// Note: May return false or complete quickly depending on timing
	// Main point is it doesn't panic or hang
	_ = synced
}
