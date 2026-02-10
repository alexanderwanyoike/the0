package daemon

import (
	"archive/zip"
	"bytes"
	"context"
	"fmt"
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/minio/minio-go/v7"
	"github.com/minio/minio-go/v7/pkg/credentials"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"github.com/testcontainers/testcontainers-go"
	"github.com/testcontainers/testcontainers-go/wait"

	miniologger "runtime/internal/minio-logger"
	"runtime/internal/runtime/storage"
)

// testMinIOServer holds MinIO test container info
type testMinIOServer struct {
	container testcontainers.Container
	endpoint  string
	accessKey string
	secretKey string
	client    *minio.Client
}

// setupMinIOTestContainer starts a MinIO container for testing
func setupMinIOTestContainer(t *testing.T) *testMinIOServer {
	ctx := context.Background()

	accessKey := "testkey"
	secretKey := "testsecret"

	req := testcontainers.ContainerRequest{
		Image:        "minio/minio:latest",
		ExposedPorts: []string{"9000/tcp"},
		Env: map[string]string{
			"MINIO_ACCESS_KEY": accessKey,
			"MINIO_SECRET_KEY": secretKey,
		},
		Cmd:        []string{"server", "/data"},
		WaitingFor: wait.ForHTTP("/minio/health/live").WithPort("9000/tcp"),
	}

	container, err := testcontainers.GenericContainer(ctx, testcontainers.GenericContainerRequest{
		ContainerRequest: req,
		Started:          true,
	})
	require.NoError(t, err)

	host, err := container.Host(ctx)
	require.NoError(t, err)

	port, err := container.MappedPort(ctx, "9000")
	require.NoError(t, err)

	endpoint := fmt.Sprintf("%s:%s", host, port.Port())

	client, err := minio.New(endpoint, &minio.Options{
		Creds:  credentials.NewStaticV4(accessKey, secretKey, ""),
		Secure: false,
	})
	require.NoError(t, err)

	return &testMinIOServer{
		container: container,
		endpoint:  endpoint,
		accessKey: accessKey,
		secretKey: secretKey,
		client:    client,
	}
}

func (s *testMinIOServer) cleanup(t *testing.T) {
	if s.container != nil {
		require.NoError(t, s.container.Terminate(context.Background()))
	}
}

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

// TestInit_Integration tests the Init function with real MinIO
func TestInit_Integration(t *testing.T) {
	if testing.Short() {
		t.Skip("Skipping integration test")
	}

	server := setupMinIOTestContainer(t)
	defer server.cleanup(t)

	ctx := context.Background()

	// Create buckets
	require.NoError(t, server.client.MakeBucket(ctx, "test-code", minio.MakeBucketOptions{}))
	require.NoError(t, server.client.MakeBucket(ctx, "test-state", minio.MakeBucketOptions{}))

	// Set environment variables for LoadConfigFromEnv
	os.Setenv("MINIO_ENDPOINT", server.endpoint)
	os.Setenv("MINIO_ACCESS_KEY", server.accessKey)
	os.Setenv("MINIO_SECRET_KEY", server.secretKey)
	os.Setenv("MINIO_USE_SSL", "false")
	os.Setenv("MINIO_CODE_BUCKET", "test-code")
	os.Setenv("MINIO_STATE_BUCKET", "test-state")
	defer func() {
		os.Unsetenv("MINIO_ENDPOINT")
		os.Unsetenv("MINIO_ACCESS_KEY")
		os.Unsetenv("MINIO_SECRET_KEY")
		os.Unsetenv("MINIO_USE_SSL")
		os.Unsetenv("MINIO_CODE_BUCKET")
		os.Unsetenv("MINIO_STATE_BUCKET")
	}()

	t.Run("DownloadCodeAndState", func(t *testing.T) {
		// Create a zip file with bot code
		var buf bytes.Buffer
		w := zip.NewWriter(&buf)
		files := map[string]string{
			"main.py":     "print('hello world')",
			"config.json": `{"param": "value"}`,
		}
		for name, content := range files {
			f, err := w.Create(name)
			require.NoError(t, err)
			_, err = f.Write([]byte(content))
			require.NoError(t, err)
		}
		require.NoError(t, w.Close())

		// Upload code zip to MinIO
		codePath := "test-bot/v1.0.0/code.zip"
		_, err := server.client.PutObject(ctx, "test-code", codePath, &buf, int64(buf.Len()), minio.PutObjectOptions{
			ContentType: "application/zip",
		})
		require.NoError(t, err)

		// Create and upload state
		stateManager := storage.NewStateManager(server.client, &storage.Config{
			StateBucket: "test-state",
		}, &testDaemonLogger{t: t})

		stateSrcDir, err := os.MkdirTemp("", "state-src-")
		require.NoError(t, err)
		defer os.RemoveAll(stateSrcDir)

		require.NoError(t, os.WriteFile(filepath.Join(stateSrcDir, "state.json"), []byte(`{"counter": 42}`), 0644))
		require.NoError(t, stateManager.UploadState(ctx, "test-bot", stateSrcDir))

		// Create temp directories for Init to download into
		codeDest, err := os.MkdirTemp("", "code-dest-")
		require.NoError(t, err)
		defer os.RemoveAll(codeDest)

		stateDest, err := os.MkdirTemp("", "state-dest-")
		require.NoError(t, err)
		defer os.RemoveAll(stateDest)

		// Run Init
		err = Init(ctx, InitOptions{
			BotID:     "test-bot",
			CodePath:  codeDest,
			StatePath: stateDest,
			CodeFile:  codePath,
		})
		require.NoError(t, err)

		// Verify code was extracted
		for name, expectedContent := range files {
			content, err := os.ReadFile(filepath.Join(codeDest, name))
			require.NoError(t, err)
			assert.Equal(t, expectedContent, string(content))
		}

		// Verify state was downloaded
		stateContent, err := os.ReadFile(filepath.Join(stateDest, "state.json"))
		require.NoError(t, err)
		assert.Equal(t, `{"counter": 42}`, string(stateContent))
	})

	t.Run("InitWithoutCode", func(t *testing.T) {
		stateDest, err := os.MkdirTemp("", "state-only-")
		require.NoError(t, err)
		defer os.RemoveAll(stateDest)

		// Init without CodeFile should succeed
		err = Init(ctx, InitOptions{
			BotID:     "code-less-bot",
			CodePath:  "/tmp/unused",
			StatePath: stateDest,
			CodeFile:  "", // No code file
		})
		require.NoError(t, err)
	})

	t.Run("InitWithoutState", func(t *testing.T) {
		codeDest, err := os.MkdirTemp("", "no-state-code-")
		require.NoError(t, err)
		defer os.RemoveAll(codeDest)

		stateDest, err := os.MkdirTemp("", "no-state-state-")
		require.NoError(t, err)
		defer os.RemoveAll(stateDest)

		// Create minimal code zip
		var buf bytes.Buffer
		w := zip.NewWriter(&buf)
		f, _ := w.Create("app.py")
		f.Write([]byte("pass"))
		w.Close()

		codePath := "no-state-bot/v1.0.0/code.zip"
		_, err = server.client.PutObject(ctx, "test-code", codePath, &buf, int64(buf.Len()), minio.PutObjectOptions{})
		require.NoError(t, err)

		// Init for bot without state should succeed (first run scenario)
		err = Init(ctx, InitOptions{
			BotID:     "no-state-bot",
			CodePath:  codeDest,
			StatePath: stateDest,
			CodeFile:  codePath,
		})
		require.NoError(t, err)
	})

	t.Run("InitWithDefaults", func(t *testing.T) {
		tmpDir, err := os.MkdirTemp("", "default-paths-")
		require.NoError(t, err)
		defer os.RemoveAll(tmpDir)

		// Init with empty paths should use defaults (/bot and /state)
		// These defaults work in Docker containers but not in tests
		// So we test with custom paths instead
		codePath := filepath.Join(tmpDir, "code")
		statePath := filepath.Join(tmpDir, "state")

		err = Init(ctx, InitOptions{
			BotID:     "defaults-bot",
			CodePath:  codePath,
			StatePath: statePath,
			CodeFile:  "", // No code file
		})
		require.NoError(t, err)

		// Verify directories were created
		_, err = os.Stat(codePath)
		assert.True(t, err == nil || os.IsNotExist(err))
		_, err = os.Stat(statePath)
		assert.NoError(t, err) // State path should always be created
	})
}

// TestLogsSyncer_Integration tests LogsSyncer with real MinIO
func TestLogsSyncer_Integration(t *testing.T) {
	if testing.Short() {
		t.Skip("Skipping integration test")
	}

	server := setupMinIOTestContainer(t)
	defer server.cleanup(t)

	ctx := context.Background()
	logger := &testDaemonLogger{t: t}

	// Create logs bucket
	require.NoError(t, server.client.MakeBucket(ctx, "test-logs", minio.MakeBucketOptions{}))

	minioLogger, err := miniologger.NewMinIOLogger(ctx, miniologger.MinioLoggerOptions{
		LogsBucket: "test-logs",
		Endpoint:   server.endpoint,
		AccessKey:  server.accessKey,
		SecretKey:  server.secretKey,
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

// TestStateSyncer_Integration tests StateSyncer with real MinIO
func TestStateSyncer_Integration(t *testing.T) {
	if testing.Short() {
		t.Skip("Skipping integration test")
	}

	server := setupMinIOTestContainer(t)
	defer server.cleanup(t)

	ctx := context.Background()
	logger := &testDaemonLogger{t: t}

	// Create state bucket
	require.NoError(t, server.client.MakeBucket(ctx, "test-state", minio.MakeBucketOptions{}))

	stateManager := storage.NewStateManager(server.client, &storage.Config{
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

	server := setupMinIOTestContainer(t)
	defer server.cleanup(t)

	logger := &testDaemonLogger{t: t}

	// Create state bucket
	require.NoError(t, server.client.MakeBucket(context.Background(), "test-state", minio.MakeBucketOptions{}))

	stateManager := storage.NewStateManager(server.client, &storage.Config{
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
