package storage

import (
	"archive/zip"
	"bytes"
	"context"
	"fmt"
	"os"
	"path/filepath"
	"testing"

	"github.com/minio/minio-go/v7"
	"github.com/minio/minio-go/v7/pkg/credentials"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"github.com/testcontainers/testcontainers-go"
	"github.com/testcontainers/testcontainers-go/wait"
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
type integrationTestLogger struct {
	t *testing.T
}

func (l *integrationTestLogger) Info(msg string, args ...interface{}) {
	if l.t != nil {
		l.t.Logf("INFO: "+msg, args...)
	}
}

// TestStateManager_Integration tests StateManager with real MinIO
func TestStateManager_Integration(t *testing.T) {
	if testing.Short() {
		t.Skip("Skipping integration test")
	}

	server := setupMinIOTestContainer(t)
	defer server.cleanup(t)

	cfg := &Config{
		Endpoint:              server.endpoint,
		AccessKey:             server.accessKey,
		SecretKey:             server.secretKey,
		UseSSL:                false,
		StateBucket:           "test-state",
		MaxStateSizeBytes:     8 * 1024 * 1024 * 1024,
		MaxStateFileSizeBytes: 10 * 1024 * 1024,
	}

	logger := &integrationTestLogger{t: t}
	stateManager := NewStateManager(server.client, cfg, logger)

	t.Run("UploadAndDownloadState", func(t *testing.T) {
		ctx := context.Background()
		botID := "test-bot-123"

		// Create temp directory with state files
		srcDir, err := os.MkdirTemp("", "state-src-")
		require.NoError(t, err)
		defer os.RemoveAll(srcDir)

		// Create test state files
		testFiles := map[string]string{
			"portfolio.json": `{"AAPL": 100, "GOOGL": 50}`,
			"counters.json":  `{"trades": 42}`,
		}
		for name, content := range testFiles {
			require.NoError(t, os.WriteFile(filepath.Join(srcDir, name), []byte(content), 0644))
		}

		// Upload state
		err = stateManager.UploadState(ctx, botID, srcDir)
		require.NoError(t, err)

		// Verify state exists
		exists, err := stateManager.StateExists(ctx, botID)
		require.NoError(t, err)
		assert.True(t, exists)

		// Download to a new directory
		destDir, err := os.MkdirTemp("", "state-dest-")
		require.NoError(t, err)
		defer os.RemoveAll(destDir)

		err = stateManager.DownloadState(ctx, botID, destDir)
		require.NoError(t, err)

		// Verify files were downloaded
		for name, expectedContent := range testFiles {
			content, err := os.ReadFile(filepath.Join(destDir, name))
			require.NoError(t, err)
			assert.Equal(t, expectedContent, string(content))
		}
	})

	t.Run("DeleteState", func(t *testing.T) {
		ctx := context.Background()
		botID := "delete-test-bot"

		// Create and upload state
		srcDir, err := os.MkdirTemp("", "delete-state-")
		require.NoError(t, err)
		defer os.RemoveAll(srcDir)

		require.NoError(t, os.WriteFile(filepath.Join(srcDir, "data.json"), []byte("{}"), 0644))
		require.NoError(t, stateManager.UploadState(ctx, botID, srcDir))

		// Verify exists
		exists, err := stateManager.StateExists(ctx, botID)
		require.NoError(t, err)
		assert.True(t, exists)

		// Delete
		err = stateManager.DeleteState(ctx, botID)
		require.NoError(t, err)

		// Verify deleted
		exists, err = stateManager.StateExists(ctx, botID)
		require.NoError(t, err)
		assert.False(t, exists)
	})

	t.Run("DownloadNonExistentState", func(t *testing.T) {
		ctx := context.Background()
		botID := "nonexistent-bot"

		destDir, err := os.MkdirTemp("", "nonexistent-state-")
		require.NoError(t, err)
		defer os.RemoveAll(destDir)

		// StateExists should return false for non-existent state
		exists, err := stateManager.StateExists(ctx, botID)
		require.NoError(t, err)
		assert.False(t, exists)

		// DownloadState returns nil for non-existent state (graceful first-run handling)
		// This allows bots to start without pre-existing state
		err = stateManager.DownloadState(ctx, botID, destDir)
		require.NoError(t, err, "DownloadState should not error for first-run scenario")

		// Destination directory should be created but empty
		entries, err := os.ReadDir(destDir)
		require.NoError(t, err)
		assert.Empty(t, entries, "Directory should be empty after downloading non-existent state")
	})

	t.Run("EmptyStateDirectory", func(t *testing.T) {
		ctx := context.Background()
		botID := "empty-state-bot"

		// Create empty directory
		srcDir, err := os.MkdirTemp("", "empty-state-")
		require.NoError(t, err)
		defer os.RemoveAll(srcDir)

		// Upload empty state should succeed (nothing to upload)
		err = stateManager.UploadState(ctx, botID, srcDir)
		require.NoError(t, err)

		// State should not exist (nothing was uploaded)
		exists, err := stateManager.StateExists(ctx, botID)
		require.NoError(t, err)
		assert.False(t, exists)
	})

	t.Run("StateWithSubdirectories", func(t *testing.T) {
		ctx := context.Background()
		botID := "subdirs-bot"

		// Create directory with subdirs
		srcDir, err := os.MkdirTemp("", "subdir-state-")
		require.NoError(t, err)
		defer os.RemoveAll(srcDir)

		require.NoError(t, os.MkdirAll(filepath.Join(srcDir, "nested", "deep"), 0755))
		require.NoError(t, os.WriteFile(filepath.Join(srcDir, "root.json"), []byte("root"), 0644))
		require.NoError(t, os.WriteFile(filepath.Join(srcDir, "nested", "mid.json"), []byte("mid"), 0644))
		require.NoError(t, os.WriteFile(filepath.Join(srcDir, "nested", "deep", "leaf.json"), []byte("leaf"), 0644))

		// Upload
		err = stateManager.UploadState(ctx, botID, srcDir)
		require.NoError(t, err)

		// Download to new location
		destDir, err := os.MkdirTemp("", "subdir-dest-")
		require.NoError(t, err)
		defer os.RemoveAll(destDir)

		err = stateManager.DownloadState(ctx, botID, destDir)
		require.NoError(t, err)

		// Verify all files
		content, err := os.ReadFile(filepath.Join(destDir, "root.json"))
		require.NoError(t, err)
		assert.Equal(t, "root", string(content))

		content, err = os.ReadFile(filepath.Join(destDir, "nested", "mid.json"))
		require.NoError(t, err)
		assert.Equal(t, "mid", string(content))

		content, err = os.ReadFile(filepath.Join(destDir, "nested", "deep", "leaf.json"))
		require.NoError(t, err)
		assert.Equal(t, "leaf", string(content))
	})
}

// TestCodeManager_Integration tests CodeManager with real MinIO
func TestCodeManager_Integration(t *testing.T) {
	if testing.Short() {
		t.Skip("Skipping integration test")
	}

	server := setupMinIOTestContainer(t)
	defer server.cleanup(t)

	cfg := &Config{
		Endpoint:   server.endpoint,
		AccessKey:  server.accessKey,
		SecretKey:  server.secretKey,
		UseSSL:     false,
		CodeBucket: "test-code",
	}

	logger := &integrationTestLogger{t: t}
	codeManager := NewCodeManager(server.client, cfg, logger)

	// Create the bucket
	ctx := context.Background()
	err := server.client.MakeBucket(ctx, "test-code", minio.MakeBucketOptions{})
	require.NoError(t, err)

	t.Run("DownloadAndExtractZip", func(t *testing.T) {
		// Create a zip file in memory
		var buf bytes.Buffer
		w := zip.NewWriter(&buf)

		files := map[string]string{
			"main.py":     "print('hello')",
			"config.json": `{"key": "value"}`,
		}
		for name, content := range files {
			f, err := w.Create(name)
			require.NoError(t, err)
			_, err = f.Write([]byte(content))
			require.NoError(t, err)
		}
		require.NoError(t, w.Close())

		// Upload zip to MinIO
		objectPath := "my-bot/v1.0.0/code.zip"
		_, err := server.client.PutObject(ctx, "test-code", objectPath, &buf, int64(buf.Len()), minio.PutObjectOptions{
			ContentType: "application/zip",
		})
		require.NoError(t, err)

		// Download and extract
		destDir, err := os.MkdirTemp("", "code-dest-")
		require.NoError(t, err)
		defer os.RemoveAll(destDir)

		err = codeManager.DownloadAndExtract(ctx, objectPath, destDir)
		require.NoError(t, err)

		// Verify files were extracted
		for name, expectedContent := range files {
			content, err := os.ReadFile(filepath.Join(destDir, name))
			require.NoError(t, err)
			assert.Equal(t, expectedContent, string(content))
		}
	})

	t.Run("DownloadNonExistentCode", func(t *testing.T) {
		destDir, err := os.MkdirTemp("", "nonexistent-code-")
		require.NoError(t, err)
		defer os.RemoveAll(destDir)

		err = codeManager.DownloadAndExtract(ctx, "nonexistent/path.zip", destDir)
		assert.Error(t, err)
	})
}

// TestConfig_LoadFromEnv tests config loading from environment variables
func TestConfig_LoadFromEnv(t *testing.T) {
	// Save original env vars
	originalEnv := map[string]string{
		"MINIO_ENDPOINT":          os.Getenv("MINIO_ENDPOINT"),
		"MINIO_ACCESS_KEY":        os.Getenv("MINIO_ACCESS_KEY"),
		"MINIO_SECRET_KEY":        os.Getenv("MINIO_SECRET_KEY"),
		"MINIO_USE_SSL":           os.Getenv("MINIO_USE_SSL"),
		"MINIO_CODE_BUCKET":       os.Getenv("MINIO_CODE_BUCKET"),
		"MINIO_STATE_BUCKET":      os.Getenv("MINIO_STATE_BUCKET"),
		"MAX_STATE_SIZE_MB":       os.Getenv("MAX_STATE_SIZE_MB"),
		"MAX_STATE_FILE_SIZE_MB":  os.Getenv("MAX_STATE_FILE_SIZE_MB"),
	}

	// Restore env vars after test
	defer func() {
		for k, v := range originalEnv {
			if v == "" {
				os.Unsetenv(k)
			} else {
				os.Setenv(k, v)
			}
		}
	}()

	t.Run("RequiredEnvVars", func(t *testing.T) {
		os.Unsetenv("MINIO_ENDPOINT")
		os.Unsetenv("MINIO_ACCESS_KEY")
		os.Unsetenv("MINIO_SECRET_KEY")

		_, err := LoadConfigFromEnv()
		assert.Error(t, err)
		assert.Contains(t, err.Error(), "MINIO_ENDPOINT is required")

		os.Setenv("MINIO_ENDPOINT", "localhost:9000")
		_, err = LoadConfigFromEnv()
		assert.Error(t, err)
		assert.Contains(t, err.Error(), "MINIO_ACCESS_KEY is required")

		os.Setenv("MINIO_ACCESS_KEY", "access")
		_, err = LoadConfigFromEnv()
		assert.Error(t, err)
		assert.Contains(t, err.Error(), "MINIO_SECRET_KEY is required")
	})

	t.Run("DefaultValues", func(t *testing.T) {
		os.Setenv("MINIO_ENDPOINT", "localhost:9000")
		os.Setenv("MINIO_ACCESS_KEY", "access")
		os.Setenv("MINIO_SECRET_KEY", "secret")
		os.Unsetenv("MINIO_USE_SSL")
		os.Unsetenv("MINIO_CODE_BUCKET")
		os.Unsetenv("MINIO_STATE_BUCKET")

		cfg, err := LoadConfigFromEnv()
		require.NoError(t, err)

		assert.Equal(t, "localhost:9000", cfg.Endpoint)
		assert.Equal(t, "access", cfg.AccessKey)
		assert.Equal(t, "secret", cfg.SecretKey)
		assert.False(t, cfg.UseSSL)
		assert.Equal(t, "custom-bots", cfg.CodeBucket)
		assert.Equal(t, "bot-state", cfg.StateBucket)
		assert.Equal(t, int64(8*1024*1024*1024), cfg.MaxStateSizeBytes)
		assert.Equal(t, int64(10*1024*1024), cfg.MaxStateFileSizeBytes)
	})

	t.Run("CustomValues", func(t *testing.T) {
		os.Setenv("MINIO_ENDPOINT", "minio.example.com:443")
		os.Setenv("MINIO_ACCESS_KEY", "mykey")
		os.Setenv("MINIO_SECRET_KEY", "mysecret")
		os.Setenv("MINIO_USE_SSL", "true")
		os.Setenv("MINIO_CODE_BUCKET", "my-code")
		os.Setenv("MINIO_STATE_BUCKET", "my-state")
		os.Setenv("MAX_STATE_SIZE_MB", "100")
		os.Setenv("MAX_STATE_FILE_SIZE_MB", "5")

		cfg, err := LoadConfigFromEnv()
		require.NoError(t, err)

		assert.Equal(t, "minio.example.com:443", cfg.Endpoint)
		assert.Equal(t, "mykey", cfg.AccessKey)
		assert.Equal(t, "mysecret", cfg.SecretKey)
		assert.True(t, cfg.UseSSL)
		assert.Equal(t, "my-code", cfg.CodeBucket)
		assert.Equal(t, "my-state", cfg.StateBucket)
		assert.Equal(t, int64(100*1024*1024), cfg.MaxStateSizeBytes)
		assert.Equal(t, int64(5*1024*1024), cfg.MaxStateFileSizeBytes)
	})
}

// TestDefaultConfig tests the default config values
func TestDefaultConfig(t *testing.T) {
	cfg := DefaultConfig()

	assert.Equal(t, "custom-bots", cfg.CodeBucket)
	assert.Equal(t, "bot-state", cfg.StateBucket)
	assert.Equal(t, "bot-logs", cfg.LogsBucket)
	assert.Equal(t, "backtest-results", cfg.ResultBucket)
	assert.Equal(t, int64(8*1024*1024*1024), cfg.MaxStateSizeBytes)
	assert.Equal(t, int64(10*1024*1024), cfg.MaxStateFileSizeBytes)
}
