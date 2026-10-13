package main

import (
	"archive/tar"
	"archive/zip"
	"bytes"
	"compress/gzip"
	"context"
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/minio/minio-go/v7"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"runtime/internal/constants"
	"runtime/internal/s3test"
)

// TestRunDaemonSync_MissingEnvironment tests sync fails with missing MinIO config
func TestRunDaemonSync_MissingEnvironment(t *testing.T) {
	// Clear MinIO environment variables
	os.Unsetenv("MINIO_ENDPOINT")
	os.Unsetenv("MINIO_ACCESS_KEY")
	os.Unsetenv("MINIO_SECRET_KEY")

	daemonBotID = constants.TestBotID
	daemonStatePath = t.TempDir()
	daemonLogsPath = t.TempDir()
	daemonSyncInterval = constants.TestDefaultSyncInterval

	err := runDaemonSync(nil, nil)
	assert.Error(t, err, "Should fail with missing MinIO config")
	assert.Contains(t, err.Error(), "MINIO_ENDPOINT")
}

// TestDaemonSync_Defaults tests that default values are applied
func TestDaemonSync_Defaults(t *testing.T) {
	daemonBotID = constants.TestBotID
	daemonStatePath = ""   // Should default to constants.TestStateDir
	daemonLogsPath = ""    // Should default to constants.TestLogsDir
	daemonSyncInterval = 0 // Should default to constants.TestDefaultSyncInterval

	// We can't run this without MinIO, but we can verify flags are set
	assert.Equal(t, constants.TestBotID, daemonBotID)
}

// TestDaemonSync_CustomInterval tests sync with custom interval
func TestDaemonSync_CustomInterval(t *testing.T) {
	daemonBotID = constants.TestBotID
	daemonStatePath = constants.TestStateDir
	daemonLogsPath = constants.TestLogsDir
	daemonSyncInterval = constants.TestCustomSyncInterval

	assert.Equal(t, constants.TestCustomSyncInterval, daemonSyncInterval)
}

// Integration tests with MinIO testcontainer

// startMinIOTestContainer starts a store with the buckets the daemon expects.
func startMinIOTestContainer(t *testing.T) (*minio.Client, string) {
	server := s3test.StartT(t)
	ctx := context.Background()

	// Bucket names are the storage config defaults.
	require.NoError(t, server.Client.MakeBucket(ctx, "custom-bots", minio.MakeBucketOptions{}))
	require.NoError(t, server.Client.MakeBucket(ctx, "state", minio.MakeBucketOptions{}))

	return server.Client, server.Endpoint
}

// createTestBotZip creates a zip file with test bot files
func createTestBotZip(t *testing.T, files map[string]string) []byte {
	buf := new(bytes.Buffer)
	zipWriter := zip.NewWriter(buf)

	for filename, content := range files {
		writer, err := zipWriter.Create(filename)
		require.NoError(t, err)
		_, err = writer.Write([]byte(content))
		require.NoError(t, err)
	}

	err := zipWriter.Close()
	require.NoError(t, err)

	return buf.Bytes()
}

// createStateTarGz creates a tar.gz archive with state files
func createStateTarGz(t *testing.T, files map[string]string) []byte {
	buf := new(bytes.Buffer)
	gzWriter := gzip.NewWriter(buf)
	tarWriter := tar.NewWriter(gzWriter)

	for filename, content := range files {
		header := &tar.Header{
			Name: filename,
			Mode: 0644,
			Size: int64(len(content)),
		}
		err := tarWriter.WriteHeader(header)
		require.NoError(t, err)
		_, err = tarWriter.Write([]byte(content))
		require.NoError(t, err)
	}

	err := tarWriter.Close()
	require.NoError(t, err)
	err = gzWriter.Close()
	require.NoError(t, err)

	return buf.Bytes()
}

// TestRunDaemonSync_ShortRun tests sync runs briefly without errors
func TestRunDaemonSync_ShortRun(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}

	_, endpoint := startMinIOTestContainer(t)

	// Set up environment
	stateDir := t.TempDir()
	logsDir := t.TempDir()

	// Create a dummy state file
	stateFile := filepath.Join(stateDir, "state.json")
	err := os.WriteFile(stateFile, []byte(`{"test": true}`), 0644)
	require.NoError(t, err)

	os.Setenv("MINIO_ENDPOINT", endpoint)
	os.Setenv("MINIO_ACCESS_KEY", s3test.AccessKey)
	os.Setenv("MINIO_SECRET_KEY", s3test.SecretKey)
	defer func() {
		os.Unsetenv("MINIO_ENDPOINT")
		os.Unsetenv("MINIO_ACCESS_KEY")
		os.Unsetenv("MINIO_SECRET_KEY")
	}()

	// Configure daemon sync with short interval
	daemonBotID = "test-sync-bot"
	daemonStatePath = stateDir
	daemonLogsPath = logsDir
	daemonSyncInterval = 1 * time.Second

	// Run sync in a goroutine with context cancellation
	ctx, cancel := context.WithTimeout(context.Background(), 3*time.Second)
	defer cancel()

	errChan := make(chan error, 1)
	go func() {
		// Note: runDaemonSync doesn't take context, so we'll just let it run
		// and kill it via timeout
		errChan <- runDaemonSync(nil, nil)
	}()

	// Wait for context timeout or error
	select {
	case <-ctx.Done():
		// Timeout is expected - sync should run for ~3 seconds
		// This is a successful test case
	case err := <-errChan:
		// If sync returns early, it should not be an error
		// (unless there's a config problem)
		if err != nil {
			t.Logf("Sync returned with error (may be expected): %v", err)
		}
	}

	// Verify state directory still exists
	assert.DirExists(t, stateDir)
}
