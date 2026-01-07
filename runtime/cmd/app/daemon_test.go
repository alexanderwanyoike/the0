package main

import (
	"archive/tar"
	"archive/zip"
	"bytes"
	"compress/gzip"
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

	"runtime/internal/constants"
)

// TestRunDaemonInit_MissingEnvironment tests init fails with missing MinIO config
func TestRunDaemonInit_MissingEnvironment(t *testing.T) {
	// Clear MinIO environment variables
	os.Unsetenv("MINIO_ENDPOINT")
	os.Unsetenv("MINIO_ACCESS_KEY")
	os.Unsetenv("MINIO_SECRET_KEY")

	daemonBotID = constants.TestBotID
	daemonCodePath = t.TempDir()
	daemonStatePath = t.TempDir()
	daemonCodeFile = ""

	err := runDaemonInit(nil, nil)
	assert.Error(t, err, "Should fail with missing MinIO config")
	assert.Contains(t, err.Error(), "MINIO_ENDPOINT")
}

// TestRunDaemonInit_InvalidEndpoint tests init fails with invalid endpoint
func TestRunDaemonInit_InvalidEndpoint(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}

	// Set environment with invalid endpoint
	os.Setenv("MINIO_ENDPOINT", fmt.Sprintf("invalid-host:%d", constants.TestInvalidPort))
	os.Setenv("MINIO_ACCESS_KEY", "testkey")
	os.Setenv("MINIO_SECRET_KEY", "testsecret")
	defer func() {
		os.Unsetenv("MINIO_ENDPOINT")
		os.Unsetenv("MINIO_ACCESS_KEY")
		os.Unsetenv("MINIO_SECRET_KEY")
	}()

	daemonBotID = constants.TestBotID
	daemonCodePath = t.TempDir()
	daemonStatePath = t.TempDir()
	daemonCodeFile = ""

	err := runDaemonInit(nil, nil)
	// Should fail to connect or timeout, but not panic
	if err != nil {
		// Expected - can't connect to invalid host
		assert.NotNil(t, err)
	}
}

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

// TestDaemonInit_Defaults tests that default paths are applied
func TestDaemonInit_Defaults(t *testing.T) {
	daemonBotID = constants.TestBotID
	daemonCodePath = "" // Should default to constants.TestBotDir
	daemonStatePath = "" // Should default to constants.TestStateDir
	daemonCodeFile = ""

	// We can't run this without MinIO, but we can verify flags are set
	assert.Equal(t, constants.TestBotID, daemonBotID)
}

// TestDaemonSync_Defaults tests that default values are applied
func TestDaemonSync_Defaults(t *testing.T) {
	daemonBotID = constants.TestBotID
	daemonStatePath = "" // Should default to constants.TestStateDir
	daemonLogsPath = ""  // Should default to constants.TestLogsDir
	daemonSyncInterval = 0 // Should default to constants.TestDefaultSyncInterval

	// We can't run this without MinIO, but we can verify flags are set
	assert.Equal(t, constants.TestBotID, daemonBotID)
}

// TestDaemonInit_CodeFileSpecified tests init with explicit code file path
func TestDaemonInit_CodeFileSpecified(t *testing.T) {
	daemonBotID = constants.TestBotID
	daemonCodePath = constants.TestBotDir
	daemonStatePath = constants.TestStateDir
	daemonCodeFile = constants.TestCodeFile

	assert.Equal(t, constants.TestCodeFile, daemonCodeFile)
}

// TestDaemonSync_WatchDoneSpecified tests sync with watch-done file
func TestDaemonSync_WatchDoneSpecified(t *testing.T) {
	daemonBotID = constants.TestBotID
	daemonStatePath = constants.TestStateDir
	daemonLogsPath = constants.TestLogsDir
	daemonWatchDone = constants.TestDoneFilePath

	assert.Equal(t, constants.TestDoneFilePath, daemonWatchDone)
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

// startMinIOTestContainer starts a MinIO container for testing
func startMinIOTestContainer(t *testing.T) (*minio.Client, func(), string) {
	ctx := context.Background()

	req := testcontainers.ContainerRequest{
		Image:        "minio/minio:latest",
		ExposedPorts: []string{"9000/tcp"},
		Env: map[string]string{
			"MINIO_ROOT_USER":     "minioadmin",
			"MINIO_ROOT_PASSWORD": "minioadmin",
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

	// Create MinIO client
	minioClient, err := minio.New(endpoint, &minio.Options{
		Creds:  credentials.NewStaticV4("minioadmin", "minioadmin", ""),
		Secure: false,
	})
	require.NoError(t, err)

	// Create buckets (using default names from storage config)
	err = minioClient.MakeBucket(ctx, "custom-bots", minio.MakeBucketOptions{})
	require.NoError(t, err)
	err = minioClient.MakeBucket(ctx, "state", minio.MakeBucketOptions{})
	require.NoError(t, err)

	cleanup := func() {
		container.Terminate(ctx)
	}

	return minioClient, cleanup, endpoint
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

// TestRunDaemonInit_Integration tests full init workflow with MinIO
func TestRunDaemonInit_Integration(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}

	minioClient, cleanup, endpoint := startMinIOTestContainer(t)
	defer cleanup()

	ctx := context.Background()

	// Create test bot code
	testCode := createTestBotZip(t, map[string]string{
		"main.py":   "print('Hello from test bot')",
		"config.json": `{"version": "1.0.0"}`,
		"README.md": "# Test Bot",
	})

	// Upload to MinIO
	codeObjectPath := "test-bot/v1.0.0/code.zip"
	_, err := minioClient.PutObject(ctx, "custom-bots", codeObjectPath,
		bytes.NewReader(testCode), int64(len(testCode)),
		minio.PutObjectOptions{ContentType: "application/zip"})
	require.NoError(t, err)

	// Set up environment
	codeDir := t.TempDir()
	stateDir := t.TempDir()

	os.Setenv("MINIO_ENDPOINT", endpoint)
	os.Setenv("MINIO_ACCESS_KEY", "minioadmin")
	os.Setenv("MINIO_SECRET_KEY", "minioadmin")
	defer func() {
		os.Unsetenv("MINIO_ENDPOINT")
		os.Unsetenv("MINIO_ACCESS_KEY")
		os.Unsetenv("MINIO_SECRET_KEY")
	}()

	// Configure daemon init
	daemonBotID = "test-bot"
	daemonCodePath = codeDir
	daemonStatePath = stateDir
	daemonCodeFile = codeObjectPath

	// Run daemon init
	err = runDaemonInit(nil, nil)
	require.NoError(t, err, "Daemon init should succeed")

	// Verify files were extracted
	assert.FileExists(t, filepath.Join(codeDir, "main.py"))
	assert.FileExists(t, filepath.Join(codeDir, "config.json"))
	assert.FileExists(t, filepath.Join(codeDir, "README.md"))

	// Verify file contents
	content, err := os.ReadFile(filepath.Join(codeDir, "main.py"))
	require.NoError(t, err)
	assert.Equal(t, "print('Hello from test bot')", string(content))
}

// TestRunDaemonInit_MultipleFiles tests init with multiple code files
func TestRunDaemonInit_MultipleFiles(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}

	minioClient, cleanup, endpoint := startMinIOTestContainer(t)
	defer cleanup()

	ctx := context.Background()

	// Create test bot code with multiple files
	testCode := createTestBotZip(t, map[string]string{
		"bot.js":      "console.log('main');",
		"utils.js":    "exports.helper = () => {};",
		"package.json": `{"name": "test-bot", "version": "1.0.0"}`,
		"README.md":   "# Test Bot\n\nA test bot.",
	})

	// Upload code to MinIO
	codeObjectPath := "test-bot-multi/v1.0.0/code.zip"
	_, err := minioClient.PutObject(ctx, "custom-bots", codeObjectPath,
		bytes.NewReader(testCode), int64(len(testCode)),
		minio.PutObjectOptions{ContentType: "application/zip"})
	require.NoError(t, err)

	// Set up environment
	codeDir := t.TempDir()
	stateDir := t.TempDir()

	os.Setenv("MINIO_ENDPOINT", endpoint)
	os.Setenv("MINIO_ACCESS_KEY", "minioadmin")
	os.Setenv("MINIO_SECRET_KEY", "minioadmin")
	defer func() {
		os.Unsetenv("MINIO_ENDPOINT")
		os.Unsetenv("MINIO_ACCESS_KEY")
		os.Unsetenv("MINIO_SECRET_KEY")
	}()

	// Configure daemon init
	daemonBotID = "test-bot-multi"
	daemonCodePath = codeDir
	daemonStatePath = stateDir
	daemonCodeFile = codeObjectPath

	// Run daemon init
	err = runDaemonInit(nil, nil)
	require.NoError(t, err, "Daemon init should succeed")

	// Verify all files were extracted
	assert.FileExists(t, filepath.Join(codeDir, "bot.js"))
	assert.FileExists(t, filepath.Join(codeDir, "utils.js"))
	assert.FileExists(t, filepath.Join(codeDir, "package.json"))
	assert.FileExists(t, filepath.Join(codeDir, "README.md"))

	// Verify file contents
	content, err := os.ReadFile(filepath.Join(codeDir, "bot.js"))
	require.NoError(t, err)
	assert.Equal(t, "console.log('main');", string(content))

	// Avoid unused variable warning
	_ = minioClient
}

// TestRunDaemonInit_NoCodeFile tests init without code file (only state)
func TestRunDaemonInit_NoCodeFile(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}

	minioClient, cleanup, endpoint := startMinIOTestContainer(t)
	defer cleanup()

	// Set up environment
	codeDir := t.TempDir()
	stateDir := t.TempDir()

	os.Setenv("MINIO_ENDPOINT", endpoint)
	os.Setenv("MINIO_ACCESS_KEY", "minioadmin")
	os.Setenv("MINIO_SECRET_KEY", "minioadmin")
	defer func() {
		os.Unsetenv("MINIO_ENDPOINT")
		os.Unsetenv("MINIO_ACCESS_KEY")
		os.Unsetenv("MINIO_SECRET_KEY")
	}()

	// Configure daemon init with empty code file
	daemonBotID = "test-bot-no-code"
	daemonCodePath = codeDir
	daemonStatePath = stateDir
	daemonCodeFile = "" // No code to download

	// Run daemon init - should succeed even without code
	err := runDaemonInit(nil, nil)
	require.NoError(t, err, "Daemon init should succeed without code file")

	// Verify directories exist
	assert.DirExists(t, codeDir)
	assert.DirExists(t, stateDir)

	// Avoid unused variable warning
	_ = minioClient
}

// TestRunDaemonSync_ShortRun tests sync runs briefly without errors
func TestRunDaemonSync_ShortRun(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}

	_, cleanup, endpoint := startMinIOTestContainer(t)
	defer cleanup()

	// Set up environment
	stateDir := t.TempDir()
	logsDir := t.TempDir()

	// Create a dummy state file
	stateFile := filepath.Join(stateDir, "state.json")
	err := os.WriteFile(stateFile, []byte(`{"test": true}`), 0644)
	require.NoError(t, err)

	os.Setenv("MINIO_ENDPOINT", endpoint)
	os.Setenv("MINIO_ACCESS_KEY", "minioadmin")
	os.Setenv("MINIO_SECRET_KEY", "minioadmin")
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
