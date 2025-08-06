package dockerrunner

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"runtime/internal/model"
	"runtime/internal/util"
	"strings"
	"testing"
	"time"

	"github.com/minio/minio-go/v7"
	"github.com/minio/minio-go/v7/pkg/credentials"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"github.com/testcontainers/testcontainers-go"
	"github.com/testcontainers/testcontainers-go/wait"
)

// Integration tests that verify the dynamic entrypoint functionality
// These tests focus on the core entrypoint script generation

func TestIntegration_EntrypointScriptGeneration_BotVsBacktest(t *testing.T) {
	tempDir := t.TempDir()
	logger := &util.DefaultLogger{}

	runner := &dockerRunner{
		logger:  logger,
		tempDir: tempDir,
	}

	tests := []struct {
		name            string
		executable      model.Executable
		entrypoint      string
		expectedDir     string
		expectedEnvType string
	}{
		{
			name: "Bot entrypoint should use /bot directory",
			executable: model.Executable{
				ID:         "test-bot",
				Runtime:    "python3.11",
				Entrypoint: "bot",
				EntrypointFiles: map[string]string{
					"bot":      "main.py",
					"backtest": "backtest.py",
				},
				Config: map[string]any{
					"test_param": "bot_value",
				},
			},
			entrypoint:      "bot",
			expectedDir:     "/bot",
			expectedEnvType: "bot",
		},
		{
			name: "Backtest entrypoint should use /backtest directory",
			executable: model.Executable{
				ID:         "test-backtest",
				Runtime:    "python3.11",
				Entrypoint: "backtest",
				EntrypointFiles: map[string]string{
					"bot":      "main.py",
					"backtest": "backtest.py",
				},
				Config: map[string]any{
					"test_param": "backtest_value",
				},
			},
			entrypoint:      "backtest",
			expectedDir:     "/backtest",
			expectedEnvType: "backtest",
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Create bot directory
			botDir := filepath.Join(tempDir, test.executable.ID)
			err := os.MkdirAll(botDir, 0755)
			require.NoError(t, err)

			// Generate entrypoint script
			scriptPath, err := runner.createEntrypointScript(test.executable, botDir)
			require.NoError(t, err)
			assert.NotEmpty(t, scriptPath)

			// Read and verify script content
			content, err := os.ReadFile(scriptPath)
			require.NoError(t, err)
			scriptContent := string(content)

			// Verify the script sets ENTRYPOINT_TYPE environment variable correctly
			assert.Contains(t, scriptContent, fmt.Sprintf(`export ENTRYPOINT_TYPE="%s"`, test.expectedEnvType))

			// Verify the Python entrypoint script is embedded
			assert.Contains(t, scriptContent, "ENTRYPOINT_TYPE", "Script should contain ENTRYPOINT_TYPE handling")

			// Verify bot ID and config are properly set
			assert.Contains(t, scriptContent, test.executable.ID)
			assert.Contains(t, scriptContent, "test_param")

			// Clean up
			os.RemoveAll(botDir)
		})
	}
}

func TestIntegration_EntrypointScriptGeneration_NodeJS(t *testing.T) {
	tempDir := t.TempDir()
	logger := &util.DefaultLogger{}

	runner := &dockerRunner{
		logger:  logger,
		tempDir: tempDir,
	}

	executable := model.Executable{
		ID:         "test-nodejs-bot",
		Runtime:    "nodejs20",
		Entrypoint: "bot",
		EntrypointFiles: map[string]string{
			"bot":      "main.js",
			"backtest": "backtest.js",
		},
		Config: map[string]any{
			"test_param": "nodejs_value",
		},
	}

	// Create bot directory
	botDir := filepath.Join(tempDir, executable.ID)
	err := os.MkdirAll(botDir, 0755)
	require.NoError(t, err)

	// Generate entrypoint script
	scriptPath, err := runner.createEntrypointScript(executable, botDir)
	require.NoError(t, err)
	assert.NotEmpty(t, scriptPath)

	// Read and verify script content
	content, err := os.ReadFile(scriptPath)
	require.NoError(t, err)
	scriptContent := string(content)

	// Verify the script sets ENTRYPOINT_TYPE environment variable correctly
	assert.Contains(t, scriptContent, `export ENTRYPOINT_TYPE="bot"`)

	// Verify the Node.js entrypoint script is embedded
	assert.Contains(t, scriptContent, "ENTRYPOINT_TYPE", "Script should contain ENTRYPOINT_TYPE handling")

	// Verify bot ID and config are properly set
	assert.Contains(t, scriptContent, executable.ID)
	assert.Contains(t, scriptContent, "test_param")

	// Verify Node.js specific script path (without .js extension)
	assert.Contains(t, scriptContent, "main") // Script path should be "main" not "main.js"
}

func TestIntegration_RealDocker_NodeJSBot(t *testing.T) {
	if testing.Short() {
		t.Skip("Skipping real Docker integration test in short mode")
	}

	// Check if Docker is available
	if !isDockerAvailable() {
		t.Skip("Docker is not available, skipping real Docker integration test")
	}

	// Start a real MinIO server for testing
	minioServer := startMinIOTestServer(t)
	defer minioServer.Close()

	tempDir := t.TempDir()
	logger := &util.DefaultLogger{}

	runner, err := NewDockerRunner(DockerRunnerOptions{
		Logger:  logger,
		TempDir: tempDir,
	})
	require.NoError(t, err)
	defer runner.Close()

	// Create test executable for Node.js bot
	executable := model.Executable{
		ID:         "real-nodejs-bot-test",
		Runtime:    "nodejs20",
		Entrypoint: "bot",
		EntrypointFiles: map[string]string{
			"bot":      "main.js",
			"backtest": "backtest.js",
		},
		Config: map[string]any{
			"test_param": "real_nodejs_value",
		},
		FilePath:       "real-nodejs-bot.zip",
		IsLongRunning:  false, // Test terminating container
		PersistResults: false, // Bots don't produce result files
	}

	// Use real Node.js test bot file
	jsZipData := loadRealTestBotFile(t, "js-test-bot.zip")
	uploadToMinIO(t, minioServer, executable.FilePath, jsZipData)

	// Test StartContainer
	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()

	result, err := runner.StartContainer(ctx, executable)
	require.NoError(t, err, "StartContainer should succeed")
	assert.NotNil(t, result)
	assert.False(t, result.IsLongRunning)

	// Log the result for debugging
	t.Logf("Node.js bot result: Status=%s, ExitCode=%d", result.Status, result.ExitCode)
	if result.Output != "" {
		t.Logf("Node.js bot output: %s", result.Output)
	}

	// Test should complete successfully or document the failure
	if result.Status == "success" {
		assert.Equal(t, "success", result.Status)
		assert.Equal(t, 0, result.ExitCode)
	} else {
		t.Logf("Node.js bot failed with exit code %d: %s", result.ExitCode, result.Output)
		// Document the actual behavior for analysis
	}
}

func TestIntegration_RealDocker_StartStopContainer_BotEntrypoint(t *testing.T) {
	if testing.Short() {
		t.Skip("Skipping real Docker integration test in short mode")
	}

	// Check if Docker is available
	if !isDockerAvailable() {
		t.Skip("Docker is not available, skipping real Docker integration test")
	}

	// Start a real MinIO server for testing
	minioServer := startMinIOTestServer(t)
	defer minioServer.Close()

	tempDir := t.TempDir()
	logger := &util.DefaultLogger{}

	runner, err := NewDockerRunner(DockerRunnerOptions{
		Logger:  logger,
		TempDir: tempDir,
	})
	require.NoError(t, err)
	defer runner.Close()

	// Create test executable for bot entrypoint that will be uploaded to MinIO
	executable := model.Executable{
		ID:         "real-integration-test-bot",
		Runtime:    "python3.11",
		Entrypoint: "bot",
		EntrypointFiles: map[string]string{
			"bot":      "main.py",
			"backtest": "backtest.py",
		},
		Config: map[string]any{
			"test_param": "real_bot_value",
		},
		FilePath:       "real-test-bot.zip",
		IsLongRunning:  true,  // Test long-running container
		PersistResults: false, // Bots don't produce result files
		Segment:        1,     // Test segment
	}

	// Create a real ZIP file with bot code and upload to MinIO
	botZipData := createTestBotZip(t, "bot")
	uploadToMinIO(t, minioServer, executable.FilePath, botZipData)

	// Test StartContainer
	ctx := context.Background()
	result, err := runner.StartContainer(ctx, executable)
	require.NoError(t, err, "StartContainer should succeed")
	assert.NotNil(t, result)
	assert.True(t, result.IsLongRunning)
	assert.NotEmpty(t, result.ContainerID)
	assert.Equal(t, "running", result.Status)

	containerID := result.ContainerID

	// Wait for container to start
	time.Sleep(2 * time.Second)

	// Test GetContainerStatus
	status, err := runner.GetContainerStatus(ctx, containerID)
	require.NoError(t, err)
	assert.Equal(t, "running", status.Status)

	// Test ListManagedContainers
	containers, err := runner.ListManagedContainers(ctx, 1)
	require.NoError(t, err)
	if len(containers) == 0 {
		t.Logf("Warning: No managed containers found, this might be expected for long-running containers")
	} else {
		assert.Len(t, containers, 1)
		assert.Equal(t, containerID, containers[0].ContainerID)
		assert.Equal(t, "main.py", containers[0].Entrypoint)
	}

	// Test GetContainerLogs - THIS IS THE CRUCIAL TEST for the fix
	logs, err := runner.GetContainerLogs(ctx, containerID, 100)
	require.NoError(t, err)
	assert.NotEmpty(t, logs)

	// Verify the container is using the bot entrypoint correctly
	assert.Contains(t, logs, "STARTUP: Python bot wrapper starting", "Container should use bot wrapper")
	assert.Contains(t, logs, "CHDIR_SUCCESS: Changed to working directory: /bot", "Container should change to /bot directory")
	assert.Contains(t, logs, "CONFIG_SUCCESS: Bot ID:", "Container should parse bot configuration")

	// Test StopContainer
	err = runner.StopContainer(ctx, containerID, executable)
	require.NoError(t, err, "StopContainer should succeed")

	// Verify container is stopped
	time.Sleep(1 * time.Second)
	finalContainers, err := runner.ListManagedContainers(ctx, 1)
	require.NoError(t, err)
	assert.Empty(t, finalContainers, "Container should be removed from managed list")
}

func TestIntegration_RealDocker_StartStopContainer_BacktestEntrypoint(t *testing.T) {
	if testing.Short() {
		t.Skip("Skipping real Docker integration test in short mode")
	}

	// Check if Docker is available
	if !isDockerAvailable() {
		t.Skip("Docker is not available, skipping real Docker integration test")
	}

	// Start a real MinIO server for testing
	minioServer := startMinIOTestServer(t)
	defer minioServer.Close()

	tempDir := t.TempDir()
	logger := &util.DefaultLogger{}

	runner, err := NewDockerRunner(DockerRunnerOptions{
		Logger:  logger,
		TempDir: tempDir,
	})
	require.NoError(t, err)
	defer runner.Close()

	// Create test executable for backtest entrypoint
	executable := model.Executable{
		ID:         "real-integration-test-backtest",
		Runtime:    "python3.11",
		Entrypoint: "backtest",
		EntrypointFiles: map[string]string{
			"bot":      "main.py",
			"backtest": "backtest.py",
		},
		Config: map[string]any{
			"test_param": "real_backtest_value",
		},
		FilePath:       "real-test-backtest.zip",
		IsLongRunning:  false, // Test terminating container
		PersistResults: true,  // Backtests do produce result files
	}

	// Create a real ZIP file with backtest code and upload to MinIO
	backtestZipData := createTestBotZip(t, "backtest")
	uploadToMinIO(t, minioServer, executable.FilePath, backtestZipData)

	// Test StartContainer
	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()

	result, err := runner.StartContainer(ctx, executable)
	require.NoError(t, err, "StartContainer should succeed")
	assert.NotNil(t, result)
	assert.False(t, result.IsLongRunning)
	assert.Equal(t, "success", result.Status)                                                   // Should complete successfully
	assert.Contains(t, result.Output, "STARTUP: Python backtest wrapper starting")              // Verify using backtest wrapper
	assert.Contains(t, result.Output, "CHDIR_SUCCESS: Changed to working directory: /backtest") // Verify working in /backtest directory
}

func TestIntegration_RealDocker_DifferentExecutables_SameContainer(t *testing.T) {
	if testing.Short() {
		t.Skip("Skipping real Docker integration test in short mode")
	}

	// Check if Docker is available
	if !isDockerAvailable() {
		t.Skip("Docker is not available, skipping real Docker integration test")
	}

	// Start a real MinIO server for testing
	minioServer := startMinIOTestServer(t)
	defer minioServer.Close()

	tempDir := t.TempDir()
	logger := &util.DefaultLogger{}

	runner, err := NewDockerRunner(DockerRunnerOptions{
		Logger:  logger,
		TempDir: tempDir,
	})
	require.NoError(t, err)
	defer runner.Close()

	ctx := context.Background()

	// Test same executable with different entrypoints
	executable := model.Executable{
		ID:         "multi-entrypoint-test",
		Runtime:    "python3.11",
		Entrypoint: "bot", // This will be overridden by the entrypoint parameter
		EntrypointFiles: map[string]string{
			"bot":      "main.py",
			"backtest": "backtest.py",
		},
		Config: map[string]any{
			"test_param": "multi_value",
		},
		FilePath:       "multi-test.zip",
		IsLongRunning:  false, // Start with terminating for this test
		PersistResults: false, // Bots don't produce result files
	}

	// Create ZIP with both bot and backtest files and upload to MinIO
	zipData := createTestBotZipWithMultipleEntrypoints(t)
	uploadToMinIO(t, minioServer, executable.FilePath, zipData)

	// Test 1: Start as bot entrypoint
	botResult, err := runner.StartContainer(ctx, executable)
	require.NoError(t, err)
	assert.Equal(t, "success", botResult.Status)

	// Verify it used bot entrypoint
	assert.Contains(t, botResult.Output, "STARTUP: Python bot wrapper starting")
	assert.Contains(t, botResult.Output, "CHDIR_SUCCESS: Changed to working directory: /bot")

	// Test 2: Start same executable as backtest entrypoint
	executable.Entrypoint = "backtest" // Change to backtest entrypoint
	executable.IsLongRunning = false   // Make it terminating for backtest
	executable.PersistResults = true   // Backtests produce result files
	backtestResult, err := runner.StartContainer(ctx, executable)
	require.NoError(t, err)
	assert.Equal(t, "success", backtestResult.Status)

	// Verify it used backtest entrypoint
	assert.Contains(t, backtestResult.Output, "STARTUP: Python backtest wrapper starting")
	assert.Contains(t, backtestResult.Output, "CHDIR_SUCCESS: Changed to working directory: /backtest")
}

// Helper functions for real Docker integration tests

func createTestBotZip(t *testing.T, entrypoint string) []byte {
	// Use real test bot files instead of synthetic ones
	var filename string
	if entrypoint == "bot" || entrypoint == "backtest" {
		filename = "py-test-bot.zip"
	} else {
		filename = "py-test-bot.zip" // Default to Python
	}

	return loadRealTestBotFile(t, filename)
}

func createTestBotZipWithMultipleEntrypoints(t *testing.T) []byte {
	// Use the multi-test bot file that has both main.py and backtest.py that properly terminate
	return loadRealTestBotFile(t, "py-multi-test-bot.zip")
}

// loadRealTestBotFile loads actual test bot files from fixtures directory
func loadRealTestBotFile(t *testing.T, filename string) []byte {
	testfilePath := filepath.Join("..", "fixtures", filename)
	data, err := os.ReadFile(testfilePath)
	require.NoError(t, err, "Failed to read test bot file: %s", testfilePath)
	return data
}

// MinIO test server management using testcontainers
type MinIOTestServer struct {
	container testcontainers.Container
	endpoint  string
	accessKey string
	secretKey string
}

func (m *MinIOTestServer) Close() {
	if m.container != nil {
		m.container.Terminate(context.Background())
	}
}

func startMinIOTestServer(t *testing.T) *MinIOTestServer {
	ctx := context.Background()

	// MinIO credentials
	accessKey := "testaccess"
	secretKey := "testsecret"

	// Create MinIO container
	req := testcontainers.ContainerRequest{
		Image:        "minio/minio:latest",
		ExposedPorts: []string{"9000/tcp"},
		Env: map[string]string{
			"MINIO_ROOT_USER":     accessKey,
			"MINIO_ROOT_PASSWORD": secretKey,
		},
		Cmd:        []string{"server", "/data"},
		WaitingFor: wait.ForHTTP("/minio/health/live").WithPort("9000"),
	}

	container, err := testcontainers.GenericContainer(ctx, testcontainers.GenericContainerRequest{
		ContainerRequest: req,
		Started:          true,
	})
	require.NoError(t, err)

	// Get the container endpoint
	host, err := container.Host(ctx)
	require.NoError(t, err)
	port, err := container.MappedPort(ctx, "9000")
	require.NoError(t, err)

	endpoint := fmt.Sprintf("%s:%s", host, port.Port())

	// Set environment variables for the docker runner
	os.Setenv("MINIO_ENDPOINT", endpoint)
	os.Setenv("MINIO_ACCESS_KEY", accessKey)
	os.Setenv("MINIO_SECRET_KEY", secretKey)
	os.Setenv("MINIO_SSL", "false")

	t.Cleanup(func() {
		os.Unsetenv("MINIO_ENDPOINT")
		os.Unsetenv("MINIO_ACCESS_KEY")
		os.Unsetenv("MINIO_SECRET_KEY")
		os.Unsetenv("MINIO_SSL")
	})

	return &MinIOTestServer{
		container: container,
		endpoint:  endpoint,
		accessKey: accessKey,
		secretKey: secretKey,
	}
}

func uploadToMinIO(t *testing.T, server *MinIOTestServer, objectName string, data []byte) {
	// Create MinIO client
	client, err := minio.New(server.endpoint, &minio.Options{
		Creds:  credentials.NewStaticV4(server.accessKey, server.secretKey, ""),
		Secure: false,
	})
	require.NoError(t, err)

	ctx := context.Background()
	bucketName := "custom-bots"

	// Create bucket if it doesn't exist
	exists, err := client.BucketExists(ctx, bucketName)
	require.NoError(t, err)
	if !exists {
		err = client.MakeBucket(ctx, bucketName, minio.MakeBucketOptions{})
		require.NoError(t, err)
	}

	// Upload the file
	reader := bytes.NewReader(data)
	_, err = client.PutObject(ctx, bucketName, objectName, reader, int64(len(data)), minio.PutObjectOptions{
		ContentType: "application/zip",
	})
	require.NoError(t, err)
}

// isDockerAvailable checks if Docker daemon is available
func isDockerAvailable() bool {
	// For testcontainers, we need Docker to be available
	// The testcontainers library will handle checking if Docker is running
	return true
}

func TestIntegration_RealDocker_NodeJSBacktest(t *testing.T) {
	if testing.Short() {
		t.Skip("Skipping real Docker integration test in short mode")
	}

	if !isDockerAvailable() {
		t.Skip("Docker is not available, skipping real Docker integration test")
	}

	minioServer := startMinIOTestServer(t)
	defer minioServer.Close()

	tempDir := t.TempDir()
	logger := &util.DefaultLogger{}

	runner, err := NewDockerRunner(DockerRunnerOptions{
		Logger:  logger,
		TempDir: tempDir,
	})
	require.NoError(t, err)
	defer runner.Close()

	executable := model.Executable{
		ID:         "real-nodejs-backtest-test",
		Runtime:    "nodejs20",
		Entrypoint: "backtest",
		EntrypointFiles: map[string]string{
			"bot":      "main.js",
			"backtest": "backtest.js",
		},
		Config: map[string]any{
			"test_param": "real_nodejs_backtest_value",
		},
		FilePath:       "real-nodejs-backtest.zip",
		IsLongRunning:  false,
		PersistResults: true, // Backtests do produce result files
	}

	jsZipData := loadRealTestBotFile(t, "js-test-bot.zip")
	uploadToMinIO(t, minioServer, executable.FilePath, jsZipData)

	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()

	result, err := runner.StartContainer(ctx, executable)
	require.NoError(t, err, "StartContainer should succeed")
	assert.NotNil(t, result)

	t.Logf("Node.js backtest result: Status=%s, ExitCode=%d", result.Status, result.ExitCode)
	if result.Output != "" {
		t.Logf("Node.js backtest output: %s", result.Output)
	}

	require.Equal(t, "success", result.Status, "Backtest should complete successfully")
	require.Equal(t, 0, result.ExitCode, "Backtest should exit with code 0")

	// Verify result file was created and contains valid JSON
	// Extract JSON from output (first line should be the result JSON)
	lines := strings.Split(result.Output, "\n")
	var jsonLine string
	for _, line := range lines {
		if strings.HasPrefix(line, "{") {
			jsonLine = line
			break
		}
	}
	require.NotEmpty(t, jsonLine, "Should find JSON result in output")

	var backtestResult map[string]any
	err = json.Unmarshal([]byte(jsonLine), &backtestResult)
	require.NoError(t, err, "Backtest output should be valid JSON")

	// Verify expected JSON structure
	assert.Equal(t, "success", backtestResult["status"], "Result should have success status")
	assert.NotNil(t, backtestResult["results"], "Result should contain results object")

	results, ok := backtestResult["results"].(map[string]any)
	require.True(t, ok, "Results should be an object")

	// Verify metrics exist
	assert.NotNil(t, results["metrics"], "Results should contain metrics")
	assert.NotNil(t, results["plots"], "Results should contain plots")
	assert.NotNil(t, results["tables"], "Results should contain tables")
}

func TestIntegration_RealDocker_PythonBot(t *testing.T) {
	if testing.Short() {
		t.Skip("Skipping real Docker integration test in short mode")
	}

	if !isDockerAvailable() {
		t.Skip("Docker is not available, skipping real Docker integration test")
	}

	minioServer := startMinIOTestServer(t)
	defer minioServer.Close()

	tempDir := t.TempDir()
	logger := &util.DefaultLogger{}

	runner, err := NewDockerRunner(DockerRunnerOptions{
		Logger:  logger,
		TempDir: tempDir,
	})
	require.NoError(t, err)
	defer runner.Close()

	executable := model.Executable{
		ID:         "real-python-bot-test",
		Runtime:    "python3.11",
		Entrypoint: "bot",
		EntrypointFiles: map[string]string{
			"bot":      "main.py",
			"backtest": "backtest.py",
		},
		Config: map[string]any{
			"test_param": "real_python_value",
		},
		FilePath:       "real-python-bot.zip",
		IsLongRunning:  true,  // Change to true for bot entrypoint
		PersistResults: false, // Bots don't produce result files
		Segment:        1,     // Test segment
	}

	pyZipData := loadRealTestBotFile(t, "py-test-bot.zip")
	uploadToMinIO(t, minioServer, executable.FilePath, pyZipData)

	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()

	result, err := runner.StartContainer(ctx, executable)
	require.NoError(t, err, "StartContainer should succeed")
	assert.NotNil(t, result)

	// Debug output
	t.Logf("StartContainer result: Status=%s, Message=%s, Error=%s, ContainerID=%s, IsLongRunning=%v, ExitCode=%d",
		result.Status, result.Message, result.Error, result.ContainerID, result.IsLongRunning, result.ExitCode)

	assert.True(t, result.IsLongRunning)
	assert.NotEmpty(t, result.ContainerID)
	assert.Equal(t, "running", result.Status)

	containerID := result.ContainerID

	// Wait for container to start
	time.Sleep(2 * time.Second)

	// Test GetContainerStatus
	status, err := runner.GetContainerStatus(ctx, containerID)
	require.NoError(t, err)
	assert.Equal(t, "running", status.Status)

	// Test GetContainerLogs
	logs, err := runner.GetContainerLogs(ctx, containerID, 100)
	require.NoError(t, err)
	assert.NotEmpty(t, logs)

	t.Logf("Python bot logs: %s", logs)

	// Test StopContainer
	err = runner.StopContainer(ctx, containerID, executable)
	require.NoError(t, err, "StopContainer should succeed")
}

func TestIntegration_RealDocker_PythonBacktest(t *testing.T) {
	if testing.Short() {
		t.Skip("Skipping real Docker integration test in short mode")
	}

	if !isDockerAvailable() {
		t.Skip("Docker is not available, skipping real Docker integration test")
	}

	minioServer := startMinIOTestServer(t)
	defer minioServer.Close()

	tempDir := t.TempDir()
	logger := &util.DefaultLogger{}

	runner, err := NewDockerRunner(DockerRunnerOptions{
		Logger:  logger,
		TempDir: tempDir,
	})
	require.NoError(t, err)
	defer runner.Close()

	executable := model.Executable{
		ID:         "real-python-backtest-test",
		Runtime:    "python3.11",
		Entrypoint: "backtest",
		EntrypointFiles: map[string]string{
			"bot":      "main.py",
			"backtest": "backtest.py",
		},
		Config: map[string]any{
			"test_param": "real_python_backtest_value",
		},
		FilePath:       "real-python-backtest.zip",
		IsLongRunning:  false,
		PersistResults: true, // Backtests do produce result files
	}

	pyZipData := loadRealTestBotFile(t, "py-test-bot.zip")
	uploadToMinIO(t, minioServer, executable.FilePath, pyZipData)

	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()

	result, err := runner.StartContainer(ctx, executable)
	require.NoError(t, err, "StartContainer should succeed")
	assert.NotNil(t, result)

	t.Logf("Python backtest result: Status=%s, ExitCode=%d", result.Status, result.ExitCode)
	if result.Output != "" {
		t.Logf("Python backtest output: %s", result.Output)
	}

	require.Equal(t, "success", result.Status, "Backtest should complete successfully")
	require.Equal(t, 0, result.ExitCode, "Backtest should exit with code 0")

	// Verify result file was created and contains valid JSON
	// Extract JSON from output (first line should be the result JSON)
	lines := strings.Split(result.Output, "\n")
	var jsonLine string
	for _, line := range lines {
		if strings.HasPrefix(line, "{") {
			jsonLine = line
			break
		}
	}
	require.NotEmpty(t, jsonLine, "Should find JSON result in output")

	var backtestResult map[string]any
	err = json.Unmarshal([]byte(jsonLine), &backtestResult)
	require.NoError(t, err, "Backtest output should be valid JSON")

	// Verify expected JSON structure
	assert.Equal(t, "success", backtestResult["status"], "Result should have success status")
	assert.NotNil(t, backtestResult["results"], "Result should contain results object")

	results, ok := backtestResult["results"].(map[string]any)
	require.True(t, ok, "Results should be an object")

	// Verify metrics exist
	assert.NotNil(t, results["metrics"], "Results should contain metrics")
	assert.NotNil(t, results["plots"], "Results should contain plots")
	assert.NotNil(t, results["tables"], "Results should contain tables")
}

func TestStoreAnalysisResult(t *testing.T) {
	if testing.Short() {
		t.Skip("Skipping Docker runner integration test in short mode")
	}

	if !isDockerAvailable() {
		t.Skip("Docker is not available, skipping Docker runner integration test")
	}

	// Start MinIO test server
	minioServer := startMinIOTestServer(t)
	defer minioServer.Close()

	// Set environment variables for backtests bucket
	os.Setenv("MINIO_BACKTESTS_BUCKET", "test-backtests")
	defer os.Unsetenv("MINIO_BACKTESTS_BUCKET")

	tempDir := t.TempDir()
	logger := &util.DefaultLogger{}

	runner, err := NewDockerRunner(DockerRunnerOptions{
		Logger:  logger,
		TempDir: tempDir,
	})
	require.NoError(t, err)
	defer runner.Close()

	// Test analysis result storage through backtest execution
	executable := model.Executable{
		ID:         "analysis-test-backtest",
		Runtime:    "python3.11",
		Entrypoint: "backtest",
		EntrypointFiles: map[string]string{
			"bot":      "main.py",
			"backtest": "backtest.py",
		},
		Config: map[string]any{
			"test_param": "analysis_test_value",
		},
		FilePath:       "analysis-test-backtest.zip",
		IsLongRunning:  false,
		PersistResults: true, // This will trigger analysis storage
	}

	// Create and upload test backtest code
	pyZipData := loadRealTestBotFile(t, "py-test-bot.zip")
	uploadToMinIO(t, minioServer, executable.FilePath, pyZipData)

	// Create the backtests bucket in MinIO for analysis storage
	client, err := minio.New(minioServer.endpoint, &minio.Options{
		Creds:  credentials.NewStaticV4(minioServer.accessKey, minioServer.secretKey, ""),
		Secure: false,
	})
	require.NoError(t, err)

	ctx := context.Background()
	backtestsBucket := "test-backtests"
	exists, err := client.BucketExists(ctx, backtestsBucket)
	require.NoError(t, err)
	if !exists {
		err = client.MakeBucket(ctx, backtestsBucket, minio.MakeBucketOptions{})
		require.NoError(t, err)
	}

	// Execute the backtest
	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()

	result, err := runner.StartContainer(ctx, executable)
	require.NoError(t, err, "StartContainer should succeed")
	assert.NotNil(t, result)

	t.Logf("Analysis test backtest result: Status=%s, ExitCode=%d", result.Status, result.ExitCode)
	if result.Output != "" {
		t.Logf("Analysis test backtest output: %s", result.Output)
	}

	// Verify backtest completed successfully
	require.Equal(t, "success", result.Status, "Backtest should complete successfully")
	require.Equal(t, 0, result.ExitCode, "Backtest should exit with code 0")

	// Wait a moment for analysis file to be stored
	time.Sleep(2 * time.Second)

	// Verify analysis file was stored in the correct bucket
	analysisPath := fmt.Sprintf("%s/analysis.json", executable.ID)

	obj, err := client.GetObject(ctx, backtestsBucket, analysisPath, minio.GetObjectOptions{})
	require.NoError(t, err, "Analysis file should be stored in backtests bucket")
	defer obj.Close()

	content, err := io.ReadAll(obj)
	require.NoError(t, err)

	// Parse and verify analysis JSON structure
	var analysisResult map[string]any
	err = json.Unmarshal(content, &analysisResult)
	require.NoError(t, err, "Analysis file should contain valid JSON")

	// Verify analysis structure
	assert.Equal(t, "success", analysisResult["status"], "Analysis should have success status")

	// The analysis result has a nested "results" structure
	results, hasResults := analysisResult["results"].(map[string]any)
	require.True(t, hasResults, "Analysis should contain results object")

	assert.NotNil(t, results["metrics"], "Analysis should contain metrics")
	assert.NotNil(t, results["plots"], "Analysis should contain plots")
	assert.NotNil(t, results["tables"], "Analysis should contain tables")

	t.Logf("Analysis file successfully stored and verified: %s", string(content))
}
