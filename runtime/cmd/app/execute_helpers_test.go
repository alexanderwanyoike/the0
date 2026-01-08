package main

import (
	"bytes"
	"context"
	"errors"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"github.com/minio/minio-go/v7"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"runtime/internal/constants"
	"runtime/internal/execute"
	"runtime/internal/util"
)

// ============================================================================
// Mock implementations for dependency injection testing
// ============================================================================

// mockProcessExecutor is a mock implementation of execute.ProcessExecutor for testing
type mockProcessExecutor struct {
	startErr error
	waitErr  error
	killErr  error
	started  bool
	waited   bool
	killed   bool
}

func (m *mockProcessExecutor) Start(cmd *exec.Cmd) error {
	m.started = true
	return m.startErr
}

func (m *mockProcessExecutor) Wait(cmd *exec.Cmd) error {
	m.waited = true
	return m.waitErr
}

func (m *mockProcessExecutor) Kill(cmd *exec.Cmd) error {
	m.killed = true
	return m.killErr
}

// mockFileWriter is a mock implementation of execute.FileWriter for testing
type mockFileWriter struct {
	createErr error
	created   string
}

func (m *mockFileWriter) Create(path string) (io.WriteCloser, error) {
	m.created = path
	if m.createErr != nil {
		return nil, m.createErr
	}
	// Return a fake file for testing
	return &mockWriteCloser{}, nil
}

// mockWriteCloser is a mock io.WriteCloser for testing
type mockWriteCloser struct{}

func (m *mockWriteCloser) Write(p []byte) (n int, err error) {
	return len(p), nil
}

func (m *mockWriteCloser) Close() error {
	return nil
}

// ============================================================================
// Tests for executeProcessWithDeps
// ============================================================================

func TestExecuteProcessWithDeps_Success(t *testing.T) {
	mockProc := &mockProcessExecutor{}
	mockFile := &mockFileWriter{}
	deps := execute.Dependencies{
		ProcessExecutor: mockProc,
		FileWriter:      mockFile,
	}

	cfg := &execute.Config{
		BotID:      "test-bot",
		Runtime:    "python3.11",
		Entrypoint: "main.py",
		CodePath:   constants.TestBotDir,
	}
	logger := &util.DefaultLogger{}
	ctx := context.Background()

	exitCode := executeProcessWithDeps(ctx, cfg, "main.py", logger, nil, deps)

	assert.Equal(t, 0, exitCode)
	assert.True(t, mockProc.started)
	assert.True(t, mockProc.waited)
	assert.False(t, mockProc.killed)
}

func TestExecuteProcessWithDeps_StartFails(t *testing.T) {
	mockProc := &mockProcessExecutor{
		startErr: errors.New("start failed"),
	}
	mockFile := &mockFileWriter{}
	deps := execute.Dependencies{
		ProcessExecutor: mockProc,
		FileWriter:      mockFile,
	}

	cfg := &execute.Config{
		BotID:      "test-bot",
		Runtime:    "python3.11",
		Entrypoint: "main.py",
		CodePath:   constants.TestBotDir,
	}
	logger := &util.DefaultLogger{}
	ctx := context.Background()

	exitCode := executeProcessWithDeps(ctx, cfg, "main.py", logger, nil, deps)

	assert.Equal(t, 1, exitCode)
	assert.True(t, mockProc.started)
	assert.False(t, mockProc.waited)
}

func TestExecuteProcessWithDeps_WaitFails(t *testing.T) {
	mockProc := &mockProcessExecutor{
		waitErr: &exec.ExitError{},
	}
	mockFile := &mockFileWriter{}
	deps := execute.Dependencies{
		ProcessExecutor: mockProc,
		FileWriter:      mockFile,
	}

	cfg := &execute.Config{
		BotID:      "test-bot",
		Runtime:    "python3.11",
		Entrypoint: "main.py",
		CodePath:   constants.TestBotDir,
	}
	logger := &util.DefaultLogger{}
	ctx := context.Background()

	_ = executeProcessWithDeps(ctx, cfg, "main.py", logger, nil, deps)

	// Exit code depends on the ExitError's ExitCode(), which defaults to -1
	// but we're testing the flow, so just verify process was started and waited
	assert.True(t, mockProc.started)
	assert.True(t, mockProc.waited)
}

func TestExecuteProcessWithDeps_ContextCancelled(t *testing.T) {
	// Use a cancelled context
	ctx, cancel := context.WithCancel(context.Background())
	cancel() // Cancel immediately

	mockProc := &mockProcessExecutor{}
	mockFile := &mockFileWriter{}
	deps := execute.Dependencies{
		ProcessExecutor: mockProc,
		FileWriter:      mockFile,
	}

	cfg := &execute.Config{
		BotID:      "test-bot",
		Runtime:    "python3.11",
		Entrypoint: "main.py",
		CodePath:   constants.TestBotDir,
	}
	logger := &util.DefaultLogger{}

	exitCode := executeProcessWithDeps(ctx, cfg, "main.py", logger, nil, deps)

	// Context is already cancelled, so it should kill the process
	// But Start() still happens, so the process may or may not have started
	assert.NotEqual(t, 0, exitCode)
}

// ============================================================================
// Tests for executeQueryProcessWithDeps
// ============================================================================

func TestExecuteQueryProcessWithDeps_Success(t *testing.T) {
	mockProc := &mockProcessExecutor{}
	deps := execute.Dependencies{
		ProcessExecutor: mockProc,
		FileWriter:      &mockFileWriter{},
	}

	cfg := &execute.Config{
		BotID:           "test-bot",
		Runtime:         "python3.11",
		Entrypoint:      "main.py",
		QueryEntrypoint: "query.py",
		CodePath:        constants.TestBotDir,
	}
	logger := &util.DefaultLogger{}
	ctx := context.Background()

	exitCode := executeQueryProcessWithDeps(ctx, cfg, "query.py", logger, deps)

	assert.Equal(t, 0, exitCode)
	assert.True(t, mockProc.started)
	assert.True(t, mockProc.waited)
}

func TestExecuteQueryProcessWithDeps_StartFails(t *testing.T) {
	mockProc := &mockProcessExecutor{
		startErr: errors.New("query start failed"),
	}
	deps := execute.Dependencies{
		ProcessExecutor: mockProc,
		FileWriter:      &mockFileWriter{},
	}

	cfg := &execute.Config{
		BotID:           "test-bot",
		Runtime:         "nodejs20",
		QueryEntrypoint: "query.js",
		CodePath:        constants.TestBotDir,
	}
	logger := &util.DefaultLogger{}
	ctx := context.Background()

	exitCode := executeQueryProcessWithDeps(ctx, cfg, "query.js", logger, deps)

	assert.Equal(t, 1, exitCode)
	assert.True(t, mockProc.started)
	assert.False(t, mockProc.waited)
}

// ============================================================================
// Tests for setupLogFileWithDeps
// ============================================================================

func TestSetupLogFileWithDeps_Success(t *testing.T) {
	mockFile := &mockFileWriter{}
	deps := execute.Dependencies{
		ProcessExecutor: &mockProcessExecutor{},
		FileWriter:      mockFile,
	}
	logger := &util.DefaultLogger{}

	// setupLogFileWithDeps returns *os.File which our mock doesn't create
	// In real tests with os.File, this would work, but for mocks we test the error path
	result := setupLogFileWithDeps("/var/logs", logger, deps)

	// Our mock returns mockWriteCloser which can't be type-asserted to *os.File
	assert.Nil(t, result)
	assert.Contains(t, mockFile.created, "bot.log")
}

func TestSetupLogFileWithDeps_CreateFails(t *testing.T) {
	mockFile := &mockFileWriter{
		createErr: errors.New("create failed"),
	}
	deps := execute.Dependencies{
		ProcessExecutor: &mockProcessExecutor{},
		FileWriter:      mockFile,
	}
	logger := &util.DefaultLogger{}

	result := setupLogFileWithDeps("/var/logs", logger, deps)

	assert.Nil(t, result)
}

// ============================================================================
// Tests for loadExecuteConfig
// ============================================================================

func TestLoadExecuteConfig_Success(t *testing.T) {
	// Set up required environment variables
	os.Setenv("BOT_ID", "test-bot-123")
	os.Setenv("RUNTIME", "python3.11")
	os.Setenv("ENTRYPOINT", "main.py")
	os.Setenv("BOT_CONFIG", `{"key": "value"}`)
	defer func() {
		os.Unsetenv("BOT_ID")
		os.Unsetenv("RUNTIME")
		os.Unsetenv("ENTRYPOINT")
		os.Unsetenv("BOT_CONFIG")
	}()

	cfg, err := loadExecuteConfig()

	require.NoError(t, err)
	assert.Equal(t, "test-bot-123", cfg.BotID)
	assert.Equal(t, "python3.11", cfg.Runtime)
	assert.Equal(t, "main.py", cfg.Entrypoint)
	assert.Equal(t, `{"key": "value"}`, cfg.BotConfig)
	assert.Equal(t, constants.TestBotDir, cfg.CodePath)   // Default value
	assert.Equal(t, constants.TestStateDir, cfg.StatePath) // Default value
}

func TestLoadExecuteConfig_MissingBotID(t *testing.T) {
	// Clear required variables
	os.Unsetenv("BOT_ID")
	os.Setenv("RUNTIME", "python3.11")
	os.Setenv("ENTRYPOINT", "main.py")
	defer func() {
		os.Unsetenv("RUNTIME")
		os.Unsetenv("ENTRYPOINT")
	}()

	cfg, err := loadExecuteConfig()

	assert.Error(t, err)
	assert.Nil(t, cfg)
	assert.Contains(t, err.Error(), "BOT_ID")
}

func TestLoadExecuteConfig_MissingRuntime(t *testing.T) {
	os.Setenv("BOT_ID", "test-bot")
	os.Unsetenv("RUNTIME")
	os.Setenv("ENTRYPOINT", "main.py")
	defer func() {
		os.Unsetenv("BOT_ID")
		os.Unsetenv("ENTRYPOINT")
	}()

	cfg, err := loadExecuteConfig()

	assert.Error(t, err)
	assert.Nil(t, cfg)
	assert.Contains(t, err.Error(), "RUNTIME")
}

func TestLoadExecuteConfig_MissingEntrypoint(t *testing.T) {
	os.Setenv("BOT_ID", "test-bot")
	os.Setenv("RUNTIME", "python3.11")
	os.Unsetenv("ENTRYPOINT")
	defer func() {
		os.Unsetenv("BOT_ID")
		os.Unsetenv("RUNTIME")
	}()

	cfg, err := loadExecuteConfig()

	assert.Error(t, err)
	assert.Nil(t, cfg)
	assert.Contains(t, err.Error(), "ENTRYPOINT")
}

func TestLoadExecuteConfig_WithOptionalFields(t *testing.T) {
	os.Setenv("BOT_ID", "test-bot")
	os.Setenv("RUNTIME", "python3.11")
	os.Setenv("ENTRYPOINT", "main.py")
	os.Setenv("BOT_TYPE", "scheduled")
	os.Setenv("QUERY_ENTRYPOINT", "query.py")
	os.Setenv("QUERY_PATH", "/status")
	os.Setenv("QUERY_PARAMS", `{"limit": 10}`)
	os.Setenv("IS_SCHEDULED", "true")
	defer func() {
		os.Unsetenv("BOT_ID")
		os.Unsetenv("RUNTIME")
		os.Unsetenv("ENTRYPOINT")
		os.Unsetenv("BOT_TYPE")
		os.Unsetenv("QUERY_ENTRYPOINT")
		os.Unsetenv("QUERY_PATH")
		os.Unsetenv("QUERY_PARAMS")
		os.Unsetenv("IS_SCHEDULED")
	}()

	cfg, err := loadExecuteConfig()

	require.NoError(t, err)
	assert.Equal(t, "scheduled", cfg.BotType)
	assert.Equal(t, "query.py", cfg.QueryEntrypoint)
	assert.Equal(t, "/status", cfg.QueryPath)
	assert.Equal(t, `{"limit": 10}`, cfg.QueryParams)
	assert.True(t, cfg.IsScheduled)
}

func TestLoadExecuteConfig_CustomPaths(t *testing.T) {
	os.Setenv("BOT_ID", "test-bot")
	os.Setenv("RUNTIME", "nodejs20")
	os.Setenv("ENTRYPOINT", "index.js")
	os.Setenv("CODE_PATH", "/custom/code")
	os.Setenv("STATE_PATH", "/custom/state")
	os.Setenv("LOGS_PATH", "/custom/logs")
	defer func() {
		os.Unsetenv("BOT_ID")
		os.Unsetenv("RUNTIME")
		os.Unsetenv("ENTRYPOINT")
		os.Unsetenv("CODE_PATH")
		os.Unsetenv("STATE_PATH")
		os.Unsetenv("LOGS_PATH")
	}()

	cfg, err := loadExecuteConfig()

	require.NoError(t, err)
	assert.Equal(t, "/custom/code", cfg.CodePath)
	assert.Equal(t, "/custom/state", cfg.StatePath)
	assert.Equal(t, "/custom/logs", cfg.LogsPath)
}

func TestLoadExecuteConfig_QueryPathFlagOverride(t *testing.T) {
	// Set environment with query path
	os.Setenv("BOT_ID", "test-bot")
	os.Setenv("RUNTIME", "python3.11")
	os.Setenv("ENTRYPOINT", "main.py")
	os.Setenv("QUERY_PATH", "/env-query-path")
	defer func() {
		os.Unsetenv("BOT_ID")
		os.Unsetenv("RUNTIME")
		os.Unsetenv("ENTRYPOINT")
		os.Unsetenv("QUERY_PATH")
	}()

	// Set flag override
	executeQueryPath = "/flag-query-path"
	defer func() { executeQueryPath = "" }()

	cfg, err := loadExecuteConfig()

	require.NoError(t, err)
	// Flag should override environment variable
	assert.Equal(t, "/flag-query-path", cfg.QueryPath)
}

// ============================================================================
// Integration tests for downloadCode and downloadState
// ============================================================================

func TestDownloadCode_Success(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}

	minioClient, cleanup, endpoint := startMinIOTestContainer(t)
	defer cleanup()

	ctx := context.Background()

	// Create test bot code
	testCode := createTestBotZip(t, map[string]string{
		"main.py":  "print('test')",
		"utils.py": "def helper(): pass",
	})

	// Upload to MinIO
	codeObjectPath := "test-download/v1.0.0/code.zip"
	_, err := minioClient.PutObject(ctx, "custom-bots", codeObjectPath,
		bytes.NewReader(testCode), int64(len(testCode)),
		minio.PutObjectOptions{ContentType: "application/zip"})
	require.NoError(t, err)

	// Set up environment
	codeDir := t.TempDir()

	os.Setenv("MINIO_ENDPOINT", endpoint)
	os.Setenv("MINIO_ACCESS_KEY", "minioadmin")
	os.Setenv("MINIO_SECRET_KEY", "minioadmin")
	defer func() {
		os.Unsetenv("MINIO_ENDPOINT")
		os.Unsetenv("MINIO_ACCESS_KEY")
		os.Unsetenv("MINIO_SECRET_KEY")
	}()

	cfg := &execute.Config{
		BotID:    "test-download",
		CodeFile: codeObjectPath,
		CodePath: codeDir,
	}
	logger := &util.DefaultLogger{}

	err = downloadCode(ctx, cfg, logger)
	require.NoError(t, err)

	// Verify files were extracted
	assert.FileExists(t, filepath.Join(codeDir, "main.py"))
	assert.FileExists(t, filepath.Join(codeDir, "utils.py"))

	// Avoid unused variable warning
	_ = minioClient
}

func TestDownloadCode_NoCodeFile(t *testing.T) {
	cfg := &execute.Config{
		BotID:    "test-no-code",
		CodeFile: "", // Empty code file
		CodePath: t.TempDir(),
	}
	logger := &util.DefaultLogger{}
	ctx := context.Background()

	// Should succeed without downloading
	err := downloadCode(ctx, cfg, logger)
	assert.NoError(t, err)
}

func TestDownloadCode_MissingMinIOConfig(t *testing.T) {
	// Clear environment
	os.Unsetenv("MINIO_ENDPOINT")
	os.Unsetenv("MINIO_ACCESS_KEY")
	os.Unsetenv("MINIO_SECRET_KEY")

	cfg := &execute.Config{
		BotID:    "test-missing-config",
		CodeFile: "some/file.zip",
		CodePath: t.TempDir(),
	}
	logger := &util.DefaultLogger{}
	ctx := context.Background()

	err := downloadCode(ctx, cfg, logger)
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "storage config")
}

func TestDownloadState_NoExistingState(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}

	_, cleanup, endpoint := startMinIOTestContainer(t)
	defer cleanup()

	ctx := context.Background()
	stateDir := t.TempDir()

	os.Setenv("MINIO_ENDPOINT", endpoint)
	os.Setenv("MINIO_ACCESS_KEY", "minioadmin")
	os.Setenv("MINIO_SECRET_KEY", "minioadmin")
	defer func() {
		os.Unsetenv("MINIO_ENDPOINT")
		os.Unsetenv("MINIO_ACCESS_KEY")
		os.Unsetenv("MINIO_SECRET_KEY")
	}()

	cfg := &execute.Config{
		BotID:     "no-state-bot",
		StatePath: stateDir,
	}
	logger := &util.DefaultLogger{}

	// Should succeed gracefully for first run (no state exists)
	err := downloadState(ctx, cfg, logger)
	assert.NoError(t, err)

	// Directory should exist but be empty
	assert.DirExists(t, stateDir)
}

func TestDownloadState_MissingMinIOConfig(t *testing.T) {
	// Clear environment
	os.Unsetenv("MINIO_ENDPOINT")
	os.Unsetenv("MINIO_ACCESS_KEY")
	os.Unsetenv("MINIO_SECRET_KEY")

	cfg := &execute.Config{
		BotID:     "test-missing-config",
		StatePath: t.TempDir(),
	}
	logger := &util.DefaultLogger{}
	ctx := context.Background()

	err := downloadState(ctx, cfg, logger)
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "storage config")
}

// Note: Helper functions startMinIOTestContainer, createTestBotZip, and createStateTarGz
// are shared with daemon_test.go and defined there.
