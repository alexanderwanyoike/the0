package execute

import (
	"context"
	"errors"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"runtime/internal/util"
)

// MockProcessExecutor for testing
type MockProcessExecutor struct {
	StartError   error
	WaitError    error
	KillError    error
	StartCalled  int
	WaitCalled   int
	KillCalled   int
	WaitDuration time.Duration
}

func (m *MockProcessExecutor) Start(cmd *exec.Cmd) error {
	m.StartCalled++
	return m.StartError
}

func (m *MockProcessExecutor) Wait(cmd *exec.Cmd) error {
	m.WaitCalled++
	if m.WaitDuration > 0 {
		time.Sleep(m.WaitDuration)
	}
	return m.WaitError
}

func (m *MockProcessExecutor) Kill(cmd *exec.Cmd) error {
	m.KillCalled++
	return m.KillError
}

// MockFileWriter for testing
type MockFileWriter struct {
	CreateError  error
	CreatedFiles []string
	MockFile     io.WriteCloser
}

func (m *MockFileWriter) Create(path string) (io.WriteCloser, error) {
	m.CreatedFiles = append(m.CreatedFiles, path)
	if m.CreateError != nil {
		return nil, m.CreateError
	}
	if m.MockFile != nil {
		return m.MockFile, nil
	}
	// Return a real temp file for testing
	return os.CreateTemp("", "test-log-*")
}

// --- Tests ---

func TestNewDefaultDependencies(t *testing.T) {
	deps := NewDefaultDependencies()

	assert.NotNil(t, deps.ProcessExecutor)
	assert.NotNil(t, deps.FileWriter)

	// Verify they are the correct types
	_, isDefaultExecutor := deps.ProcessExecutor.(*DefaultProcessExecutor)
	assert.True(t, isDefaultExecutor)

	_, isDefaultWriter := deps.FileWriter.(*DefaultFileWriter)
	assert.True(t, isDefaultWriter)
}

func TestDefaultProcessExecutor(t *testing.T) {
	executor := &DefaultProcessExecutor{}

	t.Run("Kill with nil process", func(t *testing.T) {
		cmd := exec.Command("echo", "test")
		// Don't start the command, so Process is nil
		err := executor.Kill(cmd)
		assert.NoError(t, err)
	})

	t.Run("Start echo command", func(t *testing.T) {
		cmd := exec.Command("echo", "hello")
		err := executor.Start(cmd)
		assert.NoError(t, err)
		// Wait for it to complete
		executor.Wait(cmd)
	})
}

func TestDefaultFileWriter(t *testing.T) {
	writer := &DefaultFileWriter{}

	t.Run("Create file in temp directory", func(t *testing.T) {
		tmpDir := t.TempDir()
		path := filepath.Join(tmpDir, "test.log")

		file, err := writer.Create(path)
		require.NoError(t, err)
		assert.NotNil(t, file)
		file.Close()

		// Verify file exists
		_, err = os.Stat(path)
		assert.NoError(t, err)
	})

	t.Run("Create file with nested directory", func(t *testing.T) {
		tmpDir := t.TempDir()
		path := filepath.Join(tmpDir, "nested", "dir", "test.log")

		file, err := writer.Create(path)
		require.NoError(t, err)
		assert.NotNil(t, file)
		file.Close()

		// Verify file exists
		_, err = os.Stat(path)
		assert.NoError(t, err)
	})
}

func TestNewProcessRunner(t *testing.T) {
	t.Run("with logger", func(t *testing.T) {
		logger := &util.DefaultLogger{}
		deps := NewDefaultDependencies()
		runner := NewProcessRunner(logger, deps)

		assert.NotNil(t, runner)
		assert.Equal(t, logger, runner.logger)
	})

	t.Run("nil logger uses default", func(t *testing.T) {
		deps := NewDefaultDependencies()
		runner := NewProcessRunner(nil, deps)

		assert.NotNil(t, runner)
		assert.NotNil(t, runner.logger)
	})
}

func TestProcessRunner_RunProcess(t *testing.T) {
	t.Run("successful execution", func(t *testing.T) {
		mockExecutor := &MockProcessExecutor{}
		deps := Dependencies{
			ProcessExecutor: mockExecutor,
			FileWriter:      &MockFileWriter{},
		}
		runner := NewProcessRunner(&util.DefaultLogger{}, deps)

		cmd := exec.Command("echo", "test")
		exitCode := runner.RunProcess(context.Background(), cmd)

		assert.Equal(t, 0, exitCode)
		assert.Equal(t, 1, mockExecutor.StartCalled)
		assert.Equal(t, 1, mockExecutor.WaitCalled)
	})

	t.Run("start error", func(t *testing.T) {
		mockExecutor := &MockProcessExecutor{
			StartError: errors.New("start failed"),
		}
		deps := Dependencies{
			ProcessExecutor: mockExecutor,
			FileWriter:      &MockFileWriter{},
		}
		runner := NewProcessRunner(&util.DefaultLogger{}, deps)

		cmd := exec.Command("nonexistent")
		exitCode := runner.RunProcess(context.Background(), cmd)

		assert.Equal(t, 1, exitCode)
		assert.Equal(t, 1, mockExecutor.StartCalled)
		assert.Equal(t, 0, mockExecutor.WaitCalled)
	})

	t.Run("context cancellation", func(t *testing.T) {
		mockExecutor := &MockProcessExecutor{
			WaitDuration: 5 * time.Second, // Long wait
		}
		deps := Dependencies{
			ProcessExecutor: mockExecutor,
			FileWriter:      &MockFileWriter{},
		}
		runner := NewProcessRunner(&util.DefaultLogger{}, deps)

		ctx, cancel := context.WithTimeout(context.Background(), 100*time.Millisecond)
		defer cancel()

		cmd := exec.Command("sleep", "10")
		exitCode := runner.RunProcess(ctx, cmd)

		assert.Equal(t, 1, exitCode)
		assert.Equal(t, 1, mockExecutor.KillCalled)
	})

	t.Run("process error", func(t *testing.T) {
		mockExecutor := &MockProcessExecutor{
			WaitError: errors.New("process failed"),
		}
		deps := Dependencies{
			ProcessExecutor: mockExecutor,
			FileWriter:      &MockFileWriter{},
		}
		runner := NewProcessRunner(&util.DefaultLogger{}, deps)

		cmd := exec.Command("echo", "test")
		exitCode := runner.RunProcess(context.Background(), cmd)

		assert.Equal(t, 1, exitCode)
	})
}

func TestProcessRunner_SetupLogFile(t *testing.T) {
	t.Run("successful creation", func(t *testing.T) {
		tmpDir := t.TempDir()
		deps := NewDefaultDependencies()
		runner := NewProcessRunner(&util.DefaultLogger{}, deps)

		logFile := runner.SetupLogFile(tmpDir)
		require.NotNil(t, logFile)
		defer logFile.Close()

		// Verify file exists
		expectedPath := filepath.Join(tmpDir, "bot.log")
		_, err := os.Stat(expectedPath)
		assert.NoError(t, err)
	})

	t.Run("creation error", func(t *testing.T) {
		mockWriter := &MockFileWriter{
			CreateError: errors.New("permission denied"),
		}
		deps := Dependencies{
			ProcessExecutor: &MockProcessExecutor{},
			FileWriter:      mockWriter,
		}
		runner := NewProcessRunner(&util.DefaultLogger{}, deps)

		logFile := runner.SetupLogFile("/some/path")
		assert.Nil(t, logFile)
	})

	t.Run("non-os.File returned", func(t *testing.T) {
		// MockFileWriter that returns a non-*os.File
		mockWriter := &MockFileWriter{
			MockFile: &mockWriteCloser{},
		}
		deps := Dependencies{
			ProcessExecutor: &MockProcessExecutor{},
			FileWriter:      mockWriter,
		}
		runner := NewProcessRunner(&util.DefaultLogger{}, deps)

		logFile := runner.SetupLogFile("/some/path")
		assert.Nil(t, logFile, "should return nil for non-*os.File")
	})
}

func TestProcessRunner_RunWithOutput(t *testing.T) {
	t.Run("with log file", func(t *testing.T) {
		tmpDir := t.TempDir()
		logPath := filepath.Join(tmpDir, "test.log")
		logFile, err := os.Create(logPath)
		require.NoError(t, err)
		defer logFile.Close()

		mockExecutor := &MockProcessExecutor{}
		deps := Dependencies{
			ProcessExecutor: mockExecutor,
			FileWriter:      &MockFileWriter{},
		}
		runner := NewProcessRunner(&util.DefaultLogger{}, deps)

		cmd := exec.Command("echo", "test")
		exitCode := runner.RunWithOutput(context.Background(), cmd, logFile)

		assert.Equal(t, 0, exitCode)
		assert.NotNil(t, cmd.Stdout, "stdout should be set")
		assert.NotNil(t, cmd.Stderr, "stderr should be set")
	})

	t.Run("without log file", func(t *testing.T) {
		mockExecutor := &MockProcessExecutor{}
		deps := Dependencies{
			ProcessExecutor: mockExecutor,
			FileWriter:      &MockFileWriter{},
		}
		runner := NewProcessRunner(&util.DefaultLogger{}, deps)

		cmd := exec.Command("echo", "test")
		exitCode := runner.RunWithOutput(context.Background(), cmd, nil)

		assert.Equal(t, 0, exitCode)
		assert.Equal(t, os.Stdout, cmd.Stdout)
		assert.Equal(t, os.Stderr, cmd.Stderr)
	})
}

// mockWriteCloser is a mock io.WriteCloser that is not *os.File
type mockWriteCloser struct{}

func (m *mockWriteCloser) Write(p []byte) (n int, err error) {
	return len(p), nil
}

func (m *mockWriteCloser) Close() error {
	return nil
}
