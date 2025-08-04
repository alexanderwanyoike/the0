package util

import (
	"bytes"
	"io"
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestDefaultLogger(t *testing.T) {
	logger := &DefaultLogger{}

	t.Run("Info logging", func(t *testing.T) {
		// Capture stdout
		old := os.Stdout
		r, w, _ := os.Pipe()
		os.Stdout = w

		logger.Info("test message", "key", "value")

		w.Close()
		os.Stdout = old
		var buf bytes.Buffer
		io.Copy(&buf, r)
		output := buf.String()

		assert.Contains(t, output, "[INFO]:")
		assert.Contains(t, output, "test message")
		assert.Contains(t, output, "key")
		assert.Contains(t, output, "value")
	})

	t.Run("Error logging", func(t *testing.T) {
		// Capture stdout
		old := os.Stdout
		r, w, _ := os.Pipe()
		os.Stdout = w

		logger.Error("error message", "error", "something went wrong")

		w.Close()
		os.Stdout = old
		var buf bytes.Buffer
		io.Copy(&buf, r)
		output := buf.String()

		assert.Contains(t, output, "[ERROR]:")
		assert.Contains(t, output, "error message")
		assert.Contains(t, output, "error")
		assert.Contains(t, output, "something went wrong")
	})

	t.Run("Debug logging", func(t *testing.T) {
		// Capture stdout
		old := os.Stdout
		r, w, _ := os.Pipe()
		os.Stdout = w

		logger.Debug("debug message", "debug", "info")

		w.Close()
		os.Stdout = old
		var buf bytes.Buffer
		io.Copy(&buf, r)
		output := buf.String()

		assert.Contains(t, output, "[DEBUG]:")
		assert.Contains(t, output, "debug message")
		assert.Contains(t, output, "debug")
		assert.Contains(t, output, "info")
	})

	t.Run("Logging without fields", func(t *testing.T) {
		// Capture stdout
		old := os.Stdout
		r, w, _ := os.Pipe()
		os.Stdout = w

		logger.Info("simple message")

		w.Close()
		os.Stdout = old
		var buf bytes.Buffer
		io.Copy(&buf, r)
		output := buf.String()

		assert.Contains(t, output, "[INFO]:")
		assert.Contains(t, output, "simple message")
	})
}

func TestLogMaster(t *testing.T) {
	t.Run("LogMaster with args", func(t *testing.T) {
		// Capture stdout
		old := os.Stdout
		r, w, _ := os.Pipe()
		os.Stdout = w

		LogMaster("Starting server on port %d", 8080)

		w.Close()
		os.Stdout = old
		var buf bytes.Buffer
		io.Copy(&buf, r)
		output := buf.String()

		assert.Contains(t, output, "MASTER:")
		assert.Contains(t, output, "Starting server on port 8080")
	})

	t.Run("LogMaster without args", func(t *testing.T) {
		// Capture stdout
		old := os.Stdout
		r, w, _ := os.Pipe()
		os.Stdout = w

		LogMaster("Server started")

		w.Close()
		os.Stdout = old
		var buf bytes.Buffer
		io.Copy(&buf, r)
		output := buf.String()

		assert.Contains(t, output, "MASTER:")
		assert.Contains(t, output, "Server started")
	})
}

func TestLogWorker(t *testing.T) {
	t.Run("LogWorker with args", func(t *testing.T) {
		// Capture stdout
		old := os.Stdout
		r, w, _ := os.Pipe()
		os.Stdout = w

		LogWorker("Processing job %s", "job-123")

		w.Close()
		os.Stdout = old
		var buf bytes.Buffer
		io.Copy(&buf, r)
		output := buf.String()

		assert.Contains(t, output, "WORKER:")
		assert.Contains(t, output, "Processing job job-123")
	})

	t.Run("LogWorker without args", func(t *testing.T) {
		// Capture stdout
		old := os.Stdout
		r, w, _ := os.Pipe()
		os.Stdout = w

		LogWorker("Job completed")

		w.Close()
		os.Stdout = old
		var buf bytes.Buffer
		io.Copy(&buf, r)
		output := buf.String()

		assert.Contains(t, output, "WORKER:")
		assert.Contains(t, output, "Job completed")
	})
}

func TestTestLogger(t *testing.T) {
	// Use the actual test instance for TestLogger testing
	logger := &TestLogger{T: t}

	t.Run("TestLogger Info with fields", func(t *testing.T) {
		// TestLogger will log to the test output, just verify it doesn't panic
		logger.Info("test message", "key", "value")
	})

	t.Run("TestLogger Info without fields", func(t *testing.T) {
		logger.Info("simple message")
	})

	t.Run("TestLogger Error with fields", func(t *testing.T) {
		logger.Error("error occurred", "error", "test error")
	})

	t.Run("TestLogger Error without fields", func(t *testing.T) {
		logger.Error("simple error")
	})

	t.Run("TestLogger Debug with fields", func(t *testing.T) {
		logger.Debug("debug info", "debug", "value")
	})

	t.Run("TestLogger Debug without fields", func(t *testing.T) {
		logger.Debug("simple debug")
	})
}

func TestLoggerInterface(t *testing.T) {
	t.Run("DefaultLogger implements Logger interface", func(t *testing.T) {
		var logger Logger = &DefaultLogger{}
		assert.NotNil(t, logger)
		
		// Test that all interface methods work
		logger.Info("test")
		logger.Error("test") 		
		logger.Debug("test")
	})

	t.Run("TestLogger implements Logger interface", func(t *testing.T) {
		var logger Logger = &TestLogger{T: t}
		assert.NotNil(t, logger)
		
		// Test that all interface methods work
		logger.Info("test")
		logger.Error("test")
		logger.Debug("test")
	})
}