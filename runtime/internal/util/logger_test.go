package util

import (
	"bytes"
	"io"
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestNewLogger(t *testing.T) {
	logger := NewLogger()
	assert.NotNil(t, logger)
	assert.NotNil(t, logger.logger)
}

func TestGetLogLevel(t *testing.T) {
	tests := []struct {
		name     string
		envValue string
		expected string
	}{
		{"default is INFO", "", "INFO"},
		{"DEBUG level", "DEBUG", "DEBUG"},
		{"WARN level", "WARN", "WARN"},
		{"ERROR level", "ERROR", "ERROR"},
		{"case insensitive", "debug", "DEBUG"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if tt.envValue != "" {
				os.Setenv("LOG_LEVEL", tt.envValue)
				defer os.Unsetenv("LOG_LEVEL")
			} else {
				os.Unsetenv("LOG_LEVEL")
			}

			level := getLogLevel()
			assert.Equal(t, tt.expected, level.String())
		})
	}
}

func TestDefaultLogger(t *testing.T) {
	// Ensure clean state
	os.Unsetenv("LOG_LEVEL")

	t.Run("Info logging outputs JSON", func(t *testing.T) {
		var buf bytes.Buffer
		logger := NewLoggerWithWriter(&buf)

		logger.Info("test message %s", "arg")

		output := buf.String()

		// JSON output should contain level and message
		assert.Contains(t, output, `"level":"INFO"`)
		assert.Contains(t, output, `"msg":"test message arg"`)
	})

	t.Run("Error logging outputs JSON", func(t *testing.T) {
		var buf bytes.Buffer
		logger := NewLoggerWithWriter(&buf)

		logger.Error("error message")

		output := buf.String()

		assert.Contains(t, output, `"level":"ERROR"`)
		assert.Contains(t, output, `"msg":"error message"`)
	})

	t.Run("Warn logging outputs JSON", func(t *testing.T) {
		var buf bytes.Buffer
		logger := NewLoggerWithWriter(&buf)

		logger.Warn("warning message")

		output := buf.String()

		assert.Contains(t, output, `"level":"WARN"`)
		assert.Contains(t, output, `"msg":"warning message"`)
	})

	t.Run("Debug logging respects log level", func(t *testing.T) {
		os.Setenv("LOG_LEVEL", "DEBUG")
		defer os.Unsetenv("LOG_LEVEL")

		var buf bytes.Buffer
		logger := NewLoggerWithWriter(&buf)

		logger.Debug("debug message")

		output := buf.String()

		assert.Contains(t, output, `"level":"DEBUG"`)
		assert.Contains(t, output, `"msg":"debug message"`)
	})

	t.Run("Debug suppressed at INFO level", func(t *testing.T) {
		os.Setenv("LOG_LEVEL", "INFO")
		defer os.Unsetenv("LOG_LEVEL")

		var buf bytes.Buffer
		logger := NewLoggerWithWriter(&buf)

		logger.Debug("debug message")

		output := buf.String()

		// Debug should not be output at INFO level
		assert.Empty(t, output)
	})

	t.Run("Nil logger initializes on first use", func(t *testing.T) {
		logger := &DefaultLogger{} // Not using NewLogger()

		// Capture stdout
		old := os.Stdout
		r, w, _ := os.Pipe()
		os.Stdout = w

		// Should not panic and should output
		logger.Info("auto-initialized")

		w.Close()
		os.Stdout = old
		var buf bytes.Buffer
		io.Copy(&buf, r)
		output := buf.String()

		assert.Contains(t, output, "auto-initialized")
	})
}

func TestFormatMessage(t *testing.T) {
	tests := []struct {
		name     string
		msg      string
		fields   []interface{}
		expected string
	}{
		{"no fields", "simple message", nil, "simple message"},
		{"with format args", "hello %s", []interface{}{"world"}, "hello world"},
		{"multiple args", "%s: %d", []interface{}{"count", 42}, "count: 42"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := formatMessage(tt.msg, tt.fields...)
			assert.Equal(t, tt.expected, result)
		})
	}
}

func TestIsSlogStyle(t *testing.T) {
	tests := []struct {
		name     string
		msg      string
		fields   []interface{}
		expected bool
	}{
		{"empty fields", "test", []interface{}{}, false},
		{"single string (printf)", "test %s", []interface{}{"value"}, false},
		{"odd number of fields", "test", []interface{}{"key", "value", "extra"}, false},
		{"key-value pairs", "test message", []interface{}{"key", "value"}, true},
		{"multiple key-value pairs", "test", []interface{}{"key1", "val1", "key2", 42}, true},
		{"non-string key", "test", []interface{}{123, "value"}, false},
		{"nil fields", "test", nil, false},
		{"format verb %s", "hello %s", []interface{}{"key", "value"}, false},
		{"format verb %d", "count %d", []interface{}{"key", 42}, false},
		{"format verb %v", "value %v", []interface{}{"key", "value"}, false},
		{"no format verb with pairs", "simple message", []interface{}{"key", "value"}, true},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := isSlogStyle(tt.msg, tt.fields)
			assert.Equal(t, tt.expected, result)
		})
	}
}

func TestSlogStyleLogging(t *testing.T) {
	t.Run("slog-style key-value pairs", func(t *testing.T) {
		var buf bytes.Buffer
		logger := NewLoggerWithWriter(&buf)

		// slog-style key-value pairs - use interface type to avoid go vet false positive
		var l Logger = logger
		l.Info("test message", "key1", "value1", "key2", 42)

		output := buf.String()

		assert.Contains(t, output, `"level":"INFO"`)
		assert.Contains(t, output, `"msg":"test message"`)
		assert.Contains(t, output, `"key1":"value1"`)
		assert.Contains(t, output, `"key2":42`)
	})

	t.Run("printf-style formatting still works", func(t *testing.T) {
		var buf bytes.Buffer
		logger := NewLoggerWithWriter(&buf)

		logger.Info("hello %s world %d", "beautiful", 123)

		output := buf.String()

		assert.Contains(t, output, `"msg":"hello beautiful world 123"`)
	})

	t.Run("mixed styles handled correctly", func(t *testing.T) {
		var buf bytes.Buffer
		logger := NewLoggerWithWriter(&buf)

		// Single string arg (printf with just one %s)
		logger.Info("message with %s", "value")

		output := buf.String()

		assert.Contains(t, output, `"msg":"message with value"`)
	})
}

func TestLogMaster(t *testing.T) {
	// LogMaster uses a package-level logger initialized at startup,
	// so we just verify it doesn't panic (deprecated function)
	t.Run("LogMaster with args does not panic", func(t *testing.T) {
		assert.NotPanics(t, func() {
			LogMaster("Starting server on port %d", 8080)
		})
	})

	t.Run("LogMaster without args does not panic", func(t *testing.T) {
		assert.NotPanics(t, func() {
			LogMaster("Server started")
		})
	})
}

func TestLogWorker(t *testing.T) {
	// LogWorker uses a package-level logger initialized at startup,
	// so we just verify it doesn't panic (deprecated function)
	t.Run("LogWorker with args does not panic", func(t *testing.T) {
		assert.NotPanics(t, func() {
			LogWorker("Processing job %s", "job-123")
		})
	})

	t.Run("LogWorker without args does not panic", func(t *testing.T) {
		assert.NotPanics(t, func() {
			LogWorker("Job completed")
		})
	})
}

func TestTestLogger(t *testing.T) {
	// Use the actual test instance for TestLogger testing
	logger := &TestLogger{T: t}

	t.Run("TestLogger Info with fields", func(t *testing.T) {
		// TestLogger will log to the test output, just verify it doesn't panic
		logger.Info("test message %s", "value")
	})

	t.Run("TestLogger Info without fields", func(t *testing.T) {
		logger.Info("simple message")
	})

	t.Run("TestLogger Error with fields", func(t *testing.T) {
		logger.Error("error occurred %s", "test error")
	})

	t.Run("TestLogger Error without fields", func(t *testing.T) {
		logger.Error("simple error")
	})

	t.Run("TestLogger Debug with fields", func(t *testing.T) {
		logger.Debug("debug info %s", "value")
	})

	t.Run("TestLogger Debug without fields", func(t *testing.T) {
		logger.Debug("simple debug")
	})

	t.Run("TestLogger Warn with fields", func(t *testing.T) {
		logger.Warn("warning info %s", "value")
	})

	t.Run("TestLogger Warn without fields", func(t *testing.T) {
		logger.Warn("simple warning")
	})
}

func TestNopLogger(t *testing.T) {
	logger := &NopLogger{}

	// All methods should do nothing without panicking
	logger.Info("test")
	logger.Error("test")
	logger.Debug("test")
	logger.Warn("test")

	logger.Info("test %s", "arg")
	logger.Error("test %s", "arg")
	logger.Debug("test %s", "arg")
	logger.Warn("test %s", "arg")
}

func TestLoggerInterface(t *testing.T) {
	t.Run("DefaultLogger implements Logger interface", func(t *testing.T) {
		var logger Logger = &DefaultLogger{}
		assert.NotNil(t, logger)

		// Test that all interface methods work
		logger.Info("test")
		logger.Error("test")
		logger.Debug("test")
		logger.Warn("test")
	})

	t.Run("TestLogger implements Logger interface", func(t *testing.T) {
		var logger Logger = &TestLogger{T: t}
		assert.NotNil(t, logger)

		// Test that all interface methods work
		logger.Info("test")
		logger.Error("test")
		logger.Debug("test")
		logger.Warn("test")
	})

	t.Run("NopLogger implements Logger interface", func(t *testing.T) {
		var logger Logger = &NopLogger{}
		assert.NotNil(t, logger)

		// Test that all interface methods work
		logger.Info("test")
		logger.Error("test")
		logger.Debug("test")
		logger.Warn("test")
	})
}
