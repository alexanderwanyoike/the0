package util

import (
	"fmt"
	"io"
	"log/slog"
	"os"
	"strings"
	"testing"
)

// Logger is the interface for structured logging.
// Methods accept a format string and variadic arguments for backward compatibility.
type Logger interface {
	Info(msg string, fields ...interface{})
	Error(msg string, fields ...interface{})
	Debug(msg string, fields ...interface{})
	Warn(msg string, fields ...interface{})
}

// DefaultLogger is the production logger using slog with JSON output.
// It respects the LOG_LEVEL environment variable (DEBUG, INFO, WARN, ERROR).
type DefaultLogger struct {
	logger *slog.Logger
}

// NewLogger creates a new structured JSON logger writing to stdout.
// It reads LOG_LEVEL from the environment (default: INFO).
func NewLogger() *DefaultLogger {
	return NewLoggerWithWriter(os.Stdout)
}

// NewLoggerWithWriter creates a new structured JSON logger writing to the specified writer.
// It reads LOG_LEVEL from the environment (default: INFO).
func NewLoggerWithWriter(w io.Writer) *DefaultLogger {
	level := getLogLevel()
	handler := slog.NewJSONHandler(w, &slog.HandlerOptions{
		Level: level,
	})
	return &DefaultLogger{
		logger: slog.New(handler),
	}
}

// getLogLevel reads LOG_LEVEL from environment and returns the corresponding slog.Level.
func getLogLevel() slog.Level {
	switch strings.ToUpper(os.Getenv("LOG_LEVEL")) {
	case "DEBUG":
		return slog.LevelDebug
	case "WARN":
		return slog.LevelWarn
	case "ERROR":
		return slog.LevelError
	default:
		return slog.LevelInfo
	}
}

// isSlogStyle detects if fields look like slog key-value pairs.
// Returns true if fields have an even count, all keys (even positions) are strings,
// and the message doesn't contain printf format verbs.
func isSlogStyle(msg string, fields []interface{}) bool {
	if len(fields) == 0 {
		return false
	}
	// If message contains format verbs, treat as printf-style
	if containsFormatVerbs(msg) {
		return false
	}
	// Must have an even number of args for key-value pairs
	if len(fields)%2 != 0 {
		return false
	}
	// Check if even positions (keys) are strings
	for i := 0; i < len(fields); i += 2 {
		if _, ok := fields[i].(string); !ok {
			return false
		}
	}
	return true
}

// containsFormatVerbs checks if a string contains printf-style format verbs.
func containsFormatVerbs(s string) bool {
	for i := 0; i < len(s)-1; i++ {
		if s[i] == '%' {
			next := s[i+1]
			// Common format verbs: %s, %d, %v, %f, %t, %x, %q, %p, %e, %g, %b, %o, %c, %U
			if next == 's' || next == 'd' || next == 'v' || next == 'f' ||
				next == 't' || next == 'x' || next == 'X' || next == 'q' ||
				next == 'p' || next == 'e' || next == 'E' || next == 'g' ||
				next == 'G' || next == 'b' || next == 'o' || next == 'c' ||
				next == 'U' || next == '+' || next == '#' {
				return true
			}
		}
	}
	return false
}

// formatMessage formats a message with printf-style variadic arguments.
func formatMessage(msg string, fields ...interface{}) string {
	if len(fields) == 0 {
		return msg
	}
	return fmt.Sprintf(msg, fields...)
}

func (l *DefaultLogger) Info(msg string, fields ...interface{}) {
	if l.logger == nil {
		l.logger = NewLogger().logger
	}
	if isSlogStyle(msg, fields) {
		l.logger.Info(msg, fields...)
	} else if len(fields) > 0 {
		l.logger.Info(fmt.Sprintf(msg, fields...))
	} else {
		l.logger.Info(msg)
	}
}

func (l *DefaultLogger) Error(msg string, fields ...interface{}) {
	if l.logger == nil {
		l.logger = NewLogger().logger
	}
	if isSlogStyle(msg, fields) {
		l.logger.Error(msg, fields...)
	} else if len(fields) > 0 {
		l.logger.Error(fmt.Sprintf(msg, fields...))
	} else {
		l.logger.Error(msg)
	}
}

func (l *DefaultLogger) Debug(msg string, fields ...interface{}) {
	if l.logger == nil {
		l.logger = NewLogger().logger
	}
	if isSlogStyle(msg, fields) {
		l.logger.Debug(msg, fields...)
	} else if len(fields) > 0 {
		l.logger.Debug(fmt.Sprintf(msg, fields...))
	} else {
		l.logger.Debug(msg)
	}
}

func (l *DefaultLogger) Warn(msg string, fields ...interface{}) {
	if l.logger == nil {
		l.logger = NewLogger().logger
	}
	if isSlogStyle(msg, fields) {
		l.logger.Warn(msg, fields...)
	} else if len(fields) > 0 {
		l.logger.Warn(fmt.Sprintf(msg, fields...))
	} else {
		l.logger.Warn(msg)
	}
}

// TestLogger is a logger for tests that uses testing.T for output.
type TestLogger struct {
	T *testing.T
}

func (l *TestLogger) Info(msg string, fields ...interface{}) {
	l.T.Log("INFO: " + formatLogMessage(msg, fields...))
}

func (l *TestLogger) Error(msg string, fields ...interface{}) {
	l.T.Log("ERROR: " + formatLogMessage(msg, fields...))
}

func (l *TestLogger) Debug(msg string, fields ...interface{}) {
	l.T.Log("DEBUG: " + formatLogMessage(msg, fields...))
}

func (l *TestLogger) Warn(msg string, fields ...interface{}) {
	l.T.Log("WARN: " + formatLogMessage(msg, fields...))
}

// formatLogMessage formats a log message, handling both slog-style and printf-style.
func formatLogMessage(msg string, fields ...interface{}) string {
	if len(fields) == 0 {
		return msg
	}
	if isSlogStyle(msg, fields) {
		// Format as key=value pairs
		result := msg
		for i := 0; i < len(fields); i += 2 {
			result += fmt.Sprintf(" %s=%v", fields[i], fields[i+1])
		}
		return result
	}
	return fmt.Sprintf(msg, fields...)
}

// NopLogger is a logger that discards all output.
// Useful for tests that don't need log output.
type NopLogger struct{}

func (l *NopLogger) Info(msg string, fields ...interface{})  {}
func (l *NopLogger) Error(msg string, fields ...interface{}) {}
func (l *NopLogger) Debug(msg string, fields ...interface{}) {}
func (l *NopLogger) Warn(msg string, fields ...interface{})  {}

// masterLogger is a shared logger for master process logs.
var masterLogger = NewLogger()

// workerLogger is a shared logger for worker process logs.
var workerLogger = NewLogger()

// LogMaster logs a message with the "master" component prefix.
// Deprecated: Use Logger interface methods with explicit component identification instead.
func LogMaster(msg string, args ...interface{}) {
	masterLogger.logger.Info(formatMessage(msg, args...), "component", "master")
}

// LogWorker logs a message with the "worker" component prefix.
// Deprecated: Use Logger interface methods with explicit component identification instead.
func LogWorker(msg string, args ...interface{}) {
	workerLogger.logger.Info(formatMessage(msg, args...), "component", "worker")
}
