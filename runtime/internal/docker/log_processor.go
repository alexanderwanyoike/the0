// Package docker provides the LogProcessor for converting raw container logs
// into structured JSON-line format.
package docker

import (
	"encoding/json"
	"strings"
	"time"
)

// LogEntry represents a structured log entry for JSON serialization.
type LogEntry struct {
	Level     string `json:"level"`
	Message   string `json:"message"`
	Timestamp string `json:"timestamp"`
}

// ProcessLogsToJSONLines converts raw container logs into JSON-line format.
// It groups multi-line outputs (like stack traces) into single entries.
func ProcessLogsToJSONLines(rawLogs string) string {
	lines := strings.Split(rawLogs, "\n")
	if len(lines) == 0 {
		return ""
	}

	var result []string
	var currentGroup []string
	var groupLevel string
	timestamp := time.Now().UTC().Format(time.RFC3339)

	for _, line := range lines {
		trimmed := strings.TrimSpace(line)
		if trimmed == "" {
			continue
		}

		// Check if this line is a continuation (stack trace, indented output)
		isContinuation := strings.HasPrefix(trimmed, "at ") ||
			strings.HasPrefix(line, "   ") ||
			strings.HasPrefix(line, "\t")

		if isContinuation && len(currentGroup) > 0 {
			// Append to current group
			currentGroup = append(currentGroup, trimmed)
		} else {
			// Flush previous group if any
			if len(currentGroup) > 0 {
				entry := createLogEntry(currentGroup, groupLevel, timestamp)
				result = append(result, entry)
			}

			// Start new group
			currentGroup = []string{trimmed}
			groupLevel = detectLogLevel(trimmed)
		}
	}

	// Flush final group
	if len(currentGroup) > 0 {
		entry := createLogEntry(currentGroup, groupLevel, timestamp)
		result = append(result, entry)
	}

	return strings.Join(result, "\n")
}

// createLogEntry creates a JSON log entry from grouped lines.
func createLogEntry(lines []string, level string, timestamp string) string {
	message := strings.Join(lines, "\n")

	entry := LogEntry{
		Level:     level,
		Message:   message,
		Timestamp: timestamp,
	}

	jsonBytes, err := json.Marshal(entry)
	if err != nil {
		// Fallback to plain text if JSON fails
		return message
	}
	return string(jsonBytes)
}

// detectLogLevel tries to detect the log level from a log line.
func detectLogLevel(line string) string {
	lower := strings.ToLower(line)

	// Check for error indicators
	if strings.Contains(lower, "error") ||
		strings.Contains(lower, "exception") ||
		strings.Contains(lower, "fatal") ||
		strings.Contains(lower, "panic") ||
		strings.Contains(lower, "failed") {
		return "error"
	}

	// Check for warning indicators
	if strings.Contains(lower, "warn") ||
		strings.Contains(lower, "warning") {
		return "warn"
	}

	// Check for debug indicators
	if strings.Contains(lower, "debug") ||
		strings.Contains(lower, "trace") {
		return "debug"
	}

	return "info"
}
