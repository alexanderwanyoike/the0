package dockerrunner

import (
	"encoding/json"
	"strings"
)

// ResultMarker is the prefix used to identify bot result output.
// All output functions in SDKs should prefix their JSON result with this marker.
const ResultMarker = "THE0_RESULT:"

// ParsedOutput contains the parsed result from container output.
// It separates the bot's result (if any) from general log output.
type ParsedOutput struct {
	// Result is the JSON result string (empty if no marker found)
	Result string

	// Logs is everything that's not the result (debug output, metrics, etc.)
	Logs string

	// HasMarker indicates whether a THE0_RESULT marker was found
	HasMarker bool
}

// ParseContainerOutput scans stdout for THE0_RESULT marker and separates
// the result JSON from logs. Only the LAST occurrence of the marker is used,
// allowing bots to output intermediate status that gets overwritten.
//
// If no marker is found, returns the combined output as Result for backwards
// compatibility with bots that don't use the marker protocol.
func ParseContainerOutput(stdout, stderr string) ParsedOutput {
	lines := strings.Split(stdout, "\n")

	var resultLine string
	var resultIdx int = -1
	var logLines []string

	// Find the LAST occurrence of the marker
	for i, line := range lines {
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, ResultMarker) {
			resultLine = strings.TrimPrefix(trimmed, ResultMarker)
			resultIdx = i
		}
	}

	if resultIdx == -1 {
		// No marker found - backwards compatibility mode
		// Return combined output as result (existing behavior)
		return ParsedOutput{
			Result:    stdout + stderr,
			Logs:      "",
			HasMarker: false,
		}
	}

	// Collect all non-result lines as logs
	for i, line := range lines {
		if i != resultIdx {
			logLines = append(logLines, line)
		}
	}

	// Combine stdout logs with stderr
	logs := strings.Join(logLines, "\n")
	if stderr != "" {
		if logs != "" {
			logs = logs + "\n" + stderr
		} else {
			logs = stderr
		}
	}

	return ParsedOutput{
		Result:    resultLine,
		Logs:      strings.TrimSpace(logs),
		HasMarker: true,
	}
}

// ValidateResultJSON ensures the parsed result is valid JSON.
// Returns nil if valid, error otherwise.
func ValidateResultJSON(result string) error {
	if result == "" {
		return nil
	}
	var js json.RawMessage
	return json.Unmarshal([]byte(result), &js)
}
