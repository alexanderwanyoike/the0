package miniologger

import (
	"encoding/json"
	"fmt"
	"strings"
	"time"
)

// FormatLogChunk converts a raw log chunk into NDJSON format.
// Each line becomes valid JSON with a guaranteed "timestamp" field.
// Lines that are already valid JSON with a "timestamp" field pass through unchanged.
// Lines that are valid JSON without "timestamp" get the field added.
// Plain text lines are wrapped in {"timestamp":"...","message":"..."}.
func FormatLogChunk(logs string, ts time.Time) string {
	lines := strings.Split(strings.TrimSpace(logs), "\n")
	var buffer strings.Builder
	isoTS := ts.Format(time.RFC3339)

	for _, line := range lines {
		trimmed := strings.TrimSpace(line)
		if trimmed == "" {
			continue
		}

		var raw interface{}
		if json.Unmarshal([]byte(trimmed), &raw) == nil {
			if obj, ok := raw.(map[string]interface{}); ok {
				if _, hasTS := obj["timestamp"]; !hasTS {
					obj["timestamp"] = isoTS
					normalized, _ := json.Marshal(obj)
					buffer.Write(normalized)
				} else {
					buffer.WriteString(trimmed)
				}
			} else {
				// Valid JSON but not an object (e.g. null, array, scalar) - wrap it
				escaped, _ := json.Marshal(trimmed)
				fmt.Fprintf(&buffer, `{"timestamp":"%s","message":%s}`, isoTS, escaped)
			}
		} else {
			escaped, _ := json.Marshal(trimmed)
			fmt.Fprintf(&buffer, `{"timestamp":"%s","message":%s}`, isoTS, escaped)
		}
		buffer.WriteString("\n")
	}
	return buffer.String()
}
