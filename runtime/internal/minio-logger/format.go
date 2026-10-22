package miniologger

import (
	"encoding/json"
	"fmt"
	"strings"
	"time"
)

// entryTimestampLayout is UTC ISO 8601 with milliseconds, the timestamp
// contract the SDKs write, so runtime-stamped and bot-stamped lines compare.
const entryTimestampLayout = "2006-01-02T15:04:05.000Z07:00"

// FormatLogChunk converts a raw log chunk into NDJSON, one JSON object per
// non-empty line. JSON objects keep their fields and gain a "timestamp" when
// they have none; anything else is wrapped as {"message": line}. Every line
// gets "_key" = "<ts unix ns>-<index of the emitted line>", replacing any key
// the bot wrote, so the same chunk and ts always produce the same bytes.
func FormatLogChunk(logs string, ts time.Time) string {
	isoTS, _ := json.Marshal(ts.UTC().Format(entryTimestampLayout))
	keyPrefix := fmt.Sprintf("%d-", ts.UnixNano())

	var buffer strings.Builder
	index := 0
	for _, line := range strings.Split(logs, "\n") {
		trimmed := strings.TrimSpace(line)
		if trimmed == "" {
			continue
		}

		// RawMessage values re-encode as the bot wrote them, so large
		// integers do not lose precision through float64.
		var entry map[string]json.RawMessage
		if json.Unmarshal([]byte(trimmed), &entry) != nil || entry == nil {
			message, _ := json.Marshal(trimmed)
			entry = map[string]json.RawMessage{"message": message}
		}
		if _, hasTS := entry["timestamp"]; !hasTS {
			entry["timestamp"] = isoTS
		}
		entry["_key"], _ = json.Marshal(fmt.Sprintf("%s%d", keyPrefix, index))

		encoded, _ := json.Marshal(entry)
		buffer.Write(encoded)
		buffer.WriteString("\n")
		index++
	}
	return buffer.String()
}
