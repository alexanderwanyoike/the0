package cmd

import (
	"encoding/json"
	"io"
	"strings"
	"time"

	"the0/internal"
)

func logEntryTime(entry internal.LogEntry) string {
	if entry.Timestamp != "" {
		logTime, err := time.Parse(time.RFC3339, entry.Timestamp)
		if err != nil {
			return entry.Timestamp
		}
		return logTime.UTC().Format("2006-01-02 15:04:05") + " UTC"
	}
	if day, err := time.Parse("20060102", entry.Date); err == nil {
		return day.Format("2006-01-02")
	}
	return entry.Date
}

func logEntryText(entry internal.LogEntry) string {
	content := entry.Content
	members, ok := jsonObjectMembers(content)
	if !ok {
		return content
	}

	fields := make(map[string]json.RawMessage, len(members))
	for _, member := range members {
		fields[member.name] = member.value
	}

	// A metric's data is in its fields, not its message.
	if !jsTruthy(fields["_metric"]) {
		message := stringField(fields, "message")
		if message == "" {
			message = stringField(fields, "msg")
		}
		if message != "" {
			if level := stringField(fields, "level"); level != "" {
				return strings.ToUpper(level) + " " + message
			}
			return message
		}
	}

	return withoutRuntimeKey(content, members, entry.Key)
}

type jsonMember struct {
	name  string
	value json.RawMessage
}

// jsonObjectMembers keeps members in the order and number formatting the bot
// wrote them, which decoding into a map would lose.
func jsonObjectMembers(content string) ([]jsonMember, bool) {
	decoder := json.NewDecoder(strings.NewReader(content))
	if token, err := decoder.Token(); err != nil || token != json.Delim('{') {
		return nil, false
	}

	var members []jsonMember
	for decoder.More() {
		token, err := decoder.Token()
		if err != nil {
			return nil, false
		}
		name, ok := token.(string)
		if !ok {
			return nil, false
		}
		var value json.RawMessage
		if err := decoder.Decode(&value); err != nil {
			return nil, false
		}
		members = append(members, jsonMember{name: name, value: value})
	}

	if token, err := decoder.Token(); err != nil || token != json.Delim('}') {
		return nil, false
	}
	if _, err := decoder.Token(); err != io.EOF {
		return nil, false
	}
	return members, true
}

// jsTruthy is JavaScript's Boolean(), the api's rule for a metric line.
func jsTruthy(raw json.RawMessage) bool {
	var value interface{}
	if raw == nil || json.Unmarshal(raw, &value) != nil {
		return false
	}
	switch v := value.(type) {
	case nil:
		return false
	case bool:
		return v
	case string:
		return v != ""
	case float64:
		return v != 0
	default:
		return true
	}
}

func stringField(fields map[string]json.RawMessage, name string) string {
	var value string
	if raw, ok := fields[name]; ok && json.Unmarshal(raw, &value) == nil {
		return value
	}
	return ""
}

// withoutRuntimeKey returns a line stored before the runtime keyed lines
// byte for byte, as the api returned it then. A `_key` is the runtime's only
// when the api took it as the entry key; any other is the bot's own field.
func withoutRuntimeKey(content string, members []jsonMember, entryKey string) string {
	var kept []string
	for _, member := range members {
		var key string
		if member.name == "_key" && json.Unmarshal(member.value, &key) == nil && key == entryKey {
			continue
		}
		kept = append(kept, jsonString(member.name)+":"+string(member.value))
	}
	if len(kept) == len(members) {
		return content
	}
	return "{" + strings.Join(kept, ",") + "}"
}

// jsonString leaves <, > and & as written; json.Marshal escapes them for
// HTML, which a terminal is not.
func jsonString(value string) string {
	var buffer strings.Builder
	encoder := json.NewEncoder(&buffer)
	encoder.SetEscapeHTML(false)
	_ = encoder.Encode(value)
	return strings.TrimSuffix(buffer.String(), "\n")
}

// logWatch remembers only the previous fetch's keys: each fetch is the newest
// `limit` entries, so an entry that left that window never comes back and
// the set stays bounded however long the watch runs.
type logWatch struct {
	limit    int
	previous map[string]struct{}
}

func newLogWatch(limit int) *logWatch {
	return &logWatch{limit: limit}
}

func watchLogsParams(dateParam string, limit int) *internal.LogsParams {
	return botLogsParams(dateParam, limit)
}

func (w *logWatch) newEntries(logs []internal.LogEntry) (fresh []internal.LogEntry, mayHaveSkipped bool) {
	// An empty answer (a day not written yet, a store hiccup) says nothing
	// about which lines are still in the window.
	if len(logs) == 0 {
		return nil, false
	}

	current := make(map[string]struct{}, len(logs))
	for _, log := range logs {
		key := watchKey(log)
		current[key] = struct{}{}
		if _, printed := w.previous[key]; !printed {
			fresh = append(fresh, log)
		}
	}
	// Every fetched line being new means the window moved past all the
	// lines printed last time, so lines between the two may be gone.
	mayHaveSkipped = len(w.previous) > 0 && len(fresh) == len(logs)
	w.previous = current
	return fresh, mayHaveSkipped
}

// watchKey falls back for an api that predates entry keys. That api sends
// the bare message as content, so the timestamp is what tells a bot's
// repeated messages apart; only repeats within one timestamp print once.
func watchKey(entry internal.LogEntry) string {
	if entry.Key != "" {
		return entry.Key
	}
	return entry.Date + "|" + entry.Timestamp + "|" + entry.Content
}
