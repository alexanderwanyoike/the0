package cmd

import (
	"encoding/json"
	"io"
	"strings"
	"time"

	"the0/internal"
)

func logEntryTime(entry internal.LogEntry) string {
	if logTime, err := time.Parse(time.RFC3339, entry.Timestamp); err == nil {
		return logTime.UTC().Format("2006-01-02 15:04:05") + " UTC"
	}
	if day, err := time.Parse("20060102", entry.Date); err == nil {
		return day.Format("2006-01-02")
	}
	return entry.Date
}

func logEntryText(content string) string {
	members, ok := jsonObjectMembers(content)
	if !ok {
		return content
	}

	fields := make(map[string]json.RawMessage, len(members))
	for _, member := range members {
		fields[member.name] = member.value
	}

	// A metric's data is in its fields, not its message.
	if _, isMetric := fields["_metric"]; !isMetric {
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

	return withoutRuntimeKey(content, members)
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

func stringField(fields map[string]json.RawMessage, name string) string {
	var value string
	if raw, ok := fields[name]; ok && json.Unmarshal(raw, &value) == nil {
		return value
	}
	return ""
}

// withoutRuntimeKey returns a line stored before the runtime keyed lines
// byte for byte, as the api returned it then.
func withoutRuntimeKey(content string, members []jsonMember) string {
	var kept []string
	for _, member := range members {
		if member.name == "_key" {
			continue
		}
		name, _ := json.Marshal(member.name)
		kept = append(kept, string(name)+":"+string(member.value))
	}
	if len(kept) == len(members) {
		return content
	}
	return "{" + strings.Join(kept, ",") + "}"
}

type logWatch struct {
	started       bool
	lastTimestamp int64
}

func (w *logWatch) newEntries(logs []internal.LogEntry) []internal.LogEntry {
	var fresh []internal.LogEntry
	if !w.started {
		w.started = true
		fresh = logs
	} else {
		for _, log := range logs {
			if logTime, err := time.Parse(time.RFC3339, log.Date); err == nil {
				if logTime.Unix() > w.lastTimestamp {
					fresh = append(fresh, log)
				}
			}
		}
	}

	for _, log := range fresh {
		if logTime, err := time.Parse(time.RFC3339, log.Date); err == nil {
			if logTime.Unix() > w.lastTimestamp {
				w.lastTimestamp = logTime.Unix()
			}
		}
	}
	return fresh
}
