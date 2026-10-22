package cmd

import (
	"encoding/json"
	"io"
	"strings"
	"time"

	"the0/internal"
)

func logEntryTime(entry internal.LogEntry) string {
	logTime, err := time.Parse(time.RFC3339, entry.Date)
	if err != nil {
		return entry.Date
	}
	return logTime.Format("2006-01-02 15:04:05")
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
