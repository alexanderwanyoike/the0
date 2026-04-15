// Package dev powers the `the0 dev` local development mode for the0 bots.
//
// This file defines the Event model and the ParseLine classifier used by the
// runner to turn each stdout/stderr line emitted by a bot into a typed event.
// Events are consumed by three sinks: the terminal pretty-printer, the
// append-only events.jsonl log, and the WebSocket broker that feeds the
// dashboard dev shell.
package dev

import (
	"encoding/json"
	"regexp"
	"strings"
	"time"
)

type EventKind string

const (
	EventMetric     EventKind = "metric"
	EventLog        EventKind = "log"
	EventPrint      EventKind = "print"
	EventResult     EventKind = "result"
	EventRestart    EventKind = "restart"
	EventBotStopped EventKind = "bot_stopped"
)

type Stream string

const (
	StreamStdout Stream = "stdout"
	StreamStderr Stream = "stderr"
)

// Event is the normalised unit emitted by the runner. It is persisted to
// events.jsonl and broadcast to WebSocket subscribers verbatim.
type Event struct {
	Kind       EventKind       `json:"kind"`
	Stream     Stream          `json:"stream,omitempty"`
	Timestamp  time.Time       `json:"timestamp"`
	Raw        string          `json:"raw,omitempty"`
	MetricType string          `json:"metric_type,omitempty"`
	Level      string          `json:"level,omitempty"`
	Data       json.RawMessage `json:"data,omitempty"`
}

// ansiPattern strips CSI sequences used for colour/style. Build tools
// (cargo, sbt) emit these to stderr and they must be removed before
// classification so a colourised JSON blob still parses.
var ansiPattern = regexp.MustCompile(`\x1b\[[0-9;?]*[a-zA-Z]`)

func stripANSI(s string) string {
	return ansiPattern.ReplaceAllString(s, "")
}

// ParseLine classifies a single line produced by a bot process.
//
// Classification rules (stream-agnostic unless noted):
//   - Full-line JSON object containing "_metric" (string) → EventMetric
//   - Full-line JSON object containing "level" (string) AND "message" → EventLog
//   - Everything else → EventPrint (carries Stream tag; Raw has ANSI stripped)
//
// ANSI escape sequences are stripped before JSON parsing so colourised
// output from runtime tooling does not break classification.
func ParseLine(stream Stream, line string, ts time.Time) Event {
	clean := stripANSI(line)
	trimmed := strings.TrimSpace(clean)

	if len(trimmed) > 0 && trimmed[0] == '{' {
		var obj map[string]json.RawMessage
		if err := json.Unmarshal([]byte(trimmed), &obj); err == nil {
			// Metric detection
			if rawMetric, ok := obj["_metric"]; ok {
				var metricType string
				if json.Unmarshal(rawMetric, &metricType) == nil && metricType != "" {
					return Event{
						Kind:       EventMetric,
						Stream:     stream,
						Timestamp:  ts,
						MetricType: metricType,
						Data:       json.RawMessage(append([]byte(nil), []byte(trimmed)...)),
					}
				}
			}
			// Log detection
			if rawLevel, ok := obj["level"]; ok {
				if _, hasMsg := obj["message"]; hasMsg {
					var level string
					if json.Unmarshal(rawLevel, &level) == nil && level != "" {
						return Event{
							Kind:      EventLog,
							Stream:    stream,
							Timestamp: ts,
							Level:     level,
							Data:      json.RawMessage(append([]byte(nil), []byte(trimmed)...)),
						}
					}
				}
			}
		}
	}

	return Event{
		Kind:      EventPrint,
		Stream:    stream,
		Timestamp: ts,
		Raw:       clean,
	}
}
