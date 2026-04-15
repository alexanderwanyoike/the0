package dev

import (
	"encoding/json"
	"fmt"
	"io"
	"strings"
	"sync"
	"time"
)

// TerminalSink pretty-prints events to a writer for the default `the0 dev`
// output. Colouring is done with raw ANSI because the CLI doesn't depend on
// a heavyweight TUI library and we only need four colours.
type TerminalSink struct {
	w   io.Writer
	mu  sync.Mutex
	col bool
}

func NewTerminalSink(w io.Writer, colour bool) *TerminalSink {
	return &TerminalSink{w: w, col: colour}
}

const (
	ansiReset  = "\x1b[0m"
	ansiDim    = "\x1b[2m"
	ansiRed    = "\x1b[31m"
	ansiGreen  = "\x1b[32m"
	ansiYellow = "\x1b[33m"
	ansiBlue   = "\x1b[34m"
	ansiPurple = "\x1b[35m"
	ansiCyan   = "\x1b[36m"
)

func (t *TerminalSink) colour(code, s string) string {
	if !t.col {
		return s
	}
	return code + s + ansiReset
}

func (t *TerminalSink) Emit(ev Event) {
	t.mu.Lock()
	defer t.mu.Unlock()

	ts := ev.Timestamp.Format(time.TimeOnly)
	stamp := t.colour(ansiDim, ts)

	switch ev.Kind {
	case EventMetric:
		var payload map[string]any
		_ = json.Unmarshal(ev.Data, &payload)
		delete(payload, "_metric")
		fmt.Fprintf(t.w, "%s %s %s %s\n",
			stamp,
			t.colour(ansiCyan, "METRIC"),
			t.colour(ansiCyan, ev.MetricType),
			compactJSON(payload),
		)
	case EventLog:
		levelColour := ansiBlue
		switch strings.ToUpper(ev.Level) {
		case "WARN", "WARNING":
			levelColour = ansiYellow
		case "ERROR":
			levelColour = ansiRed
		case "DEBUG":
			levelColour = ansiDim
		}
		var payload map[string]any
		_ = json.Unmarshal(ev.Data, &payload)
		msg, _ := payload["message"].(string)
		fmt.Fprintf(t.w, "%s %s %s\n",
			stamp,
			t.colour(levelColour, fmt.Sprintf("%-5s", ev.Level)),
			msg,
		)
	case EventPrint:
		colour := ansiReset
		if ev.Stream == StreamStderr {
			colour = ansiYellow
		}
		fmt.Fprintf(t.w, "%s %s %s\n",
			stamp,
			t.colour(colour, string(ev.Stream)),
			ev.Raw,
		)
	case EventResult:
		fmt.Fprintf(t.w, "%s %s %s\n",
			stamp,
			t.colour(ansiGreen, "RESULT"),
			string(ev.Data),
		)
	case EventRestart:
		fmt.Fprintf(t.w, "\n%s %s\n\n",
			stamp,
			t.colour(ansiPurple, "-- restart --"),
		)
	case EventBotStopped:
		fmt.Fprintf(t.w, "%s %s\n", stamp, t.colour(ansiDim, "bot stopped"))
	}
}

func compactJSON(v any) string {
	b, err := json.Marshal(v)
	if err != nil {
		return "<unprintable>"
	}
	return string(b)
}
