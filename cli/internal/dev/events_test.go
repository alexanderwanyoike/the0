package dev

import (
	"encoding/json"
	"testing"
	"time"
)

// Core classifier coverage: one happy case per event kind, one defensive
// case against malformed input, and the ANSI-strip invariant that matters
// for compiled toolchains wrapping output in colour codes.

func TestParseLine_Metric(t *testing.T) {
	line := `{"_metric":"price","symbol":"BTC","value":45000}`
	got := ParseLine(StreamStdout, line, time.Unix(0, 0))

	if got.Kind != EventMetric {
		t.Fatalf("Kind = %q, want metric", got.Kind)
	}
	if got.MetricType != "price" {
		t.Errorf("MetricType = %q, want price", got.MetricType)
	}
	if got.Stream != StreamStdout {
		t.Errorf("Stream = %q", got.Stream)
	}
	var payload map[string]any
	if err := json.Unmarshal(got.Data, &payload); err != nil {
		t.Fatalf("unmarshal Data: %v", err)
	}
	if payload["symbol"] != "BTC" {
		t.Errorf("payload missing symbol: %v", payload)
	}
}

func TestParseLine_Log(t *testing.T) {
	line := `{"level":"INFO","message":"bot started"}`
	got := ParseLine(StreamStderr, line, time.Now())
	if got.Kind != EventLog {
		t.Errorf("Kind = %q, want log", got.Kind)
	}
	if got.Level != "INFO" {
		t.Errorf("Level = %q", got.Level)
	}
}

func TestParseLine_Print(t *testing.T) {
	got := ParseLine(StreamStdout, "hello world", time.Now())
	if got.Kind != EventPrint {
		t.Errorf("Kind = %q, want print", got.Kind)
	}
	if got.Raw != "hello world" {
		t.Errorf("Raw = %q", got.Raw)
	}
}

// TestParseLine_UnrecognizedIsPrint groups the defensive cases: malformed
// JSON, arrays (not objects), objects missing the metric/log markers, and
// non-string level fields all must fall back to print rather than crash
// or mis-classify.
func TestParseLine_UnrecognizedIsPrint(t *testing.T) {
	cases := map[string]string{
		"malformed":         `{"_metric":"broken`,
		"array":             `[1,2,3]`,
		"unknown object":    `{"arbitrary":"value"}`,
		"non-string level":  `{"level":42,"message":"x"}`,
		"empty":             ``,
	}
	for name, line := range cases {
		t.Run(name, func(t *testing.T) {
			got := ParseLine(StreamStdout, line, time.Now())
			if got.Kind != EventPrint {
				t.Errorf("Kind = %q, want print", got.Kind)
			}
		})
	}
}

// TestParseLine_StripsANSI is load-bearing: cargo and sbt wrap output in
// colour codes, and the raw bytes must be cleaned before JSON parsing
// runs so a colourised metric line still classifies as a metric.
func TestParseLine_StripsANSI(t *testing.T) {
	// Plain print with ANSI: raw must come back stripped.
	plain := ParseLine(StreamStderr, "\x1b[31merror\x1b[0m: broke", time.Now())
	if plain.Kind != EventPrint || plain.Raw != "error: broke" {
		t.Errorf("plain: Kind=%q Raw=%q", plain.Kind, plain.Raw)
	}

	// JSON wrapped in ANSI still classifies as metric.
	wrapped := "\x1b[2m" + `{"_metric":"price","value":1}` + "\x1b[0m"
	got := ParseLine(StreamStdout, wrapped, time.Now())
	if got.Kind != EventMetric {
		t.Errorf("wrapped: Kind = %q, want metric after ANSI strip", got.Kind)
	}
}

func TestParseLine_TimestampPreserved(t *testing.T) {
	ts := time.Date(2026, 4, 15, 12, 0, 0, 0, time.UTC)
	got := ParseLine(StreamStdout, "hi", ts)
	if !got.Timestamp.Equal(ts) {
		t.Errorf("Timestamp = %v, want %v", got.Timestamp, ts)
	}
}
