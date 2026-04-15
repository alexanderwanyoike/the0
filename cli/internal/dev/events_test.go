package dev

import (
	"encoding/json"
	"testing"
	"time"
)

func mustJSON(t *testing.T, v any) json.RawMessage {
	t.Helper()
	b, err := json.Marshal(v)
	if err != nil {
		t.Fatalf("mustJSON: %v", err)
	}
	return b
}

func TestParseLine_MetricOnStdout(t *testing.T) {
	ts := time.Unix(0, 0)
	line := `{"_metric":"price","symbol":"BTC","value":45000}`
	got := ParseLine(StreamStdout, line, ts)

	if got.Kind != EventMetric {
		t.Errorf("Kind = %q, want metric", got.Kind)
	}
	if got.MetricType != "price" {
		t.Errorf("MetricType = %q, want price", got.MetricType)
	}
	if got.Stream != StreamStdout {
		t.Errorf("Stream = %q, want stdout", got.Stream)
	}
	var payload map[string]any
	if err := json.Unmarshal(got.Data, &payload); err != nil {
		t.Fatalf("unmarshal Data: %v", err)
	}
	if payload["symbol"] != "BTC" {
		t.Errorf("payload[symbol] = %v, want BTC", payload["symbol"])
	}
}

func TestParseLine_MetricOnStderr_StillMetric(t *testing.T) {
	line := `{"_metric":"price","value":1}`
	got := ParseLine(StreamStderr, line, time.Now())
	if got.Kind != EventMetric {
		t.Errorf("Kind = %q, want metric (stream-agnostic)", got.Kind)
	}
}

func TestParseLine_LogWithLevelAndMessage(t *testing.T) {
	line := `{"level":"INFO","message":"bot started","timestamp":"2026-01-01T00:00:00Z"}`
	got := ParseLine(StreamStderr, line, time.Now())

	if got.Kind != EventLog {
		t.Errorf("Kind = %q, want log", got.Kind)
	}
	if got.Level != "INFO" {
		t.Errorf("Level = %q, want INFO", got.Level)
	}
}

func TestParseLine_PlainStdoutIsPrint(t *testing.T) {
	got := ParseLine(StreamStdout, "hello world", time.Now())
	if got.Kind != EventPrint {
		t.Errorf("Kind = %q, want print", got.Kind)
	}
	if got.Raw != "hello world" {
		t.Errorf("Raw = %q", got.Raw)
	}
	if got.Stream != StreamStdout {
		t.Errorf("Stream = %q", got.Stream)
	}
}

func TestParseLine_PlainStderrIsPrint(t *testing.T) {
	got := ParseLine(StreamStderr, "cargo: Compiling the0 v0.1.0", time.Now())
	if got.Kind != EventPrint {
		t.Errorf("Kind = %q, want print (plain stderr)", got.Kind)
	}
	if got.Stream != StreamStderr {
		t.Errorf("Stream = %q", got.Stream)
	}
}

func TestParseLine_MalformedJSONIsPrint(t *testing.T) {
	got := ParseLine(StreamStdout, `{"_metric":"broken`, time.Now())
	if got.Kind != EventPrint {
		t.Errorf("Kind = %q, want print (malformed)", got.Kind)
	}
}

func TestParseLine_JSONArrayIsPrint(t *testing.T) {
	got := ParseLine(StreamStdout, `[1,2,3]`, time.Now())
	if got.Kind != EventPrint {
		t.Errorf("Kind = %q, want print (array, not object)", got.Kind)
	}
}

func TestParseLine_JSONObjectWithoutRecognizedKeysIsPrint(t *testing.T) {
	// Generic JSON object that's neither a metric nor a log. SDKs don't emit
	// these, but user code might via json.dumps — surface as print so it still
	// shows, but don't misclassify.
	got := ParseLine(StreamStdout, `{"arbitrary":"value"}`, time.Now())
	if got.Kind != EventPrint {
		t.Errorf("Kind = %q, want print (unrecognized object)", got.Kind)
	}
}

func TestParseLine_StripsANSI(t *testing.T) {
	line := "\x1b[31merror\x1b[0m: something broke"
	got := ParseLine(StreamStderr, line, time.Now())
	if got.Kind != EventPrint {
		t.Fatalf("Kind = %q, want print", got.Kind)
	}
	if got.Raw != "error: something broke" {
		t.Errorf("Raw = %q, want ANSI stripped", got.Raw)
	}
}

func TestParseLine_ANSIAroundJSONDoesNotBreakClassification(t *testing.T) {
	// If colorizing middleware wraps the JSON output, we must still classify
	// it correctly after stripping.
	line := "\x1b[2m" + `{"_metric":"price","value":1}` + "\x1b[0m"
	got := ParseLine(StreamStdout, line, time.Now())
	if got.Kind != EventMetric {
		t.Errorf("Kind = %q, want metric (after ANSI strip)", got.Kind)
	}
}

func TestParseLine_EmptyLine(t *testing.T) {
	got := ParseLine(StreamStdout, "", time.Now())
	if got.Kind != EventPrint {
		t.Errorf("Kind = %q", got.Kind)
	}
	if got.Raw != "" {
		t.Errorf("Raw = %q", got.Raw)
	}
}

func TestParseLine_Unicode(t *testing.T) {
	line := `🚀 hello`
	got := ParseLine(StreamStdout, line, time.Now())
	if got.Kind != EventPrint {
		t.Errorf("Kind = %q", got.Kind)
	}
	if got.Raw != `🚀 hello` {
		t.Errorf("Raw = %q", got.Raw)
	}
}

func TestParseLine_TimestampPreserved(t *testing.T) {
	ts := time.Date(2026, 4, 15, 12, 0, 0, 0, time.UTC)
	got := ParseLine(StreamStdout, "hi", ts)
	if !got.Timestamp.Equal(ts) {
		t.Errorf("Timestamp = %v, want %v", got.Timestamp, ts)
	}
}

func TestParseLine_MetricPayloadPreservesAllFields(t *testing.T) {
	line := `{"_metric":"trade","symbol":"ETH","qty":1.5,"side":"buy"}`
	got := ParseLine(StreamStdout, line, time.Now())
	if got.Kind != EventMetric {
		t.Fatalf("Kind = %q", got.Kind)
	}
	var p map[string]any
	if err := json.Unmarshal(got.Data, &p); err != nil {
		t.Fatalf("unmarshal: %v", err)
	}
	if p["side"] != "buy" {
		t.Errorf("missing side field")
	}
	if p["_metric"] != "trade" {
		t.Errorf("missing _metric field in Data")
	}
}

func TestParseLine_LeadingTrailingWhitespace(t *testing.T) {
	// Runtime wrappers can inject whitespace. Don't false-classify.
	line := "   " + `{"_metric":"x","v":1}` + "\t"
	got := ParseLine(StreamStdout, line, time.Now())
	if got.Kind != EventMetric {
		t.Errorf("Kind = %q, want metric (whitespace tolerant)", got.Kind)
	}
}

func TestParseLine_NonStringLevel(t *testing.T) {
	// If some bot does {"level": 42, "message": "x"}, we shouldn't crash.
	// Current rule: require string level. Fall back to print.
	line := `{"level":42,"message":"x"}`
	got := ParseLine(StreamStdout, line, time.Now())
	if got.Kind != EventPrint {
		t.Errorf("Kind = %q, want print (non-string level)", got.Kind)
	}
}

func TestEvent_JSONRoundTrip(t *testing.T) {
	// Events are persisted to events.jsonl; round-trip through JSON must be stable.
	original := Event{
		Kind:       EventMetric,
		Stream:     StreamStdout,
		Timestamp:  time.Date(2026, 4, 15, 12, 0, 0, 0, time.UTC),
		MetricType: "price",
		Data:       mustJSON(t, map[string]any{"_metric": "price", "value": 1}),
	}
	b, err := json.Marshal(original)
	if err != nil {
		t.Fatalf("marshal: %v", err)
	}
	var decoded Event
	if err := json.Unmarshal(b, &decoded); err != nil {
		t.Fatalf("unmarshal: %v", err)
	}
	if decoded.Kind != original.Kind {
		t.Errorf("Kind mismatch: got %q, want %q", decoded.Kind, original.Kind)
	}
	if decoded.MetricType != original.MetricType {
		t.Errorf("MetricType mismatch")
	}
	if !decoded.Timestamp.Equal(original.Timestamp) {
		t.Errorf("Timestamp mismatch")
	}
}
