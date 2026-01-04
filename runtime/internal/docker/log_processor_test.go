package docker

import (
	"encoding/json"
	"strings"
	"testing"
)

// Helper to parse JSON and extract fields
func parseJSON(t *testing.T, line string) map[string]interface{} {
	var result map[string]interface{}
	if err := json.Unmarshal([]byte(line), &result); err != nil {
		t.Fatalf("Failed to parse JSON: %s, error: %v", line, err)
	}
	return result
}

func TestProcessLogsToJSONLines_MetricsPassThrough(t *testing.T) {
	// Metrics with _metric field should pass through unchanged
	tests := []struct {
		name  string
		input string
	}{
		{
			name:  "price metric",
			input: `{"symbol":"AAPL","value":271.01,"change_pct":-0.313,"_metric":"price","timestamp":1767479671511}`,
		},
		{
			name:  "sma metric",
			input: `{"symbol":"AAPL","short_sma":272.62,"long_sma":274.82,"_metric":"sma","timestamp":1767479671518}`,
		},
		{
			name:  "custom metric",
			input: `{"_metric":"custom","data":{"nested":"value"},"count":42}`,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := ProcessLogsToJSONLines(tt.input)
			if result != tt.input {
				t.Errorf("Metric should pass through unchanged.\nInput:  %s\nOutput: %s", tt.input, result)
			}

			// Verify _metric field is preserved
			parsed := parseJSON(t, result)
			if _, ok := parsed["_metric"]; !ok {
				t.Error("_metric field should be preserved")
			}
		})
	}
}

func TestProcessLogsToJSONLines_StructuredLogsPassThrough(t *testing.T) {
	// Structured logs (with level/message) should pass through unchanged
	tests := []struct {
		name  string
		input string
	}{
		{
			name:  "info log with message",
			input: `{"level":"info","message":"Bot started successfully","timestamp":1767479671187}`,
		},
		{
			name:  "error log",
			input: `{"level":"error","message":"Connection failed","timestamp":1767479671187}`,
		},
		{
			name:  "pino format with msg",
			input: `{"level":30,"msg":"Processing request","time":1767479671187}`,
		},
		{
			name:  "rust tracing format",
			input: `{"level":"INFO","fields":{"message":"Starting bot"},"target":"bot::main"}`,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := ProcessLogsToJSONLines(tt.input)
			if result != tt.input {
				t.Errorf("Structured log should pass through unchanged.\nInput:  %s\nOutput: %s", tt.input, result)
			}
		})
	}
}

func TestProcessLogsToJSONLines_PlainTextWrapped(t *testing.T) {
	// Plain text logs should be wrapped in JSON
	tests := []struct {
		name          string
		input         string
		expectedLevel string
	}{
		{
			name:          "simple info message",
			input:         "Starting bot...",
			expectedLevel: "info",
		},
		{
			name:          "message with error keyword",
			input:         "Error: connection refused",
			expectedLevel: "error",
		},
		{
			name:          "message with warning",
			input:         "Warning: rate limit approaching",
			expectedLevel: "warn",
		},
		{
			name:          "debug message",
			input:         "Debug: variable x = 42",
			expectedLevel: "debug",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := ProcessLogsToJSONLines(tt.input)
			parsed := parseJSON(t, result)

			if parsed["level"] != tt.expectedLevel {
				t.Errorf("Expected level %s, got %s", tt.expectedLevel, parsed["level"])
			}
			if parsed["message"] != tt.input {
				t.Errorf("Expected message %s, got %s", tt.input, parsed["message"])
			}
			if _, ok := parsed["timestamp"]; !ok {
				t.Error("Expected timestamp field")
			}
		})
	}
}

func TestProcessLogsToJSONLines_StackTraceGrouped(t *testing.T) {
	// Stack traces should be grouped into a single entry with ERROR level
	input := `Exception in thread "main" java.lang.NullPointerException
	at com.example.MyClass.method(MyClass.java:10)
	at com.example.Main.main(Main.java:5)`

	result := ProcessLogsToJSONLines(input)
	lines := strings.Split(result, "\n")

	if len(lines) != 1 {
		t.Errorf("Stack trace should be grouped into 1 entry, got %d", len(lines))
	}

	parsed := parseJSON(t, lines[0])
	if parsed["level"] != "error" {
		t.Errorf("Stack trace should have error level, got %s", parsed["level"])
	}

	message := parsed["message"].(string)
	if !strings.Contains(message, "NullPointerException") {
		t.Error("Message should contain exception")
	}
	if !strings.Contains(message, "at com.example.MyClass") {
		t.Error("Message should contain stack trace lines")
	}
}

func TestProcessLogsToJSONLines_CrashWithStackTrace(t *testing.T) {
	// Simulates a bot crash with stack trace
	input := `Unhandled exception. System.IO.DirectoryNotFoundException: Could not find a part of the path '/state/.the0-state/signals.json'.
   at System.IO.FileStream.ValidateFileHandle(SafeFileHandle fileHandle)
   at System.IO.FileStream.CreateFileOpenHandle(FileMode mode, FileShare share)
   at System.IO.FileStream..ctor(String path, FileMode mode)
   at System.IO.File.ReadAllText(String path, Encoding encoding)
   at The0.State.Get[T](String key, T defaultValue) in /sdk/dotnet/State.cs:line 79`

	result := ProcessLogsToJSONLines(input)
	lines := strings.Split(result, "\n")

	if len(lines) != 1 {
		t.Errorf("Crash with stack trace should be grouped into 1 entry, got %d", len(lines))
	}

	parsed := parseJSON(t, lines[0])
	if parsed["level"] != "error" {
		t.Errorf("Crash should have error level, got %s", parsed["level"])
	}

	message := parsed["message"].(string)
	if !strings.Contains(message, "DirectoryNotFoundException") {
		t.Error("Message should contain exception type")
	}
	if !strings.Contains(message, "at System.IO.FileStream") {
		t.Error("Message should contain stack trace")
	}
}

func TestProcessLogsToJSONLines_MixedContent(t *testing.T) {
	// Mix of metrics, structured logs, and plain text
	input := `{"level":"info","message":"Bot started","timestamp":1767479671187}
{"symbol":"AAPL","value":271.01,"_metric":"price","timestamp":1767479671511}
Processing trade...
{"symbol":"AAPL","short_sma":272.62,"_metric":"sma","timestamp":1767479671518}
Error: API rate limit exceeded
	at fetchData (api.js:42)
	at processSymbol (main.js:15)
{"level":"warn","message":"Retrying...","timestamp":1767479671600}`

	result := ProcessLogsToJSONLines(input)
	lines := strings.Split(result, "\n")

	// Should have: structured log, metric, plain text, metric, grouped error, structured log
	if len(lines) != 6 {
		t.Errorf("Expected 6 entries, got %d: %v", len(lines), lines)
	}

	// Line 0: structured log (pass through)
	p0 := parseJSON(t, lines[0])
	if p0["message"] != "Bot started" {
		t.Errorf("Line 0 should be structured log, got: %s", lines[0])
	}

	// Line 1: metric (pass through)
	p1 := parseJSON(t, lines[1])
	if p1["_metric"] != "price" {
		t.Errorf("Line 1 should be price metric, got: %s", lines[1])
	}

	// Line 2: plain text wrapped
	p2 := parseJSON(t, lines[2])
	if p2["message"] != "Processing trade..." {
		t.Errorf("Line 2 should be wrapped plain text, got: %s", lines[2])
	}

	// Line 3: metric (pass through)
	p3 := parseJSON(t, lines[3])
	if p3["_metric"] != "sma" {
		t.Errorf("Line 3 should be sma metric, got: %s", lines[3])
	}

	// Line 4: grouped error with stack trace
	p4 := parseJSON(t, lines[4])
	if p4["level"] != "error" {
		t.Errorf("Line 4 should be error level, got: %s", p4["level"])
	}
	msg4 := p4["message"].(string)
	if !strings.Contains(msg4, "API rate limit") || !strings.Contains(msg4, "at fetchData") {
		t.Errorf("Line 4 should contain error and stack trace, got: %s", msg4)
	}

	// Line 5: structured log (pass through)
	p5 := parseJSON(t, lines[5])
	if p5["message"] != "Retrying..." {
		t.Errorf("Line 5 should be structured log, got: %s", lines[5])
	}
}

func TestProcessLogsToJSONLines_PythonTraceback(t *testing.T) {
	// Python tracebacks with indented lines get grouped with the header
	// The final exception line may be separate if not indented
	input := `Traceback (most recent call last):
  File "/app/main.py", line 42, in process
    result = api.fetch(symbol)
  File "/app/api.py", line 15, in fetch
    raise ConnectionError("Failed to connect")
ConnectionError: Failed to connect`

	result := ProcessLogsToJSONLines(input)
	lines := strings.Split(result, "\n")

	// Should have 2 entries: traceback (grouped) + final error line
	if len(lines) != 2 {
		t.Errorf("Expected 2 entries (traceback + error), got %d", len(lines))
	}

	// First entry should have the traceback header and indented lines
	p0 := parseJSON(t, lines[0])
	msg0 := p0["message"].(string)
	if !strings.Contains(msg0, "Traceback") {
		t.Error("First entry should contain Traceback")
	}
	if !strings.Contains(msg0, "File \"/app/main.py\"") {
		t.Error("First entry should contain stack frames")
	}

	// Second entry should be the error line, detected as error level
	p1 := parseJSON(t, lines[1])
	if p1["level"] != "error" {
		t.Errorf("ConnectionError line should have error level, got %s", p1["level"])
	}
}

func TestProcessLogsToJSONLines_RustPanic(t *testing.T) {
	// Rust panics: first line is the panic, then stack backtrace header, then frames
	input := `thread 'main' panicked at 'called Option::unwrap() on a None value', src/main.rs:42:10
stack backtrace:
   0: rust_begin_unwind
             at /rustc/xxx/library/std/src/panicking.rs:578:5
   1: core::panicking::panic_fmt
             at /rustc/xxx/library/core/src/panicking.rs:67:14
   2: core::panicking::panic
             at /rustc/xxx/library/core/src/panicking.rs:117:5`

	result := ProcessLogsToJSONLines(input)
	lines := strings.Split(result, "\n")

	// Should have 2 entries: panic line + grouped stack backtrace
	if len(lines) != 2 {
		t.Errorf("Expected 2 entries (panic + backtrace), got %d", len(lines))
	}

	// First entry should be the panic line with error level
	p0 := parseJSON(t, lines[0])
	if p0["level"] != "error" {
		t.Errorf("Panic line should have error level, got %s", p0["level"])
	}
	if !strings.Contains(p0["message"].(string), "panicked") {
		t.Error("First entry should contain panic message")
	}

	// Second entry should have the stack backtrace grouped
	p1 := parseJSON(t, lines[1])
	msg1 := p1["message"].(string)
	if !strings.Contains(msg1, "stack backtrace") {
		t.Error("Second entry should contain stack backtrace header")
	}
	if !strings.Contains(msg1, "rust_begin_unwind") {
		t.Error("Second entry should contain stack frames")
	}
}

func TestProcessLogsToJSONLines_EmptyInput(t *testing.T) {
	result := ProcessLogsToJSONLines("")
	if result != "" {
		t.Errorf("Empty input should return empty string, got: %s", result)
	}
}

func TestProcessLogsToJSONLines_OnlyWhitespace(t *testing.T) {
	result := ProcessLogsToJSONLines("   \n\t\n  ")
	if result != "" {
		t.Errorf("Whitespace-only input should return empty string, got: %s", result)
	}
}

func TestProcessLogsToJSONLines_InvalidJSON(t *testing.T) {
	// Invalid JSON should be treated as plain text
	tests := []struct {
		name  string
		input string
	}{
		{
			name:  "truncated JSON",
			input: `{"level":"info","message":"truncated`,
		},
		{
			name:  "malformed JSON",
			input: `{level: "info", message: "no quotes on keys"}`,
		},
		{
			name:  "looks like JSON but isn't",
			input: `{this is not json}`,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := ProcessLogsToJSONLines(tt.input)
			parsed := parseJSON(t, result)

			// Should be wrapped as plain text
			if _, ok := parsed["message"]; !ok {
				t.Error("Invalid JSON should be wrapped with message field")
			}
			if parsed["message"] != tt.input {
				t.Errorf("Message should be the original input, got: %s", parsed["message"])
			}
		})
	}
}

func TestProcessLogsToJSONLines_CrashMarker(t *testing.T) {
	// Crash marker JSON should pass through unchanged
	input := `{"level":"error","message":"Bot crashed with exit code 1","exit_code":1,"timestamp":"2026-01-03T22:00:00Z","bot_id":"abc123"}`

	result := ProcessLogsToJSONLines(input)
	if result != input {
		t.Errorf("Crash marker should pass through unchanged.\nInput:  %s\nOutput: %s", input, result)
	}

	parsed := parseJSON(t, result)
	if parsed["level"] != "error" {
		t.Error("Crash marker level should be preserved")
	}
	if parsed["exit_code"].(float64) != 1 {
		t.Error("Crash marker exit_code should be preserved")
	}
}

func TestIsValidJSON(t *testing.T) {
	tests := []struct {
		input    string
		expected bool
	}{
		{`{"key":"value"}`, true},
		{`{"nested":{"key":"value"}}`, true},
		{`[1,2,3]`, true},
		{`[]`, true},
		{`{}`, true},
		{`{"_metric":"price","value":100}`, true},
		{`plain text`, false},
		{`{invalid}`, false},
		{``, false},
		{`{`, false},
		{`{"key":}`, false},
		{`not json at all`, false},
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			result := isValidJSON(tt.input)
			if result != tt.expected {
				t.Errorf("isValidJSON(%q) = %v, expected %v", tt.input, result, tt.expected)
			}
		})
	}
}
