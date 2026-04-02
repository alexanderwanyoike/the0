package miniologger

import (
	"encoding/json"
	"strings"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFormatLogChunk_PlainText(t *testing.T) {
	ts := time.Date(2026, 4, 1, 10, 0, 0, 0, time.UTC)
	output := FormatLogChunk("INFO:main:Starting bot", ts)

	var obj map[string]interface{}
	err := json.Unmarshal([]byte(strings.TrimSpace(output)), &obj)
	require.NoError(t, err, "output should be valid JSON: %s", output)

	assert.Equal(t, "INFO:main:Starting bot", obj["message"])

	tsStr, ok := obj["timestamp"].(string)
	require.True(t, ok, "timestamp should be a string")
	_, err = time.Parse(time.RFC3339, tsStr)
	assert.NoError(t, err, "timestamp should be valid RFC3339: %s", tsStr)
}

func TestFormatLogChunk_JSONWithTimestamp(t *testing.T) {
	ts := time.Date(2026, 4, 1, 10, 0, 0, 0, time.UTC)
	input := `{"_metric":"price","value":42,"timestamp":"2026-04-01T10:00:00Z"}`
	output := FormatLogChunk(input, ts)

	assert.Equal(t, input+"\n", output, "JSON with timestamp should pass through unchanged")
}

func TestFormatLogChunk_JSONWithoutTimestamp(t *testing.T) {
	ts := time.Date(2026, 4, 1, 10, 0, 0, 0, time.UTC)
	input := `{"level":"info","time":1711800000,"msg":"connected"}`
	output := FormatLogChunk(input, ts)

	var obj map[string]interface{}
	err := json.Unmarshal([]byte(strings.TrimSpace(output)), &obj)
	require.NoError(t, err, "output should be valid JSON: %s", output)

	assert.Contains(t, obj, "timestamp", "should have timestamp field added")
	assert.Equal(t, "info", obj["level"], "original level field should be preserved")
	assert.Equal(t, float64(1711800000), obj["time"], "original time field should be preserved")
	assert.Equal(t, "connected", obj["msg"], "original msg field should be preserved")
}

func TestFormatLogChunk_MixedChunk(t *testing.T) {
	ts := time.Date(2026, 4, 1, 10, 0, 0, 0, time.UTC)
	input := `INFO:main:Starting bot
{"_metric":"price","value":42,"timestamp":"2026-04-01T10:00:00Z"}
{"level":"info","msg":"connected"}`

	output := FormatLogChunk(input, ts)
	lines := strings.Split(strings.TrimRight(output, "\n"), "\n")

	assert.Equal(t, 3, len(lines), "should have 3 output lines")

	for i, line := range lines {
		var obj map[string]interface{}
		err := json.Unmarshal([]byte(line), &obj)
		assert.NoError(t, err, "line %d should be valid JSON: %s", i, line)
		assert.Contains(t, obj, "timestamp", "line %d should have a timestamp field", i)
	}
}

func TestFormatLogChunk_SpecialCharacters(t *testing.T) {
	ts := time.Date(2026, 4, 1, 10, 0, 0, 0, time.UTC)
	input := `INFO:main:Quote is "40.09" with 100% confidence`
	output := FormatLogChunk(input, ts)

	var obj map[string]interface{}
	err := json.Unmarshal([]byte(strings.TrimSpace(output)), &obj)
	require.NoError(t, err, "output should be valid JSON: %s", output)

	assert.Equal(t, input, obj["message"], "message should contain original text with quotes preserved")
}

func TestFormatLogChunk_EmptyLines(t *testing.T) {
	ts := time.Date(2026, 4, 1, 10, 0, 0, 0, time.UTC)
	input := "line1\n\n\nline2\n"
	output := FormatLogChunk(input, ts)

	lines := strings.Split(strings.TrimRight(output, "\n"), "\n")
	assert.Equal(t, 2, len(lines), "empty lines should be skipped, expecting exactly 2 lines")
}

func TestFormatLogChunk_NoDoubleTimestamp(t *testing.T) {
	ts := time.Date(2026, 4, 1, 10, 0, 0, 0, time.UTC)
	input := `{"timestamp":"2026-04-01T10:00:00Z","message":"hello"}`
	output := FormatLogChunk(input, ts)

	assert.Equal(t, input+"\n", output, "JSON with timestamp should pass through exactly")

	var obj map[string]interface{}
	err := json.Unmarshal([]byte(strings.TrimSpace(output)), &obj)
	require.NoError(t, err)

	// Count occurrences of "timestamp" in the raw output to confirm no duplication
	count := strings.Count(strings.TrimSpace(output), `"timestamp"`)
	assert.Equal(t, 1, count, "should have exactly one timestamp key")
}

func TestFormatLogChunk_RealProductionLogs(t *testing.T) {
	ts := time.Date(2026, 3, 30, 13, 0, 26, 0, time.UTC)

	// Realistic scheduled market-maker bot log format: mix of plain text, bracket-prefixed, and metrics
	input := `[2026-03-30 13:00:26] INFO:broker.client:Using paper trading
INFO:main:[abc123def456] MM run #1 for ACME (paper)
[2026-03-30 13:00:27] INFO:broker.client:Filtered 2 dust positions (< $1.0)
[2026-03-30 13:00:27] INFO:broker.client:Retrieved 3 positions
INFO:broker.client:Available cash: $50000.00, Buying power: $100000.00
INFO:broker.client:Retrieved 0 orders for ACME
[2026-03-30 13:00:27] INFO:broker.client:Retrieved 0 orders for ACME
INFO:broker.client:Retrieved 0 orders for ACME
INFO:broker.client:Fetched 0 candles for ACME (1m)
INFO:main:Quotes: bid=150.25, ask=None, spread=12.5bps, regime=Sideways
INFO:broker.client:Placing limit buy order for 1.0 ACME
[2026-03-30 13:00:29] INFO:broker.client:Buy order placed successfully: aaaabbbb-cccc-dddd-eeee-ffffffffffff
INFO:main:Posted BID: 1.0 @ $150.25
{"_metric": "inventory", "symbol": "ACME", "position_qty": 0, "position_value": 0, "cash": 50000.0, "equity": 50000.0, "timestamp": "2026-03-30T13:00:27.141337Z"}
{"_metric": "staleness", "symbol": "ACME", "mid_drift_bps": 0.0, "stale_rate_5": 0.0, "timestamp": "2026-03-30T13:00:27.141410Z"}
{"_metric": "quote", "symbol": "ACME", "bid": 150.25, "ask": null, "spread_bps": 12.5, "regime": "Sideways", "mid": 150.30, "timestamp": "2026-03-30T13:00:27.631856Z"}
{"_metric": "momentum", "symbol": "ACME", "spread_multiplier": 1.0, "is_toxic": false, "momentum_bps": 0.0, "trend_3_bps": 0.0, "timestamp": "2026-03-30T13:00:27.631904Z"}
{"_metric": "model", "symbol": "ACME", "reservation_price": 150.30, "mid": 150.30, "skew_from_mid_bps": 0.0, "gamma": 0.1, "timestamp": "2026-03-30T13:00:27.631926Z"}
{"_metric": "order", "symbol": "ACME", "side": "buy", "price": 150.25, "size": 1.5, "status": "posted", "attempts": 1, "reason": null, "timestamp": "2026-03-30T13:00:27.855647Z"}
{"_metric": "cycle", "symbol": "ACME", "regime": "Sideways", "spread_bps": 12.5, "orders_posted": 1, "run_count": 1, "timestamp": "2026-03-30T13:00:27.856524Z"}`

	output := FormatLogChunk(input, ts)
	lines := strings.Split(strings.TrimRight(output, "\n"), "\n")

	assert.Equal(t, 20, len(lines), "should have 20 output lines")

	for i, line := range lines {
		var obj map[string]interface{}
		err := json.Unmarshal([]byte(line), &obj)
		assert.NoError(t, err, "line %d should be valid JSON: %s", i, line)
		assert.Contains(t, obj, "timestamp", "line %d should have a timestamp field", i)
	}

	// Check that metric lines preserved their original timestamp
	metricLines := []int{13, 14, 15, 16, 17, 18, 19} // 0-indexed lines with _metric
	for _, idx := range metricLines {
		var obj map[string]interface{}
		err := json.Unmarshal([]byte(lines[idx]), &obj)
		require.NoError(t, err)
		assert.Contains(t, obj, "_metric", "line %d should have _metric field", idx)
		tsStr, ok := obj["timestamp"].(string)
		require.True(t, ok)
		assert.True(t, strings.HasPrefix(tsStr, "2026-03-30T13:00:27"),
			"metric line %d should preserve original timestamp, got: %s", idx, tsStr)
	}

	// Check that plain text lines have a "message" field
	plainTextLines := []int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12}
	for _, idx := range plainTextLines {
		var obj map[string]interface{}
		err := json.Unmarshal([]byte(lines[idx]), &obj)
		require.NoError(t, err)
		assert.Contains(t, obj, "message", "plain text line %d should have message field", idx)
	}
}
