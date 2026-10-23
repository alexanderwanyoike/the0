package cmd

import (
	"reflect"
	"testing"
	"time"

	"the0/internal"
)

func TestDefaultLogDate(t *testing.T) {
	t.Run("is the UTC day, the naming of daily log files", func(t *testing.T) {
		// 23:30 on 22 Oct in New York is already 23 Oct in UTC.
		newYork := time.FixedZone("EDT", -4*60*60)
		now := time.Date(2026, 10, 22, 23, 30, 0, 0, newYork)

		if got := defaultLogDate(now); got != "20261023" {
			t.Errorf("defaultLogDate() = %q, want %q", got, "20261023")
		}
	})

	t.Run("is the UTC day when local time is ahead of UTC", func(t *testing.T) {
		tokyo := time.FixedZone("JST", 9*60*60)
		now := time.Date(2026, 10, 23, 2, 0, 0, 0, tokyo)

		if got := defaultLogDate(now); got != "20261022" {
			t.Errorf("defaultLogDate() = %q, want %q", got, "20261022")
		}
	})
}

func TestLogEntryText(t *testing.T) {
	tests := []struct {
		name    string
		key     string
		content string
		want    string
	}{
		{
			name:    "an SDK log line shows its level and message",
			key:     "1792713599999000000-0",
			content: `{"_key":"1792713599999000000-0","level":"info","message":"Bought 1 BTC","timestamp":"2026-10-22T23:59:59.999Z"}`,
			want:    "INFO Bought 1 BTC",
		},
		{
			name:    "a plain text line the runtime wrapped shows its message",
			key:     "1792713599999000000-1",
			content: `{"_key":"1792713599999000000-1","message":"connecting to exchange","timestamp":"2026-10-22T23:59:59.999Z"}`,
			want:    "connecting to exchange",
		},
		{
			name:    "a structured logger line shows its msg",
			key:     "1792713599999000000-2",
			content: `{"_key":"1792713599999000000-2","level":"warn","msg":"order book stale","timestamp":"2026-10-22T23:59:59.999Z"}`,
			want:    "WARN order book stale",
		},
		{
			name:    "message wins over msg",
			content: `{"level":"error","message":"order rejected","msg":"ignored"}`,
			want:    "ERROR order rejected",
		},
		{
			name:    "a level that is not a string is left out",
			content: `{"level":30,"msg":"pino line"}`,
			want:    "pino line",
		},
		{
			name:    "a metric line prints as JSON without its key",
			key:     "1792713599999000000-3",
			content: `{"_key":"1792713599999000000-3","_metric":"price","symbol":"BTC/USD","timestamp":"2026-10-22T23:59:59.999Z","value":45000.5}`,
			want:    `{"_metric":"price","symbol":"BTC/USD","timestamp":"2026-10-22T23:59:59.999Z","value":45000.5}`,
		},
		{
			name:    "a metric line keeps its numbers as written",
			key:     "1792713599999000000-4",
			content: `{"_key":"1792713599999000000-4","_metric":"fill","order_id":12345678901234567890,"price":1.10}`,
			want:    `{"_metric":"fill","order_id":12345678901234567890,"price":1.10}`,
		},
		{
			name:    "a metric line stored before keyed lines prints verbatim",
			content: `{"_metric": "price", "value": 45000.5, "timestamp": "2026-10-22T23:59:59Z"}`,
			want:    `{"_metric": "price", "value": 45000.5, "timestamp": "2026-10-22T23:59:59Z"}`,
		},
		{
			name:    "a metric line is not shown as a message",
			key:     "1792713599999000000-5",
			content: `{"_key":"1792713599999000000-5","_metric":"signal","message":"go long"}`,
			want:    `{"_metric":"signal","message":"go long"}`,
		},
		{
			name:    "an object without a message prints as JSON without its key",
			key:     "1792713599999000000-6",
			content: `{"_key":"1792713599999000000-6","event":"tick","timestamp":"2026-10-22T23:59:59.999Z"}`,
			want:    `{"event":"tick","timestamp":"2026-10-22T23:59:59.999Z"}`,
		},
		{
			name:    "an empty message prints the object without its key",
			key:     "1792713599999000000-7",
			content: `{"_key":"1792713599999000000-7","level":"info","message":""}`,
			want:    `{"level":"info","message":""}`,
		},
		{
			name:    "a message that is not a string prints the object without its key",
			key:     "1792713599999000000-8",
			content: `{"_key":"1792713599999000000-8","message":{"text":"nested"}}`,
			want:    `{"message":{"text":"nested"}}`,
		},
		{
			name:    "a _key the bot wrote is not the entry key and stays in the line",
			key:     "20261022:0",
			content: `{"_key":"user-42","event":"tick"}`,
			want:    `{"_key":"user-42","event":"tick"}`,
		},
		{
			name:    "a plain text line stored before the runtime wrapped lines prints verbatim",
			content: "Bot started on BTC/USD",
			want:    "Bot started on BTC/USD",
		},
		{
			name:    "JSON that is not an object prints verbatim",
			content: `[1,2,3]`,
			want:    `[1,2,3]`,
		},
		{
			name:    "a line that only looks like JSON prints verbatim",
			content: `{not json at all`,
			want:    `{not json at all`,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := logEntryText(internal.LogEntry{Key: tt.key, Content: tt.content}); got != tt.want {
				t.Errorf("logEntryText() =\n  %s\nwant\n  %s", got, tt.want)
			}
		})
	}
}

func TestLogEntryTime(t *testing.T) {
	tests := []struct {
		name  string
		entry internal.LogEntry
		want  string
	}{
		{
			name:  "shows the line's own timestamp",
			entry: internal.LogEntry{Date: "20261022", Timestamp: "2026-10-22T23:59:59.999Z"},
			want:  "2026-10-22 23:59:59 UTC",
		},
		{
			name:  "shows a timestamp written with an offset in UTC",
			entry: internal.LogEntry{Date: "20261022", Timestamp: "2026-10-22T10:00:00+02:00"},
			want:  "2026-10-22 08:00:00 UTC",
		},
		{
			name:  "falls back to the day of the log file without a timestamp",
			entry: internal.LogEntry{Date: "20261022"},
			want:  "2026-10-22",
		},
		{
			name:  "shows an unreadable timestamp as written",
			entry: internal.LogEntry{Date: "20261022", Timestamp: "yesterday"},
			want:  "yesterday",
		},
		{
			name:  "shows a timestamp without a zone as written",
			entry: internal.LogEntry{Date: "20261022", Timestamp: "2026-10-22T10:00:00"},
			want:  "2026-10-22T10:00:00",
		},
		{
			name:  "shows an unreadable date as sent",
			entry: internal.LogEntry{Date: "today"},
			want:  "today",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := logEntryTime(tt.entry); got != tt.want {
				t.Errorf("logEntryTime() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestBotLogsParams(t *testing.T) {
	t.Run("asks for one day's newest entries oldest first", func(t *testing.T) {
		got := botLogsParams("20261022", 100)

		want := &internal.LogsParams{Date: "20261022", Limit: 100, Sort: "asc"}
		if !reflect.DeepEqual(got, want) {
			t.Errorf("botLogsParams() = %+v, want %+v", got, want)
		}
	})

	t.Run("asks for a date range's newest entries oldest first", func(t *testing.T) {
		got := botLogsParams("20261020-20261022", 50)

		want := &internal.LogsParams{DateRange: "20261020-20261022", Limit: 50, Sort: "asc"}
		if !reflect.DeepEqual(got, want) {
			t.Errorf("botLogsParams() = %+v, want %+v", got, want)
		}
	})
}

func watchEntries(keys ...string) []internal.LogEntry {
	entries := make([]internal.LogEntry, len(keys))
	for i, key := range keys {
		entries[i] = internal.LogEntry{Key: key, Date: "20261022", Content: "line " + key}
	}
	return entries
}

func TestLogWatchNewEntries(t *testing.T) {
	t.Run("prints the first fetch in full", func(t *testing.T) {
		watch := &logWatch{}

		got := watch.newEntries(watchEntries("a", "b"))

		if want := watchEntries("a", "b"); !reflect.DeepEqual(got, want) {
			t.Errorf("newEntries() = %v, want %v", got, want)
		}
	})

	t.Run("prints only entries whose key it has not printed", func(t *testing.T) {
		watch := &logWatch{}
		watch.newEntries(watchEntries("a", "b"))

		got := watch.newEntries(watchEntries("a", "b", "c"))

		if want := watchEntries("c"); !reflect.DeepEqual(got, want) {
			t.Errorf("newEntries() = %v, want %v", got, want)
		}
	})

	t.Run("prints nothing when nothing new arrived", func(t *testing.T) {
		watch := &logWatch{}
		watch.newEntries(watchEntries("a", "b"))

		if got := watch.newEntries(watchEntries("a", "b")); len(got) != 0 {
			t.Errorf("newEntries() = %v, want none", got)
		}
	})

	t.Run("prints new entries as the limit window moves on", func(t *testing.T) {
		watch := &logWatch{}
		watch.newEntries(watchEntries("a", "b", "c"))

		got := watch.newEntries(watchEntries("c", "d", "e"))

		if want := watchEntries("d", "e"); !reflect.DeepEqual(got, want) {
			t.Errorf("newEntries() = %v, want %v", got, want)
		}
	})

	t.Run("prints in the api's oldest-first order, like a tail", func(t *testing.T) {
		watch := &logWatch{}
		first := watch.newEntries(watchEntries("a", "b"))
		next := watch.newEntries(watchEntries("b", "c", "d"))

		if got, want := append(first, next...), watchEntries("a", "b", "c", "d"); !reflect.DeepEqual(got, want) {
			t.Errorf("printed = %v, want %v", got, want)
		}
	})
}
