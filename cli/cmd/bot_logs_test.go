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
