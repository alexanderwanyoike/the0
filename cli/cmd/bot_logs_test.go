package cmd

import (
	"reflect"
	"testing"

	"the0/internal"
)

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
