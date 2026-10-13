package desired

import (
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
)

func TestNextExecutionTime(t *testing.T) {
	baseTime := time.Date(2023, 1, 1, 12, 0, 0, 0, time.UTC)

	tests := []struct {
		name        string
		cronExpr    string
		baseTime    time.Time
		expectError bool
		description string
	}{
		{
			name:        "Valid 5-field cron expression - every minute",
			cronExpr:    "* * * * *",
			baseTime:    baseTime,
			expectError: false,
			description: "Should parse standard 5-field cron expression",
		},
		{
			name:        "Invalid cron expression - 6 fields with seconds",
			cronExpr:    "* * * * * *",
			baseTime:    baseTime,
			expectError: true,
			description: "Should reject a seconds field, which Kubernetes CronJobs cannot honour",
		},
		{
			name:        "Valid specific time - daily at 9 AM",
			cronExpr:    "0 9 * * *",
			baseTime:    baseTime,
			expectError: false,
			description: "Should parse specific daily schedule",
		},
		{
			name:        "Valid weekly schedule - Monday at 10:30 AM",
			cronExpr:    "30 10 * * 1",
			baseTime:    baseTime,
			expectError: false,
			description: "Should parse weekly schedule",
		},
		{
			name:        "Valid monthly schedule - first of month at midnight",
			cronExpr:    "0 0 1 * *",
			baseTime:    baseTime,
			expectError: false,
			description: "Should parse monthly schedule",
		},
		{
			name:        "Invalid cron expression - too many fields",
			cronExpr:    "* * * * * * *",
			baseTime:    baseTime,
			expectError: true,
			description: "Should reject invalid cron format",
		},
		{
			name:        "Invalid cron expression - invalid field value",
			cronExpr:    "60 * * * *",
			baseTime:    baseTime,
			expectError: true,
			description: "Should reject invalid minute value",
		},
		{
			name:        "Empty cron expression",
			cronExpr:    "",
			baseTime:    baseTime,
			expectError: true,
			description: "Should reject empty cron expression",
		},
		{
			name:        "Valid predefined schedule - @hourly",
			cronExpr:    "@hourly",
			baseTime:    baseTime,
			expectError: false,
			description: "Should parse predefined @hourly schedule",
		},
		{
			name:        "Valid predefined schedule - @daily",
			cronExpr:    "@daily",
			baseTime:    baseTime,
			expectError: false,
			description: "Should parse predefined @daily schedule",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			nextTime, err := NextExecutionTime(tt.cronExpr, tt.baseTime)

			if tt.expectError {
				assert.Error(t, err, tt.description)
				assert.Equal(t, time.Time{}, nextTime, "Should return zero time on error")
			} else {
				assert.NoError(t, err, tt.description)
				assert.True(t, nextTime.After(tt.baseTime), "Next execution time should be after base time")
			}
		})
	}
}

func TestNextExecutionTimeSpecificCases(t *testing.T) {
	t.Run("Every minute should advance by one minute", func(t *testing.T) {
		baseTime := time.Date(2023, 1, 1, 12, 30, 0, 0, time.UTC)
		nextTime, err := NextExecutionTime("* * * * *", baseTime)

		assert.NoError(t, err)
		expectedNext := time.Date(2023, 1, 1, 12, 31, 0, 0, time.UTC)
		assert.Equal(t, expectedNext, nextTime)
	})

	t.Run("Daily at 9 AM should advance to next 9 AM", func(t *testing.T) {
		baseTime := time.Date(2023, 1, 1, 12, 0, 0, 0, time.UTC) // Sunday noon
		nextTime, err := NextExecutionTime("0 9 * * *", baseTime)

		assert.NoError(t, err)
		expectedNext := time.Date(2023, 1, 2, 9, 0, 0, 0, time.UTC) // Monday 9 AM
		assert.Equal(t, expectedNext, nextTime)
	})

	t.Run("Weekly on Monday should advance to next Monday", func(t *testing.T) {
		baseTime := time.Date(2023, 1, 1, 12, 0, 0, 0, time.UTC) // Sunday
		nextTime, err := NextExecutionTime("0 10 * * 1", baseTime)

		assert.NoError(t, err)
		expectedNext := time.Date(2023, 1, 2, 10, 0, 0, 0, time.UTC) // Monday 10 AM
		assert.Equal(t, expectedNext, nextTime)
	})

	t.Run("Six-field format with seconds is rejected rather than trimmed", func(t *testing.T) {
		baseTime := time.Date(2023, 1, 1, 12, 30, 30, 0, time.UTC)
		nextTime, err := NextExecutionTime("30 0 9 * * *", baseTime)

		assert.Error(t, err)
		assert.Equal(t, time.Time{}, nextTime)
	})
}
