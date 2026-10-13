package util

import (
	"fmt"
	"github.com/robfig/cron/v3"
	"time"
)

// CalculateNextExecutionTime accepts standard 5-field cron (and descriptors
// such as @daily) only. Kubernetes CronJobs cannot honour a seconds field, so
// both modes refuse one rather than run the schedule at a different time.
func CalculateNextExecutionTime(
	cronExpr string,
	t time.Time,
) (time.Time, error) {
	schedule, err := cron.ParseStandard(cronExpr)
	if err != nil {
		return time.Time{}, fmt.Errorf("failed to parse cron expression: %v", err)
	}

	return schedule.Next(t), nil
}
