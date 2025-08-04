package util

import (
	"fmt"
	"github.com/robfig/cron/v3"
	"time"
)

func CalculateNextExecutionTime(
	cronExpr string,
	t time.Time,
) (time.Time, error) {
	// Try to parse with seconds first (6-field format)
	parser := cron.NewParser(cron.Second | cron.Minute | cron.Hour | cron.Dom | cron.Month | cron.Dow | cron.Descriptor)
	schedule, err := parser.Parse(cronExpr)
	if err != nil {
		// If that fails, try standard 5-field format (without seconds)
		schedule, err = cron.ParseStandard(cronExpr)
		if err != nil {
			return time.Time{}, fmt.Errorf("failed to parse cron expression: %v", err)
		}
	}

	return schedule.Next(t), nil
}
