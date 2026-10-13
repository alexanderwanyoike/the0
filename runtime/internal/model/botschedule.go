package model

// BotSchedule represents a scheduled bot for cron-based execution.
type BotSchedule struct {
	ID                string                 `bson:"id"`
	NextExecutionTime int64                  `bson:"next_execution_time"`
	Config            map[string]interface{} `bson:"config"`
	CustomBotVersion  CustomBotVersion       `bson:"custom"`
}
