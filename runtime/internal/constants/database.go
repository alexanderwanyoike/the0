package constants

// Database configurations for each service
const (
	// Separate database names to avoid conflicts
	BOT_RUNNER_DB_NAME      = "bot_runner"
	BOT_RUNNER_COLLECTION   = "bots"
	BOT_SCHEDULER_DB_NAME   = "bot_scheduler"
	BOT_SCHEDULE_COLLECTION = "bot_schedules"

	// Default service ports
	BOT_RUNNER_PORT    = 50051
	BOT_SCHEDULER_PORT = 50053
)
