package constants

// Database configurations for each service
const (
	// Separate database names to avoid conflicts
	BOT_RUNNER_DB_NAME      = "bot_runner"
	BOT_RUNNER_COLLECTION   = "bots"
	BACKTEST_RUNNER_DB_NAME = "backtest_runner"
	BACKTEST_COLLECTION     = "backtests"
	BOT_SCHEDULER_DB_NAME   = "bot_scheduler"
	BOT_SCHEDULE_COLLECTION = "bot_schedules"

	// Default service ports
	BOT_RUNNER_PORT      = 50051
	BACKTEST_RUNNER_PORT = 50052
	BOT_SCHEDULER_PORT   = 50053
)
