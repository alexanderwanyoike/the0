package runtime

// MinIO bucket names for bot storage and logs.
const (
	// BucketCustomBots is the bucket for storing bot code archives.
	BucketCustomBots = "custom-bots"

	// BucketBotLogs is the bucket for storing bot execution logs.
	BucketBotLogs = "bot-logs"

	// BucketBacktests is the bucket for storing backtest results.
	BucketBacktests = "backtests"

	// BucketBotState is the bucket for storing persistent bot state.
	BucketBotState = "bot-state"
)
