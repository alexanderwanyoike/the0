package main

import "runtime/internal/constants"

// ExecuteConfigBuilder provides a fluent interface for building ExecuteConfig in tests.
type ExecuteConfigBuilder struct {
	config *ExecuteConfig
}

// NewExecuteConfigBuilder creates a new builder with sensible defaults for testing.
func NewExecuteConfigBuilder() *ExecuteConfigBuilder {
	return &ExecuteConfigBuilder{
		config: &ExecuteConfig{
			BotID:      "test-bot",
			Runtime:    "python3.11",
			Entrypoint: "main.py",
			CodePath:   constants.DefaultCodePath,
			StatePath:  constants.DefaultStatePath,
			LogsPath:   constants.DefaultLogsPath,
		},
	}
}

// WithBotID sets the bot ID.
func (b *ExecuteConfigBuilder) WithBotID(id string) *ExecuteConfigBuilder {
	b.config.BotID = id
	return b
}

// WithRuntime sets the runtime.
func (b *ExecuteConfigBuilder) WithRuntime(runtime string) *ExecuteConfigBuilder {
	b.config.Runtime = runtime
	return b
}

// WithEntrypoint sets the entrypoint.
func (b *ExecuteConfigBuilder) WithEntrypoint(entrypoint string) *ExecuteConfigBuilder {
	b.config.Entrypoint = entrypoint
	return b
}

// WithCodePath sets the code path.
func (b *ExecuteConfigBuilder) WithCodePath(path string) *ExecuteConfigBuilder {
	b.config.CodePath = path
	return b
}

// WithStatePath sets the state path.
func (b *ExecuteConfigBuilder) WithStatePath(path string) *ExecuteConfigBuilder {
	b.config.StatePath = path
	return b
}

// WithLogsPath sets the logs path.
func (b *ExecuteConfigBuilder) WithLogsPath(path string) *ExecuteConfigBuilder {
	b.config.LogsPath = path
	return b
}

// WithCodeFile sets the code file.
func (b *ExecuteConfigBuilder) WithCodeFile(file string) *ExecuteConfigBuilder {
	b.config.CodeFile = file
	return b
}

// WithBotConfig sets the bot config JSON.
func (b *ExecuteConfigBuilder) WithBotConfig(config string) *ExecuteConfigBuilder {
	b.config.BotConfig = config
	return b
}

// WithBotType sets the bot type.
func (b *ExecuteConfigBuilder) WithBotType(botType string) *ExecuteConfigBuilder {
	b.config.BotType = botType
	return b
}

// WithQueryEntrypoint sets the query entrypoint.
func (b *ExecuteConfigBuilder) WithQueryEntrypoint(entrypoint string) *ExecuteConfigBuilder {
	b.config.QueryEntrypoint = entrypoint
	return b
}

// WithQueryPath sets the query path.
func (b *ExecuteConfigBuilder) WithQueryPath(path string) *ExecuteConfigBuilder {
	b.config.QueryPath = path
	return b
}

// WithQueryParams sets the query parameters JSON.
func (b *ExecuteConfigBuilder) WithQueryParams(params string) *ExecuteConfigBuilder {
	b.config.QueryParams = params
	return b
}

// WithQueryResultKey sets the query result key.
func (b *ExecuteConfigBuilder) WithQueryResultKey(key string) *ExecuteConfigBuilder {
	b.config.QueryResultKey = key
	return b
}

// AsScheduled marks the config as scheduled.
func (b *ExecuteConfigBuilder) AsScheduled() *ExecuteConfigBuilder {
	b.config.IsScheduled = true
	return b
}

// Build returns the configured ExecuteConfig.
func (b *ExecuteConfigBuilder) Build() *ExecuteConfig {
	return b.config
}
