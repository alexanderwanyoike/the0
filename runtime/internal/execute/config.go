package execute

import "runtime/internal/constants"

// Config holds configuration for bot/query execution.
type Config struct {
	// Bot identification
	BotID string

	// Code location in MinIO
	CodeFile string

	// Runtime and entrypoint
	Runtime    string
	Entrypoint string

	// Bot configuration (JSON string)
	BotConfig string

	// Bot type: "realtime" or "scheduled"
	BotType string

	// Query settings
	QueryEntrypoint string // For realtime bots with query server
	QueryPath       string // For ephemeral query execution
	QueryParams     string // JSON query parameters
	QueryResultKey  string // MinIO key for storing query result (K8s mode)

	// Paths
	CodePath  string
	StatePath string
	LogsPath  string

	// Scheduled bot flag
	IsScheduled bool
}

// ConfigBuilder provides a fluent interface for building Config in tests.
type ConfigBuilder struct {
	config *Config
}

// NewConfigBuilder creates a new builder with sensible defaults for testing.
func NewConfigBuilder() *ConfigBuilder {
	return &ConfigBuilder{
		config: &Config{
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
func (b *ConfigBuilder) WithBotID(id string) *ConfigBuilder {
	b.config.BotID = id
	return b
}

// WithRuntime sets the runtime.
func (b *ConfigBuilder) WithRuntime(runtime string) *ConfigBuilder {
	b.config.Runtime = runtime
	return b
}

// WithEntrypoint sets the entrypoint.
func (b *ConfigBuilder) WithEntrypoint(entrypoint string) *ConfigBuilder {
	b.config.Entrypoint = entrypoint
	return b
}

// WithCodePath sets the code path.
func (b *ConfigBuilder) WithCodePath(path string) *ConfigBuilder {
	b.config.CodePath = path
	return b
}

// WithStatePath sets the state path.
func (b *ConfigBuilder) WithStatePath(path string) *ConfigBuilder {
	b.config.StatePath = path
	return b
}

// WithLogsPath sets the logs path.
func (b *ConfigBuilder) WithLogsPath(path string) *ConfigBuilder {
	b.config.LogsPath = path
	return b
}

// WithCodeFile sets the code file.
func (b *ConfigBuilder) WithCodeFile(file string) *ConfigBuilder {
	b.config.CodeFile = file
	return b
}

// WithBotConfig sets the bot config JSON.
func (b *ConfigBuilder) WithBotConfig(config string) *ConfigBuilder {
	b.config.BotConfig = config
	return b
}

// WithBotType sets the bot type.
func (b *ConfigBuilder) WithBotType(botType string) *ConfigBuilder {
	b.config.BotType = botType
	return b
}

// WithQueryEntrypoint sets the query entrypoint.
func (b *ConfigBuilder) WithQueryEntrypoint(entrypoint string) *ConfigBuilder {
	b.config.QueryEntrypoint = entrypoint
	return b
}

// WithQueryPath sets the query path.
func (b *ConfigBuilder) WithQueryPath(path string) *ConfigBuilder {
	b.config.QueryPath = path
	return b
}

// WithQueryParams sets the query parameters JSON.
func (b *ConfigBuilder) WithQueryParams(params string) *ConfigBuilder {
	b.config.QueryParams = params
	return b
}

// WithQueryResultKey sets the query result key.
func (b *ConfigBuilder) WithQueryResultKey(key string) *ConfigBuilder {
	b.config.QueryResultKey = key
	return b
}

// AsScheduled marks the config as scheduled.
func (b *ConfigBuilder) AsScheduled() *ConfigBuilder {
	b.config.IsScheduled = true
	return b
}

// Build returns the configured Config.
func (b *ConfigBuilder) Build() *Config {
	return b.config
}
