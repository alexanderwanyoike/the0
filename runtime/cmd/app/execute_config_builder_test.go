package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"runtime/internal/constants"
)

func TestExecuteConfigBuilder_Defaults(t *testing.T) {
	cfg := NewExecuteConfigBuilder().Build()

	assert.Equal(t, "test-bot", cfg.BotID)
	assert.Equal(t, "python3.11", cfg.Runtime)
	assert.Equal(t, "main.py", cfg.Entrypoint)
	assert.Equal(t, constants.DefaultCodePath, cfg.CodePath)
	assert.Equal(t, constants.DefaultStatePath, cfg.StatePath)
	assert.Equal(t, constants.DefaultLogsPath, cfg.LogsPath)
	assert.False(t, cfg.IsScheduled)
}

func TestExecuteConfigBuilder_WithAllFields(t *testing.T) {
	cfg := NewExecuteConfigBuilder().
		WithBotID("custom-bot").
		WithRuntime("nodejs20").
		WithEntrypoint("index.js").
		WithCodePath("/custom/code").
		WithStatePath("/custom/state").
		WithLogsPath("/custom/logs").
		WithCodeFile("bot/v1.0.0/code.zip").
		WithBotConfig(`{"key": "value"}`).
		WithBotType("scheduled").
		WithQueryEntrypoint("query.js").
		WithQueryPath("/api/query").
		WithQueryParams(`{"param": "value"}`).
		WithQueryResultKey("result").
		AsScheduled().
		Build()

	assert.Equal(t, "custom-bot", cfg.BotID)
	assert.Equal(t, "nodejs20", cfg.Runtime)
	assert.Equal(t, "index.js", cfg.Entrypoint)
	assert.Equal(t, "/custom/code", cfg.CodePath)
	assert.Equal(t, "/custom/state", cfg.StatePath)
	assert.Equal(t, "/custom/logs", cfg.LogsPath)
	assert.Equal(t, "bot/v1.0.0/code.zip", cfg.CodeFile)
	assert.Equal(t, `{"key": "value"}`, cfg.BotConfig)
	assert.Equal(t, "scheduled", cfg.BotType)
	assert.Equal(t, "query.js", cfg.QueryEntrypoint)
	assert.Equal(t, "/api/query", cfg.QueryPath)
	assert.Equal(t, `{"param": "value"}`, cfg.QueryParams)
	assert.Equal(t, "result", cfg.QueryResultKey)
	assert.True(t, cfg.IsScheduled)
}

func TestExecuteConfigBuilder_ChainedCalls(t *testing.T) {
	// Test that builder pattern allows chaining
	cfg := NewExecuteConfigBuilder().
		WithBotID("bot-1").
		WithRuntime("python3.11").
		WithEntrypoint("bot.py").
		Build()

	assert.Equal(t, "bot-1", cfg.BotID)
	assert.Equal(t, "python3.11", cfg.Runtime)
	assert.Equal(t, "bot.py", cfg.Entrypoint)
}

func TestExecuteConfigBuilder_PartialConfiguration(t *testing.T) {
	// Test building with only some fields set
	cfg := NewExecuteConfigBuilder().
		WithBotID("partial-bot").
		WithCodePath("/custom/path").
		Build()

	assert.Equal(t, "partial-bot", cfg.BotID)
	assert.Equal(t, "/custom/path", cfg.CodePath)
	// Defaults should still be set
	assert.Equal(t, "python3.11", cfg.Runtime)
	assert.Equal(t, "main.py", cfg.Entrypoint)
}
