package natssubscriber

import (
	"context"
	"encoding/json"
	"os"
	"testing"
	"time"

	"runtime/internal/constants"
	"runtime/internal/util"
	"github.com/stretchr/testify/assert"
)

func TestDefaultOptions(t *testing.T) {
	os.Setenv("NATS_URL", "nats://localhost:4222")
	os.Setenv("MAX_RETRIES", "5")
	os.Setenv("MAX_BOTS_PER_PARTITION", "20")

	opts := DefaultOptions()

	assert.Equal(t, "nats://localhost:4222", opts.NATSUrl)
	assert.Equal(t, constants.BOT_RUNNER_DB_NAME, opts.DBName)
	assert.Equal(t, constants.BOT_RUNNER_COLLECTION, opts.CollectionName)
	assert.Equal(t, 5, opts.MaxRetries)
	assert.Equal(t, 20, opts.MaxBotPerPartition)
}

func TestParseEventMessage(t *testing.T) {
	subscriber := &NATSSubscriber{
		logger: &util.DefaultLogger{},
	}

	// Create a sample event message
	event := BotEvent{
		Type:     "created",
		EntityID: "test-bot-123",
		Data: BotEventData{
			ID: "test-bot-123",
			Config: map[string]interface{}{
				"type":    "custom/test-bot",
				"version": "1.0.0",
			},
			Custom: CustomBotVersionData{
				FilePath:  "bucket/test-bot.zip",
				Version:   "1.0.0",
				CreatedAt: time.Now(),
				UpdatedAt: time.Now(),
			},
		},
		Timestamp: time.Now().Format(time.RFC3339),
	}

	eventJSON, err := json.Marshal(event)
	assert.NoError(t, err)

	botMessage, err := subscriber.parseEventMessage(eventJSON)
	assert.NoError(t, err)
	assert.NotNil(t, botMessage)
	assert.Equal(t, "test-bot-123", botMessage.ID)
	assert.Equal(t, "custom/test-bot", botMessage.Config["type"])
	assert.Equal(t, "bucket/test-bot.zip", botMessage.CustomBotVersion.FilePath)
}

func TestParseEventMessageInvalidJSON(t *testing.T) {
	subscriber := &NATSSubscriber{
		logger: &util.DefaultLogger{},
	}

	invalidJSON := []byte(`{"invalid": json}`)

	botMessage, err := subscriber.parseEventMessage(invalidJSON)
	assert.Error(t, err)
	assert.Nil(t, botMessage)
	assert.Contains(t, err.Error(), "failed to unmarshal")
}

func TestNewNATSSubscriber(t *testing.T) {
	t.Run("MissingNATSURL", func(t *testing.T) {
		os.Setenv("NATS_URL", "")
		os.Setenv("MONGO_URI", "mongodb://localhost:27017")

		_, err := NewNATSSubscriber(context.Background())
		assert.Error(t, err)
		assert.Contains(t, err.Error(), "NATS_URL")
	})

	t.Run("MissingMongoURI", func(t *testing.T) {
		os.Setenv("NATS_URL", "nats://localhost:4222")
		os.Setenv("MONGO_URI", "")

		_, err := NewNATSSubscriber(context.Background())
		assert.Error(t, err)
		assert.Contains(t, err.Error(), "MONGO_URI")
	})
}

func TestGetDefaultMaxRetries(t *testing.T) {
	// Test default value
	os.Setenv("MAX_RETRIES", "")
	assert.Equal(t, 3, getDefaultMaxRetries())

	// Test valid value
	os.Setenv("MAX_RETRIES", "5")
	assert.Equal(t, 5, getDefaultMaxRetries())

	// Test invalid value (should fall back to default)
	os.Setenv("MAX_RETRIES", "invalid")
	assert.Equal(t, 3, getDefaultMaxRetries())
}

func TestGetDefaultMaxBotsPerPartition(t *testing.T) {
	// Test default value
	os.Setenv("MAX_BOTS_PER_PARTITION", "")
	assert.Equal(t, 10, getDefaultMaxBotsPerPartition())

	// Test valid value
	os.Setenv("MAX_BOTS_PER_PARTITION", "15")
	assert.Equal(t, 15, getDefaultMaxBotsPerPartition())

	// Test invalid value (should fall back to default)
	os.Setenv("MAX_BOTS_PER_PARTITION", "invalid")
	assert.Equal(t, 10, getDefaultMaxBotsPerPartition())
}
