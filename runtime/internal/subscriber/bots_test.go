package subscriber

import (
	"encoding/json"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"runtime/internal/util"
)

func TestParseBotMessage_ReadsTheWrappedEventFormat(t *testing.T) {
	subscriber := &Subscriber{logger: &util.DefaultLogger{}}
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
	require.NoError(t, err)

	botMessage, err := subscriber.parseBotMessage(eventJSON)

	require.NoError(t, err)
	assert.Equal(t, "test-bot-123", botMessage.ID)
	assert.Equal(t, "custom/test-bot", botMessage.Config["type"])
	assert.Equal(t, "bucket/test-bot.zip", botMessage.CustomBotVersion.FilePath)
}

func TestParseBotMessage_RejectsInvalidJSON(t *testing.T) {
	subscriber := &Subscriber{logger: &util.DefaultLogger{}}

	botMessage, err := subscriber.parseBotMessage([]byte(`{"invalid": json}`))

	assert.Error(t, err)
	assert.Nil(t, botMessage)
	assert.Contains(t, err.Error(), "failed to unmarshal")
}
