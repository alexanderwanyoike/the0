package docker

import (
	"testing"

	"runtime/internal/model"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestNewBotService_RequiresMongoURI(t *testing.T) {
	_, err := NewBotService(BotServiceConfig{
		MongoURI: "",
	})
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "MongoURI is required")
}

func TestNewBotService_ValidConfig(t *testing.T) {
	service, err := NewBotService(BotServiceConfig{
		MongoURI: "mongodb://localhost:27017",
	})
	require.NoError(t, err)
	require.NotNil(t, service)
	defer service.Stop()

	assert.NotNil(t, service.state)
	assert.NotNil(t, service.logger)
	assert.NotNil(t, service.ctx)
	assert.NotNil(t, service.cancel)
}

func TestNewBotService_DefaultsDBName(t *testing.T) {
	service, err := NewBotService(BotServiceConfig{
		MongoURI: "mongodb://localhost:27017",
	})
	require.NoError(t, err)
	defer service.Stop()

	assert.Equal(t, "bot_runner", service.config.DBName)
	assert.Equal(t, "bots", service.config.Collection)
}

func TestNewBotService_CustomDBName(t *testing.T) {
	service, err := NewBotService(BotServiceConfig{
		MongoURI:   "mongodb://localhost:27017",
		DBName:     "custom_db",
		Collection: "custom_collection",
	})
	require.NoError(t, err)
	defer service.Stop()

	assert.Equal(t, "custom_db", service.config.DBName)
	assert.Equal(t, "custom_collection", service.config.Collection)
}

func TestBotService_ToExecutable(t *testing.T) {
	service, err := NewBotService(BotServiceConfig{
		MongoURI: "mongodb://localhost:27017",
	})
	require.NoError(t, err)
	defer service.Stop()

	bot := model.Bot{
		ID: "test-bot-123",
		Config: map[string]interface{}{
			"symbol": "BTC/USD",
		},
		CustomBotVersion: model.CustomBotVersion{
			FilePath: "bots/my-bot.zip",
			Config: model.APIBotConfig{
				Runtime: "python3.11",
				Entrypoints: map[string]string{
					"bot": "main.py",
				},
			},
		},
	}

	executable := service.toExecutable(bot)

	assert.Equal(t, "test-bot-123", executable.ID)
	assert.Equal(t, "python3.11", executable.Runtime)
	assert.Equal(t, "bot", executable.Entrypoint)
	assert.Equal(t, "bots/my-bot.zip", executable.FilePath)
	assert.True(t, executable.IsLongRunning)
	assert.False(t, executable.PersistResults)
	assert.Equal(t, int32(-1), executable.Segment)
	assert.Equal(t, "BTC/USD", executable.Config["symbol"])
}

func TestBotService_HashMap(t *testing.T) {
	service, err := NewBotService(BotServiceConfig{
		MongoURI: "mongodb://localhost:27017",
	})
	require.NoError(t, err)
	defer service.Stop()

	tests := []struct {
		name     string
		map1     map[string]interface{}
		map2     map[string]interface{}
		expected bool // true if hashes should be equal
	}{
		{
			name:     "identical maps",
			map1:     map[string]interface{}{"key": "value"},
			map2:     map[string]interface{}{"key": "value"},
			expected: true,
		},
		{
			name:     "different values",
			map1:     map[string]interface{}{"key": "value1"},
			map2:     map[string]interface{}{"key": "value2"},
			expected: false,
		},
		{
			name:     "different keys",
			map1:     map[string]interface{}{"key1": "value"},
			map2:     map[string]interface{}{"key2": "value"},
			expected: false,
		},
		{
			name:     "nil maps",
			map1:     nil,
			map2:     nil,
			expected: true,
		},
		{
			name:     "empty maps",
			map1:     map[string]interface{}{},
			map2:     map[string]interface{}{},
			expected: true,
		},
		{
			name:     "complex nested maps",
			map1:     map[string]interface{}{"nested": map[string]interface{}{"a": 1}},
			map2:     map[string]interface{}{"nested": map[string]interface{}{"a": 1}},
			expected: true,
		},
		{
			name:     "different nested values",
			map1:     map[string]interface{}{"nested": map[string]interface{}{"a": 1}},
			map2:     map[string]interface{}{"nested": map[string]interface{}{"a": 2}},
			expected: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			hash1 := service.hashMap(tt.map1)
			hash2 := service.hashMap(tt.map2)

			if tt.expected {
				assert.Equal(t, hash1, hash2)
			} else {
				assert.NotEqual(t, hash1, hash2)
			}
		})
	}
}

func TestBotService_HasConfigChanged(t *testing.T) {
	service, err := NewBotService(BotServiceConfig{
		MongoURI: "mongodb://localhost:27017",
	})
	require.NoError(t, err)
	defer service.Stop()

	tests := []struct {
		name     string
		oldBot   model.Bot
		newBot   model.Bot
		expected bool
	}{
		{
			name: "no change",
			oldBot: model.Bot{
				Config: map[string]interface{}{"symbol": "BTC/USD"},
			},
			newBot: model.Bot{
				Config: map[string]interface{}{"symbol": "BTC/USD"},
			},
			expected: false,
		},
		{
			name: "value changed",
			oldBot: model.Bot{
				Config: map[string]interface{}{"symbol": "BTC/USD"},
			},
			newBot: model.Bot{
				Config: map[string]interface{}{"symbol": "ETH/USD"},
			},
			expected: true,
		},
		{
			name: "key added",
			oldBot: model.Bot{
				Config: map[string]interface{}{"symbol": "BTC/USD"},
			},
			newBot: model.Bot{
				Config: map[string]interface{}{"symbol": "BTC/USD", "interval": 60},
			},
			expected: true,
		},
		{
			name: "key removed",
			oldBot: model.Bot{
				Config: map[string]interface{}{"symbol": "BTC/USD", "interval": 60},
			},
			newBot: model.Bot{
				Config: map[string]interface{}{"symbol": "BTC/USD"},
			},
			expected: true,
		},
		{
			name: "nil to empty",
			oldBot: model.Bot{
				Config: nil,
			},
			newBot: model.Bot{
				Config: map[string]interface{}{},
			},
			expected: true,
		},
		{
			name: "both nil",
			oldBot: model.Bot{
				Config: nil,
			},
			newBot: model.Bot{
				Config: nil,
			},
			expected: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := service.hasConfigChanged(tt.oldBot, tt.newBot)
			assert.Equal(t, tt.expected, result)
		})
	}
}

func TestBotService_GetStatus_EmptyState(t *testing.T) {
	service, err := NewBotService(BotServiceConfig{
		MongoURI: "mongodb://localhost:27017",
	})
	require.NoError(t, err)
	defer service.Stop()

	status := service.GetStatus()

	assert.Equal(t, 0, status["active_containers"])
	assert.Equal(t, int64(0), status["reconcile_count"])
	assert.Equal(t, 0, status["failed_restarts"])
	assert.Equal(t, false, status["nats_connected"])
}

func TestBotService_Stop(t *testing.T) {
	service, err := NewBotService(BotServiceConfig{
		MongoURI: "mongodb://localhost:27017",
	})
	require.NoError(t, err)

	// Should not panic
	service.Stop()

	// Context should be cancelled
	select {
	case <-service.ctx.Done():
		// Expected
	default:
		t.Error("Context should be cancelled after Stop()")
	}
}

func TestDefaultBotServiceConfig(t *testing.T) {
	// Set environment variables for test
	t.Setenv("MONGO_URI", "mongodb://test:27017")
	t.Setenv("NATS_URL", "nats://test:4222")

	config := DefaultBotServiceConfig()

	assert.Equal(t, "mongodb://test:27017", config.MongoURI)
	assert.Equal(t, "nats://test:4222", config.NATSUrl)
	assert.NotNil(t, config.Logger)
}
