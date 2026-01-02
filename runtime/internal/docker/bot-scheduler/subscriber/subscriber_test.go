package subscriber

import (
	"context"
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
	"runtime/internal/constants"
	"runtime/internal/util"
)

func TestDefaultOptions(t *testing.T) {
	os.Setenv("NATS_URL", "nats://localhost:4222")
	os.Setenv("MAX_RETRIES", "5")
	os.Setenv("MAX_BOT_SCHEDULE_PER_PARTITION", "20")

	opts := DefaultOptions()

	assert.Equal(t, "nats://localhost:4222", opts.NATSUrl)
	assert.Equal(t, constants.BOT_SCHEDULER_DB_NAME, opts.DBName)
	assert.Equal(t, constants.BOT_SCHEDULE_COLLECTION, opts.CollectionName)
	assert.Equal(t, 5, opts.MaxRetries)
	assert.Equal(t, 20, opts.MaxBotSchedulePerPartition)
}

func TestParseEventMessage(t *testing.T) {
	subscriber := &NATSSubscriber{
		logger: &util.DefaultLogger{},
	}

	tests := []struct {
		name        string
		data        []byte
		expectError bool
		expectedID  string
	}{
		{
			name: "Valid bot schedule event",
			data: []byte(`{
				"id": "550e8400-e29b-41d4-a716-446655440001",
				"config": {"test": "value"},
				"custom": {
					"config": {
						"name": "test-bot",
						"description": "Test bot",
						"runtime": "python3.11",
						"version": "1.0.0",
						"author": "test-author",
						"type": "scheduled",
						"entrypoints": {},
						"schema": {},
						"readme": "",
						"metadata": {}
					},
					"createdAt": "2023-01-01T00:00:00Z",
					"updatedAt": "2023-01-01T00:00:00Z",
					"filePath": "test.zip",
					"version": "1.0"
				}
			}`),
			expectError: false,
			expectedID:  "550e8400-e29b-41d4-a716-446655440001",
		},
		{
			name:        "Invalid JSON",
			data:        []byte(`{invalid json`),
			expectError: true,
		},
		{
			name: "Empty ID",
			data: []byte(`{
				"id": "",
				"config": {"test": "value"},
				"custom": {
					"config": {},
					"createdAt": "2023-01-01T00:00:00Z",
					"updatedAt": "2023-01-01T00:00:00Z",
					"filePath": "test.zip",
					"version": "1.0"
				}
			}`),
			expectError: false,
			expectedID:  "",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			message, err := subscriber.parseEventMessage(tt.data)

			if tt.expectError {
				assert.Error(t, err)
				assert.Nil(t, message)
			} else {
				assert.NoError(t, err)
				assert.NotNil(t, message)
				assert.Equal(t, tt.expectedID, message.ID)
			}
		})
	}
}

func TestConstants(t *testing.T) {
	assert.Equal(t, "the0.bot-schedule.created", SubjectBotScheduleCreated)
	assert.Equal(t, "the0.bot-schedule.updated", SubjectBotScheduleUpdated)
	assert.Equal(t, "the0.bot-schedule.deleted", SubjectBotScheduleDeleted)
}

func TestBotScheduleMessage(t *testing.T) {
	message := BotScheduleMessage{
		ID:     "test-id",
		Config: map[string]interface{}{"key": "value"},
		Custom: map[string]interface{}{"filePath": "test.zip"},
	}

	assert.Equal(t, "test-id", message.ID)
	assert.Equal(t, "value", message.Config["key"])
	assert.Equal(t, "test.zip", message.Custom["filePath"])
}

func TestGetDefaultMaxRetries(t *testing.T) {
	tests := []struct {
		name     string
		envValue string
		expected int
	}{
		{
			name:     "Default when not set",
			envValue: "",
			expected: 3,
		},
		{
			name:     "Valid environment value",
			envValue: "5",
			expected: 5,
		},
		{
			name:     "Invalid environment value",
			envValue: "invalid",
			expected: 3,
		},
		{
			name:     "Zero value",
			envValue: "0",
			expected: 0,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if tt.envValue == "" {
				os.Unsetenv("MAX_RETRIES")
			} else {
				os.Setenv("MAX_RETRIES", tt.envValue)
			}

			result := getDefaultMaxRetries()
			assert.Equal(t, tt.expected, result)
		})
	}

	// Clean up
	os.Unsetenv("MAX_RETRIES")
}

func TestGetDefaultMaxBotSchedulePerPartition(t *testing.T) {
	tests := []struct {
		name     string
		envValue string
		expected int
	}{
		{
			name:     "Default when not set",
			envValue: "",
			expected: 10,
		},
		{
			name:     "Valid environment value",
			envValue: "15",
			expected: 15,
		},
		{
			name:     "Invalid environment value",
			envValue: "invalid",
			expected: 10,
		},
		{
			name:     "Zero value",
			envValue: "0",
			expected: 0,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if tt.envValue == "" {
				os.Unsetenv("MAX_BOT_SCHEDULE_PER_PARTITION")
			} else {
				os.Setenv("MAX_BOT_SCHEDULE_PER_PARTITION", tt.envValue)
			}

			result := getDefaultMaxBotSchedulePerPartition()
			assert.Equal(t, tt.expected, result)
		})
	}

	// Clean up
	os.Unsetenv("MAX_BOT_SCHEDULE_PER_PARTITION")
}

func TestNewNATSSubscriber(t *testing.T) {
	t.Run("MissingNATSURL", func(t *testing.T) {
		// Save original value
		originalURL := os.Getenv("NATS_URL")
		defer func() {
			if originalURL != "" {
				os.Setenv("NATS_URL", originalURL)
			} else {
				os.Unsetenv("NATS_URL")
			}
		}()

		// Unset NATS_URL
		os.Unsetenv("NATS_URL")

		subscriber, err := NewNATSSubscriber(context.Background())

		assert.Error(t, err)
		assert.Nil(t, subscriber)
		assert.Contains(t, err.Error(), "NATS_URL environment variable is not set")
	})

	t.Run("InvalidNATSURL", func(t *testing.T) {
		// Save original value
		originalURL := os.Getenv("NATS_URL")
		defer func() {
			if originalURL != "" {
				os.Setenv("NATS_URL", originalURL)
			} else {
				os.Unsetenv("NATS_URL")
			}
		}()

		// Set invalid NATS URL
		os.Setenv("NATS_URL", "invalid://url")

		subscriber, err := NewNATSSubscriber(context.Background())

		assert.Error(t, err)
		assert.Nil(t, subscriber)
	})
}
