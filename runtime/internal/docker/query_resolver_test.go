package docker

import (
	"context"
	"runtime/internal/constants"
	"runtime/internal/model"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"github.com/testcontainers/testcontainers-go"
	"github.com/testcontainers/testcontainers-go/wait"
	"go.mongodb.org/mongo-driver/mongo"
	"go.mongodb.org/mongo-driver/mongo/options"
)

func TestNewMultiBotResolver(t *testing.T) {
	resolver := NewMultiBotResolver(nil, nil)

	assert.NotNil(t, resolver)
	assert.NotNil(t, resolver.scheduledIDs)
	assert.Equal(t, 0, len(resolver.scheduledIDs))
}

func TestScheduleToBot(t *testing.T) {
	resolver := NewMultiBotResolver(nil, nil)

	schedule := &model.BotSchedule{
		ID:     "test-schedule-1",
		Config: map[string]interface{}{"key": "value"},
		CustomBotVersion: model.CustomBotVersion{
			Version: "v1.0.0",
			Config: model.APIBotConfig{
				Runtime: "python3.11",
			},
		},
		NextExecutionTime: time.Now().Add(24 * time.Hour).Unix(),
	}

	bot := resolver.scheduleToBot(schedule)

	assert.Equal(t, "test-schedule-1", bot.ID)
	assert.Equal(t, map[string]interface{}{"key": "value"}, bot.Config)
	assert.Equal(t, "v1.0.0", bot.CustomBotVersion.Version)
}

func TestScheduleToBot_EmptyConfig(t *testing.T) {
	resolver := NewMultiBotResolver(nil, nil)

	schedule := &model.BotSchedule{
		ID:     "test-schedule-2",
		Config: nil,
		CustomBotVersion: model.CustomBotVersion{
			Version: "v2.0.0",
		},
	}

	bot := resolver.scheduleToBot(schedule)

	assert.Equal(t, "test-schedule-2", bot.ID)
	assert.Nil(t, bot.Config)
	assert.Equal(t, "v2.0.0", bot.CustomBotVersion.Version)
}

func TestGetContainerID_ScheduledBot_ReturnsEmpty(t *testing.T) {
	resolver := NewMultiBotResolver(nil, nil)

	// Mark bot as scheduled
	resolver.scheduledIDs["scheduled-bot-1"] = true

	containerID, found := resolver.GetContainerID(context.Background(), "scheduled-bot-1")

	assert.Empty(t, containerID)
	assert.False(t, found)
}

func TestGetContainerID_NilRunner_ReturnsEmpty(t *testing.T) {
	resolver := NewMultiBotResolver(nil, nil)

	containerID, found := resolver.GetContainerID(context.Background(), "realtime-bot-1")

	assert.Empty(t, containerID)
	assert.False(t, found)
}

// Integration tests with MongoDB testcontainer

func setupMongoTestContainer(t *testing.T) (*mongo.Client, testcontainers.Container, string) {
	ctx := context.Background()

	req := testcontainers.ContainerRequest{
		Image:        "mongo:7",
		ExposedPorts: []string{"27017/tcp"},
		WaitingFor:   wait.ForListeningPort("27017/tcp"),
	}

	mongoC, err := testcontainers.GenericContainer(ctx, testcontainers.GenericContainerRequest{
		ContainerRequest: req,
		Started:          true,
	})
	require.NoError(t, err)

	host, err := mongoC.Host(ctx)
	require.NoError(t, err)

	port, err := mongoC.MappedPort(ctx, "27017")
	require.NoError(t, err)

	mongoURI := "mongodb://" + host + ":" + port.Port()

	client, err := mongo.Connect(ctx, options.Client().ApplyURI(mongoURI))
	require.NoError(t, err)

	return client, mongoC, mongoURI
}

func TestGetBot_FromRealtimeCollection(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}

	client, container, _ := setupMongoTestContainer(t)
	defer container.Terminate(context.Background())
	defer client.Disconnect(context.Background())

	resolver := NewMultiBotResolver(client, nil)

	// Insert a bot into realtime collection
	enabled := true
	testBot := model.Bot{
		ID:     "realtime-bot-1",
		Config: map[string]interface{}{"strategy": "momentum"},
		CustomBotVersion: model.CustomBotVersion{
			Version: "v1.2.3",
			Config: model.APIBotConfig{
				Runtime: "python3.11",
			},
		},
		Enabled: &enabled,
	}

	collection := client.Database(constants.BOT_RUNNER_DB_NAME).Collection(constants.BOT_RUNNER_COLLECTION)
	_, err := collection.InsertOne(context.Background(), testBot)
	require.NoError(t, err)

	// Retrieve the bot
	bot, err := resolver.GetBot(context.Background(), "realtime-bot-1")

	require.NoError(t, err)
	assert.Equal(t, "realtime-bot-1", bot.ID)
	assert.Equal(t, "v1.2.3", bot.CustomBotVersion.Version)
	assert.True(t, bot.IsEnabled())

	// Verify it's marked as NOT scheduled
	assert.False(t, resolver.scheduledIDs["realtime-bot-1"])
}

func TestGetBot_FromScheduledCollection(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}

	client, container, _ := setupMongoTestContainer(t)
	defer container.Terminate(context.Background())
	defer client.Disconnect(context.Background())

	resolver := NewMultiBotResolver(client, nil)

	// Insert a schedule into scheduled collection
	testSchedule := model.BotSchedule{
		ID:     "scheduled-bot-1",
		Config: map[string]interface{}{"symbol": "BTC"},
		CustomBotVersion: model.CustomBotVersion{
			Version: "v2.0.0",
			Config: model.APIBotConfig{
				Runtime: "nodejs20",
			},
		},
		NextExecutionTime: time.Now().Add(24 * time.Hour).Unix(),
	}

	collection := client.Database(constants.BOT_SCHEDULER_DB_NAME).Collection(constants.BOT_SCHEDULE_COLLECTION)
	_, err := collection.InsertOne(context.Background(), testSchedule)
	require.NoError(t, err)

	// Retrieve the bot
	bot, err := resolver.GetBot(context.Background(), "scheduled-bot-1")

	require.NoError(t, err)
	assert.Equal(t, "scheduled-bot-1", bot.ID)
	assert.Equal(t, "v2.0.0", bot.CustomBotVersion.Version)

	// Verify it's marked as scheduled
	assert.True(t, resolver.scheduledIDs["scheduled-bot-1"])
}

func TestGetBot_NotFound(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}

	client, container, _ := setupMongoTestContainer(t)
	defer container.Terminate(context.Background())
	defer client.Disconnect(context.Background())

	resolver := NewMultiBotResolver(client, nil)

	// Try to get a non-existent bot
	bot, err := resolver.GetBot(context.Background(), "non-existent-bot")

	assert.Error(t, err)
	assert.Nil(t, bot)
	assert.Contains(t, err.Error(), "bot not found")
}

func TestGetBot_WithIDField(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}

	client, container, _ := setupMongoTestContainer(t)
	defer container.Terminate(context.Background())
	defer client.Disconnect(context.Background())

	resolver := NewMultiBotResolver(client, nil)

	// Insert bot with only "id" field (not "_id")
	collection := client.Database(constants.BOT_RUNNER_DB_NAME).Collection(constants.BOT_RUNNER_COLLECTION)
	_, err := collection.InsertOne(context.Background(), map[string]interface{}{
		"id":                 "bot-with-id-field",
		"config":             map[string]interface{}{"test": true},
		"custom_bot_version": "v1.0.0",
		"enabled":            true,
	})
	require.NoError(t, err)

	// Should be able to retrieve by id field
	bot, err := resolver.GetBot(context.Background(), "bot-with-id-field")

	require.NoError(t, err)
	assert.Equal(t, "bot-with-id-field", bot.ID)
}

func TestGetBot_PreferRealtimeOverScheduled(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}

	client, container, _ := setupMongoTestContainer(t)
	defer container.Terminate(context.Background())
	defer client.Disconnect(context.Background())

	resolver := NewMultiBotResolver(client, nil)

	// Insert bot in both collections with same ID
	botID := "duplicate-bot"

	enabled := true
	realtimeBot := model.Bot{
		ID:     botID,
		Config: map[string]interface{}{"source": "realtime"},
		CustomBotVersion: model.CustomBotVersion{
			Version: "v1.0.0",
			Config: model.APIBotConfig{
				Runtime: "python3.11",
			},
		},
		Enabled: &enabled,
	}

	scheduledBot := model.BotSchedule{
		ID:     botID,
		Config: map[string]interface{}{"source": "scheduled"},
		CustomBotVersion: model.CustomBotVersion{
			Version: "v2.0.0",
			Config: model.APIBotConfig{
				Runtime: "nodejs20",
			},
		},
		NextExecutionTime: time.Now().Unix(),
	}

	realtimeCollection := client.Database(constants.BOT_RUNNER_DB_NAME).Collection(constants.BOT_RUNNER_COLLECTION)
	_, err := realtimeCollection.InsertOne(context.Background(), realtimeBot)
	require.NoError(t, err)

	scheduledCollection := client.Database(constants.BOT_SCHEDULER_DB_NAME).Collection(constants.BOT_SCHEDULE_COLLECTION)
	_, err = scheduledCollection.InsertOne(context.Background(), scheduledBot)
	require.NoError(t, err)

	// Should retrieve realtime bot (checked first)
	bot, err := resolver.GetBot(context.Background(), botID)

	require.NoError(t, err)
	assert.Equal(t, "v1.0.0", bot.CustomBotVersion.Version) // Realtime version
	assert.Equal(t, "realtime", bot.Config["source"])

	// Should NOT be marked as scheduled
	assert.False(t, resolver.scheduledIDs[botID])
}
