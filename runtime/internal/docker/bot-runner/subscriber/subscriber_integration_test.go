package natssubscriber

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"runtime/internal/model"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"go.mongodb.org/mongo-driver/bson"
)

// TestSubscriber_BotCreated_PersistsToMongo verifies that a bot.created event
// results in a bot being inserted into MongoDB with proper partition assignment
func TestSubscriber_BotCreated_PersistsToMongo(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}

	ctx := context.Background()

	// Set up environment for subscriber
	os.Setenv("NATS_URL", sharedInfra.NATSURL)
	os.Setenv("MONGO_URI", sharedInfra.MongoURI)
	defer os.Unsetenv("NATS_URL")
	defer os.Unsetenv("MONGO_URI")

	// Get unique database for this test
	db := sharedInfra.GetDatabase("bot_created")
	collectionName := "bots"

	// Create subscriber with test database
	opts := SubscriberOptions{
		NATSUrl:            sharedInfra.NATSURL,
		DBName:             db.Name(),
		CollectionName:     collectionName,
		MaxRetries:         3,
		MaxBotPerPartition: 10,
		Logger:             &testLogger{t: t},
	}

	subscriber, err := NewNATSSubscriber(ctx, opts)
	require.NoError(t, err)
	defer subscriber.Stop()

	// Start subscriber
	err = subscriber.Start(ctx)
	require.NoError(t, err)

	// Create bot message
	botMessage := BotMessage{
		ID: "test-bot-123",
		Config: map[string]interface{}{
			"symbol": "BTC/USD",
			"amount": 100.0,
		},
		CustomBotVersion: model.CustomBotVersion{
			Config: model.APIBotConfig{
				Runtime: "python3.11",
				Entrypoints: map[string]string{
					"bot": "main.py",
				},
			},
			FilePath:  "bots/test-bot/v1.0.0.zip",
			Version:   "1.0.0",
			CreatedAt: time.Now().Format(time.RFC3339),
			UpdatedAt: time.Now().Format(time.RFC3339),
		},
	}

	// Publish bot.created event
	payload, err := json.Marshal(botMessage)
	require.NoError(t, err)

	err = sharedInfra.natsConn.Publish(SubjectBotCreated, payload)
	require.NoError(t, err)

	// Wait for subscriber to process
	time.Sleep(200 * time.Millisecond)

	// Verify bot exists in MongoDB
	collection := db.Collection(collectionName)
	var result model.Bot
	err = collection.FindOne(ctx, bson.M{"id": "test-bot-123"}).Decode(&result)
	require.NoError(t, err)

	assert.Equal(t, "test-bot-123", result.ID)
	assert.Equal(t, "BTC/USD", result.Config["symbol"])
	assert.Equal(t, 100.0, result.Config["amount"])
	assert.Equal(t, "python3.11", result.CustomBotVersion.Config.Runtime)
	assert.Equal(t, "main.py", result.CustomBotVersion.Config.Entrypoints["bot"])
	assert.True(t, result.SegmentId > 0, "SegmentId should be assigned")

	// Verify partition was created
	partitions := db.Collection("partitions")
	var partition struct {
		ID       int32 `bson:"_id"`
		BotCount int32 `bson:"bot_count"`
	}
	err = partitions.FindOne(ctx, bson.M{"_id": result.SegmentId}).Decode(&partition)
	require.NoError(t, err)
	assert.Equal(t, int32(1), partition.BotCount)
}

// TestSubscriber_BotCreated_Duplicate_SkipsCreation verifies that duplicate
// bot.created events don't create multiple bot entries
func TestSubscriber_BotCreated_Duplicate_SkipsCreation(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}

	ctx := context.Background()
	os.Setenv("NATS_URL", sharedInfra.NATSURL)
	os.Setenv("MONGO_URI", sharedInfra.MongoURI)
	defer os.Unsetenv("NATS_URL")
	defer os.Unsetenv("MONGO_URI")

	db := sharedInfra.GetDatabase("bot_created_duplicate")
	collectionName := "bots"

	opts := SubscriberOptions{
		NATSUrl:            sharedInfra.NATSURL,
		DBName:             db.Name(),
		CollectionName:     collectionName,
		MaxRetries:         3,
		MaxBotPerPartition: 10,
		Logger:             &testLogger{t: t},
	}

	subscriber, err := NewNATSSubscriber(ctx, opts)
	require.NoError(t, err)
	defer subscriber.Stop()

	err = subscriber.Start(ctx)
	require.NoError(t, err)

	botMessage := BotMessage{
		ID:     "duplicate-bot",
		Config: map[string]interface{}{"test": "value"},
		CustomBotVersion: model.CustomBotVersion{
			Config: model.APIBotConfig{
				Runtime: "python3.11",
				Entrypoints: map[string]string{
					"bot": "main.py",
				},
			},
			Version: "1.0.0",
		},
	}

	payload, _ := json.Marshal(botMessage)

	// Publish twice
	sharedInfra.natsConn.Publish(SubjectBotCreated, payload)
	time.Sleep(100 * time.Millisecond)
	sharedInfra.natsConn.Publish(SubjectBotCreated, payload)

	time.Sleep(300 * time.Millisecond)

	// Verify only one bot exists
	collection := db.Collection(collectionName)
	count, err := collection.CountDocuments(ctx, bson.M{"id": "duplicate-bot"})
	require.NoError(t, err)
	assert.Equal(t, int64(1), count, "Should only have one bot, not duplicates")
}

// TestSubscriber_BotUpdated_UpdatesMongo verifies that a bot.updated event
// updates the bot configuration in MongoDB
func TestSubscriber_BotUpdated_UpdatesMongo(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}

	ctx := context.Background()
	os.Setenv("NATS_URL", sharedInfra.NATSURL)
	os.Setenv("MONGO_URI", sharedInfra.MongoURI)
	defer os.Unsetenv("NATS_URL")
	defer os.Unsetenv("MONGO_URI")

	db := sharedInfra.GetDatabase("bot_updated")
	collectionName := "bots"

	opts := SubscriberOptions{
		NATSUrl:            sharedInfra.NATSURL,
		DBName:             db.Name(),
		CollectionName:     collectionName,
		MaxRetries:         3,
		MaxBotPerPartition: 10,
		Logger:             &testLogger{t: t},
	}

	subscriber, err := NewNATSSubscriber(ctx, opts)
	require.NoError(t, err)
	defer subscriber.Stop()

	err = subscriber.Start(ctx)
	require.NoError(t, err)

	// Insert initial bot
	initialBot := model.Bot{
		ID:        "update-test-bot",
		SegmentId: 1,
		Config: map[string]interface{}{
			"symbol": "ETH/USD",
			"amount": 50.0,
		},
		CustomBotVersion: model.CustomBotVersion{
			Config: model.APIBotConfig{
				Runtime: "python3.11",
				Entrypoints: map[string]string{
					"bot": "main.py",
				},
			},
			Version: "1.0.0",
		},
	}

	collection := db.Collection(collectionName)
	_, err = collection.InsertOne(ctx, initialBot)
	require.NoError(t, err)

	// Create partition
	partitions := db.Collection("partitions")
	_, err = partitions.InsertOne(ctx, bson.M{"_id": int32(1), "bot_count": 1})
	require.NoError(t, err)

	// Publish bot.updated event with new config
	updatedMessage := BotMessage{
		ID: "update-test-bot",
		Config: map[string]interface{}{
			"symbol": "BTC/USD", // Changed
			"amount": 200.0,     // Changed
		},
		CustomBotVersion: model.CustomBotVersion{
			Config: model.APIBotConfig{
				Runtime: "nodejs20", // Changed
				Entrypoints: map[string]string{
					"bot": "index.js", // Changed
				},
			},
			Version: "2.0.0", // Changed
		},
	}

	payload, _ := json.Marshal(updatedMessage)
	err = sharedInfra.natsConn.Publish(SubjectBotUpdated, payload)
	require.NoError(t, err)

	time.Sleep(200 * time.Millisecond)

	// Verify bot was updated
	var result model.Bot
	err = collection.FindOne(ctx, bson.M{"id": "update-test-bot"}).Decode(&result)
	require.NoError(t, err)

	assert.Equal(t, "BTC/USD", result.Config["symbol"])
	assert.Equal(t, 200.0, result.Config["amount"])
	assert.Equal(t, "nodejs20", result.CustomBotVersion.Config.Runtime)
	assert.Equal(t, "index.js", result.CustomBotVersion.Config.Entrypoints["bot"])
	assert.Equal(t, "2.0.0", result.CustomBotVersion.Version)
	assert.Equal(t, int32(1), result.SegmentId, "SegmentId should not change on update")
}

// TestSubscriber_BotUpdated_NotFound_CreatesBot verifies that updating a
// non-existent bot creates it (robust upsert behavior)
func TestSubscriber_BotUpdated_NotFound_CreatesBot(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}

	ctx := context.Background()
	os.Setenv("NATS_URL", sharedInfra.NATSURL)
	os.Setenv("MONGO_URI", sharedInfra.MongoURI)
	defer os.Unsetenv("NATS_URL")
	defer os.Unsetenv("MONGO_URI")

	db := sharedInfra.GetDatabase("bot_updated_not_found")
	collectionName := "bots"

	opts := SubscriberOptions{
		NATSUrl:            sharedInfra.NATSURL,
		DBName:             db.Name(),
		CollectionName:     collectionName,
		MaxRetries:         3,
		MaxBotPerPartition: 10,
		Logger:             &testLogger{t: t},
	}

	subscriber, err := NewNATSSubscriber(ctx, opts)
	require.NoError(t, err)
	defer subscriber.Stop()

	err = subscriber.Start(ctx)
	require.NoError(t, err)

	// Publish bot.updated for non-existent bot
	updatedMessage := BotMessage{
		ID:     "new-bot-from-update",
		Config: map[string]interface{}{"created": "from update"},
		CustomBotVersion: model.CustomBotVersion{
			Config: model.APIBotConfig{
				Runtime: "python3.11",
				Entrypoints: map[string]string{
					"bot": "main.py",
				},
			},
			Version: "1.0.0",
		},
	}

	payload, _ := json.Marshal(updatedMessage)
	err = sharedInfra.natsConn.Publish(SubjectBotUpdated, payload)
	require.NoError(t, err)

	time.Sleep(200 * time.Millisecond)

	// Verify bot was created with partition assignment
	collection := db.Collection(collectionName)
	var result model.Bot
	err = collection.FindOne(ctx, bson.M{"id": "new-bot-from-update"}).Decode(&result)
	require.NoError(t, err)
	assert.Equal(t, "from update", result.Config["created"])
	assert.True(t, result.SegmentId > 0, "Should have partition assigned")
}

// TestSubscriber_BotDeleted_RemovesFromMongo verifies that a bot.deleted event
// removes the bot from MongoDB and updates partition count
func TestSubscriber_BotDeleted_RemovesFromMongo(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}

	ctx := context.Background()
	os.Setenv("NATS_URL", sharedInfra.NATSURL)
	os.Setenv("MONGO_URI", sharedInfra.MongoURI)
	defer os.Unsetenv("NATS_URL")
	defer os.Unsetenv("MONGO_URI")

	db := sharedInfra.GetDatabase("bot_deleted")
	collectionName := "bots"

	opts := SubscriberOptions{
		NATSUrl:            sharedInfra.NATSURL,
		DBName:             db.Name(),
		CollectionName:     collectionName,
		MaxRetries:         3,
		MaxBotPerPartition: 10,
		Logger:             &testLogger{t: t},
	}

	subscriber, err := NewNATSSubscriber(ctx, opts)
	require.NoError(t, err)
	defer subscriber.Stop()

	err = subscriber.Start(ctx)
	require.NoError(t, err)

	// Insert bot to delete
	botToDelete := model.Bot{
		ID:        "delete-me",
		SegmentId: 1,
		Config:    map[string]interface{}{"test": "value"},
		CustomBotVersion: model.CustomBotVersion{
			Config: model.APIBotConfig{
				Runtime: "python3.11",
				Entrypoints: map[string]string{
					"bot": "main.py",
				},
			},
			Version: "1.0.0",
		},
	}

	collection := db.Collection(collectionName)
	_, err = collection.InsertOne(ctx, botToDelete)
	require.NoError(t, err)

	// Create partition with initial count
	partitions := db.Collection("partitions")
	_, err = partitions.InsertOne(ctx, bson.M{"_id": int32(1), "bot_count": 5})
	require.NoError(t, err)

	// Publish bot.deleted event
	deleteMessage := BotMessage{
		ID: "delete-me",
	}

	payload, _ := json.Marshal(deleteMessage)
	err = sharedInfra.natsConn.Publish(SubjectBotDeleted, payload)
	require.NoError(t, err)

	time.Sleep(200 * time.Millisecond)

	// Verify bot was deleted
	count, err := collection.CountDocuments(ctx, bson.M{"id": "delete-me"})
	require.NoError(t, err)
	assert.Equal(t, int64(0), count, "Bot should be deleted")

	// Verify partition count was decremented
	var partition struct {
		BotCount int32 `bson:"bot_count"`
	}
	err = partitions.FindOne(ctx, bson.M{"_id": int32(1)}).Decode(&partition)
	require.NoError(t, err)
	assert.Equal(t, int32(4), partition.BotCount, "Partition count should be decremented")
}

// TestSubscriber_BotDeleted_NotFound_NoError verifies that deleting a
// non-existent bot doesn't cause an error
func TestSubscriber_BotDeleted_NotFound_NoError(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}

	ctx := context.Background()
	os.Setenv("NATS_URL", sharedInfra.NATSURL)
	os.Setenv("MONGO_URI", sharedInfra.MongoURI)
	defer os.Unsetenv("NATS_URL")
	defer os.Unsetenv("MONGO_URI")

	db := sharedInfra.GetDatabase("bot_deleted_not_found")
	collectionName := "bots"

	opts := SubscriberOptions{
		NATSUrl:            sharedInfra.NATSURL,
		DBName:             db.Name(),
		CollectionName:     collectionName,
		MaxRetries:         3,
		MaxBotPerPartition: 10,
		Logger:             &testLogger{t: t},
	}

	subscriber, err := NewNATSSubscriber(ctx, opts)
	require.NoError(t, err)
	defer subscriber.Stop()

	err = subscriber.Start(ctx)
	require.NoError(t, err)

	// Publish delete for non-existent bot
	deleteMessage := BotMessage{ID: "does-not-exist"}
	payload, _ := json.Marshal(deleteMessage)

	err = sharedInfra.natsConn.Publish(SubjectBotDeleted, payload)
	require.NoError(t, err)

	time.Sleep(200 * time.Millisecond)

	// Should not crash or error - verify subscriber still running
	collection := db.Collection(collectionName)
	count, err := collection.CountDocuments(ctx, bson.M{})
	require.NoError(t, err)
	assert.Equal(t, int64(0), count)
}

// TestSubscriber_WrappedEventFormat verifies that the subscriber can parse
// both direct BotMessage format and wrapped BotEvent format
func TestSubscriber_WrappedEventFormat(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}

	ctx := context.Background()
	os.Setenv("NATS_URL", sharedInfra.NATSURL)
	os.Setenv("MONGO_URI", sharedInfra.MongoURI)
	defer os.Unsetenv("NATS_URL")
	defer os.Unsetenv("MONGO_URI")

	db := sharedInfra.GetDatabase("wrapped_event")
	collectionName := "bots"

	opts := SubscriberOptions{
		NATSUrl:            sharedInfra.NATSURL,
		DBName:             db.Name(),
		CollectionName:     collectionName,
		MaxRetries:         3,
		MaxBotPerPartition: 10,
		Logger:             &testLogger{t: t},
	}

	subscriber, err := NewNATSSubscriber(ctx, opts)
	require.NoError(t, err)
	defer subscriber.Stop()

	err = subscriber.Start(ctx)
	require.NoError(t, err)

	// Create wrapped BotEvent format (API format)
	wrappedEvent := BotEvent{
		Type:      "bot.created",
		EntityID:  "wrapped-bot",
		Timestamp: time.Now().Format(time.RFC3339),
		Data: BotEventData{
			ID:     "wrapped-bot",
			Config: map[string]interface{}{"wrapped": true},
			Custom: CustomBotVersionData{
				Config: model.APIBotConfig{
					Runtime: "python3.11",
					Entrypoints: map[string]string{
						"bot": "main.py",
					},
				},
				Version:   "1.0.0",
				FilePath:  "bots/wrapped/v1.0.0.zip",
				CreatedAt: time.Now(),
				UpdatedAt: time.Now(),
			},
		},
	}

	payload, _ := json.Marshal(wrappedEvent)
	err = sharedInfra.natsConn.Publish(SubjectBotCreated, payload)
	require.NoError(t, err)

	time.Sleep(200 * time.Millisecond)

	// Verify bot was created from wrapped format
	collection := db.Collection(collectionName)
	var result model.Bot
	err = collection.FindOne(ctx, bson.M{"id": "wrapped-bot"}).Decode(&result)
	require.NoError(t, err)
	assert.Equal(t, true, result.Config["wrapped"])
	assert.Equal(t, "python3.11", result.CustomBotVersion.Config.Runtime)
}

// TestSubscriber_InvalidJSON_LogsError verifies that invalid JSON doesn't
// crash the subscriber
func TestSubscriber_InvalidJSON_LogsError(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}

	ctx := context.Background()
	os.Setenv("NATS_URL", sharedInfra.NATSURL)
	os.Setenv("MONGO_URI", sharedInfra.MongoURI)
	defer os.Unsetenv("NATS_URL")
	defer os.Unsetenv("MONGO_URI")

	db := sharedInfra.GetDatabase("invalid_json")
	collectionName := "bots"

	opts := SubscriberOptions{
		NATSUrl:            sharedInfra.NATSURL,
		DBName:             db.Name(),
		CollectionName:     collectionName,
		MaxRetries:         3,
		MaxBotPerPartition: 10,
		Logger:             &testLogger{t: t},
	}

	subscriber, err := NewNATSSubscriber(ctx, opts)
	require.NoError(t, err)
	defer subscriber.Stop()

	err = subscriber.Start(ctx)
	require.NoError(t, err)

	// Publish invalid JSON
	invalidPayload := []byte(`{invalid json}`)
	err = sharedInfra.natsConn.Publish(SubjectBotCreated, invalidPayload)
	require.NoError(t, err)

	time.Sleep(200 * time.Millisecond)

	// Subscriber should still be running - publish valid message
	validMessage := BotMessage{
		ID:     "after-invalid",
		Config: map[string]interface{}{"test": "value"},
		CustomBotVersion: model.CustomBotVersion{
			Config: model.APIBotConfig{
				Runtime: "python3.11",
				Entrypoints: map[string]string{
					"bot": "main.py",
				},
			},
			Version: "1.0.0",
		},
	}
	validPayload, _ := json.Marshal(validMessage)
	sharedInfra.natsConn.Publish(SubjectBotCreated, validPayload)

	time.Sleep(200 * time.Millisecond)

	// Verify valid message was processed
	collection := db.Collection(collectionName)
	count, err := collection.CountDocuments(ctx, bson.M{"id": "after-invalid"})
	require.NoError(t, err)
	assert.Equal(t, int64(1), count, "Subscriber should recover from invalid JSON")
}

// TestSubscriber_GracefulShutdown verifies clean shutdown behavior
func TestSubscriber_GracefulShutdown(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}

	ctx := context.Background()
	os.Setenv("NATS_URL", sharedInfra.NATSURL)
	os.Setenv("MONGO_URI", sharedInfra.MongoURI)
	defer os.Unsetenv("NATS_URL")
	defer os.Unsetenv("MONGO_URI")

	db := sharedInfra.GetDatabase("graceful_shutdown")
	collectionName := "bots"

	opts := SubscriberOptions{
		NATSUrl:            sharedInfra.NATSURL,
		DBName:             db.Name(),
		CollectionName:     collectionName,
		MaxRetries:         3,
		MaxBotPerPartition: 10,
		Logger:             &testLogger{t: t},
	}

	subscriber, err := NewNATSSubscriber(ctx, opts)
	require.NoError(t, err)

	err = subscriber.Start(ctx)
	require.NoError(t, err)

	// Publish some events
	for i := 0; i < 5; i++ {
		msg := BotMessage{
			ID:     fmt.Sprintf("shutdown-bot-%d", i),
			Config: map[string]interface{}{"index": i},
			CustomBotVersion: model.CustomBotVersion{
				Config: model.APIBotConfig{
					Runtime: "python3.11",
					Entrypoints: map[string]string{
						"bot": "main.py",
					},
				},
				Version: "1.0.0",
			},
		}
		payload, _ := json.Marshal(msg)
		sharedInfra.natsConn.Publish(SubjectBotCreated, payload)
	}

	// Wait longer to ensure all messages are processed
	time.Sleep(500 * time.Millisecond)

	// Graceful shutdown
	err = subscriber.Stop()
	assert.NoError(t, err)

	// Verify subscriber is stopped - publishing should not be processed
	laterMessage := BotMessage{ID: "after-shutdown"}
	laterPayload, _ := json.Marshal(laterMessage)
	sharedInfra.natsConn.Publish(SubjectBotCreated, laterPayload)

	time.Sleep(200 * time.Millisecond)

	// "after-shutdown" should not exist because subscriber was stopped
	collection := db.Collection(collectionName)
	count, err := collection.CountDocuments(ctx, bson.M{"id": "after-shutdown"})
	require.NoError(t, err)
	assert.Equal(t, int64(0), count, "Message after shutdown should not be processed")

	// But the 5 messages before shutdown should exist
	totalCount, err := collection.CountDocuments(ctx, bson.M{})
	require.NoError(t, err)
	assert.Equal(t, int64(5), totalCount, "Messages before shutdown should be processed")
}

// testLogger is a simple logger that logs to testing.T
type testLogger struct {
	t *testing.T
}

func (l *testLogger) Info(msg string, keysAndValues ...interface{}) {
	l.t.Logf("[INFO] %s %v", msg, keysAndValues)
}

func (l *testLogger) Error(msg string, keysAndValues ...interface{}) {
	l.t.Logf("[ERROR] %s %v", msg, keysAndValues)
}

func (l *testLogger) Debug(msg string, keysAndValues ...interface{}) {
	l.t.Logf("[DEBUG] %s %v", msg, keysAndValues)
}

func (l *testLogger) Warn(msg string, keysAndValues ...interface{}) {
	l.t.Logf("[WARN] %s %v", msg, keysAndValues)
}
