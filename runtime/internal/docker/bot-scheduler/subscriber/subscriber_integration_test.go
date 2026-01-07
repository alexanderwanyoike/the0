package subscriber

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

// TestSubscriber_BotScheduleCreated_PersistsToMongo verifies that a bot-schedule.created event
// results in a bot schedule being inserted into MongoDB with proper partition assignment
func TestSubscriber_BotScheduleCreated_PersistsToMongo(t *testing.T) {
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
	db := sharedInfra.GetDatabase("bot_schedule_created")
	collectionName := "bot_schedules"

	// Create subscriber with test database
	opts := SubscriberOptions{
		NATSUrl:                    sharedInfra.NATSURL,
		DBName:                     db.Name(),
		CollectionName:             collectionName,
		MaxRetries:                 3,
		MaxBotSchedulePerPartition: 10,
		Logger:                     &testLogger{t: t},
	}

	subscriber, err := NewNATSSubscriber(ctx, opts)
	require.NoError(t, err)
	defer subscriber.Stop()

	// Start subscriber
	err = subscriber.Start(ctx)
	require.NoError(t, err)

	// Create bot schedule message
	scheduleEvent := BotScheduleEvent{
		ID: "test-schedule-123",
		Config: map[string]interface{}{
			"symbol":   "ETH/USD",
			"interval": "daily",
			"schedule": "0 0 * * *",
		},
		Custom: CustomBotScheduleVersionData{
			Config: model.APIBotConfig{
				Runtime: "python3.11",
				Entrypoints: map[string]string{
					"bot": "main.py",
				},
			},
			FilePath:  "bots/test-schedule/v1.0.0.zip",
			Version:   "1.0.0",
			CreatedAt: time.Now(),
			UpdatedAt: time.Now(),
		},
	}

	// Publish bot-schedule.created event
	payload, err := json.Marshal(scheduleEvent)
	require.NoError(t, err)

	err = sharedInfra.natsConn.Publish(SubjectBotScheduleCreated, payload)
	require.NoError(t, err)

	// Wait for subscriber to process
	time.Sleep(200 * time.Millisecond)

	// Verify bot schedule exists in MongoDB
	collection := db.Collection(collectionName)
	var result model.BotSchedule
	err = collection.FindOne(ctx, bson.M{"id": "test-schedule-123"}).Decode(&result)
	require.NoError(t, err)

	assert.Equal(t, "test-schedule-123", result.ID)
	assert.Equal(t, "ETH/USD", result.Config["symbol"])
	assert.Equal(t, "daily", result.Config["interval"])
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

// TestSubscriber_BotScheduleCreated_Duplicate_SkipsCreation verifies that duplicate
// bot-schedule.created events don't create multiple schedule entries
func TestSubscriber_BotScheduleCreated_Duplicate_SkipsCreation(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}

	ctx := context.Background()
	os.Setenv("NATS_URL", sharedInfra.NATSURL)
	os.Setenv("MONGO_URI", sharedInfra.MongoURI)
	defer os.Unsetenv("NATS_URL")
	defer os.Unsetenv("MONGO_URI")

	db := sharedInfra.GetDatabase("bot_schedule_created_duplicate")
	collectionName := "bot_schedules"

	opts := SubscriberOptions{
		NATSUrl:                    sharedInfra.NATSURL,
		DBName:                     db.Name(),
		CollectionName:             collectionName,
		MaxRetries:                 3,
		MaxBotSchedulePerPartition: 10,
		Logger:                     &testLogger{t: t},
	}

	subscriber, err := NewNATSSubscriber(ctx, opts)
	require.NoError(t, err)
	defer subscriber.Stop()

	err = subscriber.Start(ctx)
	require.NoError(t, err)

	scheduleEvent := BotScheduleEvent{
		ID:     "duplicate-schedule",
		Config: map[string]interface{}{
			"test":     "value",
			"schedule": "0 0 * * *",
		},
		Custom: CustomBotScheduleVersionData{
			Config: model.APIBotConfig{
				Runtime: "python3.11",
				Entrypoints: map[string]string{
					"bot": "main.py",
				},
			},
			Version:   "1.0.0",
			CreatedAt: time.Now(),
			UpdatedAt: time.Now(),
		},
	}

	payload, _ := json.Marshal(scheduleEvent)

	// Publish twice
	sharedInfra.natsConn.Publish(SubjectBotScheduleCreated, payload)
	time.Sleep(100 * time.Millisecond)
	sharedInfra.natsConn.Publish(SubjectBotScheduleCreated, payload)

	time.Sleep(300 * time.Millisecond)

	// Verify only one schedule exists
	collection := db.Collection(collectionName)
	count, err := collection.CountDocuments(ctx, bson.M{"id": "duplicate-schedule"})
	require.NoError(t, err)
	assert.Equal(t, int64(1), count, "Should only have one schedule, not duplicates")
}

// TestSubscriber_BotScheduleUpdated_UpdatesMongo verifies that a bot-schedule.updated event
// updates the bot schedule configuration in MongoDB
func TestSubscriber_BotScheduleUpdated_UpdatesMongo(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}

	ctx := context.Background()
	os.Setenv("NATS_URL", sharedInfra.NATSURL)
	os.Setenv("MONGO_URI", sharedInfra.MongoURI)
	defer os.Unsetenv("NATS_URL")
	defer os.Unsetenv("MONGO_URI")

	db := sharedInfra.GetDatabase("bot_schedule_updated")
	collectionName := "bot_schedules"

	opts := SubscriberOptions{
		NATSUrl:                    sharedInfra.NATSURL,
		DBName:                     db.Name(),
		CollectionName:             collectionName,
		MaxRetries:                 3,
		MaxBotSchedulePerPartition: 10,
		Logger:                     &testLogger{t: t},
	}

	subscriber, err := NewNATSSubscriber(ctx, opts)
	require.NoError(t, err)
	defer subscriber.Stop()

	err = subscriber.Start(ctx)
	require.NoError(t, err)

	// Insert initial bot schedule
	initialSchedule := model.BotSchedule{
		ID:        "update-test-schedule",
		SegmentId: 1,
		Config: map[string]interface{}{
			"schedule": "0 0 * * *",
			"symbol":   "BTC/USD",
		},
		CustomBotVersion: model.CustomBotVersion{
			Version: "1.0.0",
		},
	}

	collection := db.Collection(collectionName)
	_, err = collection.InsertOne(ctx, initialSchedule)
	require.NoError(t, err)

	// Create partition
	partitions := db.Collection("partitions")
	_, err = partitions.InsertOne(ctx, bson.M{"_id": int32(1), "bot_count": 1})
	require.NoError(t, err)

	// Publish bot-schedule.updated event with new config
	updatedEvent := BotScheduleEvent{
		ID: "update-test-schedule",
		Config: map[string]interface{}{
			"schedule": "0 12 * * *", // Changed
			"symbol":   "ETH/USD",    // Changed
		},
		Custom: CustomBotScheduleVersionData{
			Version:   "2.0.0", // Changed
			CreatedAt: time.Now(),
			UpdatedAt: time.Now(),
		},
	}

	payload, _ := json.Marshal(updatedEvent)
	err = sharedInfra.natsConn.Publish(SubjectBotScheduleUpdated, payload)
	require.NoError(t, err)

	time.Sleep(200 * time.Millisecond)

	// Verify schedule was updated
	var result model.BotSchedule
	err = collection.FindOne(ctx, bson.M{"id": "update-test-schedule"}).Decode(&result)
	require.NoError(t, err)

	assert.Equal(t, "0 12 * * *", result.Config["schedule"])
	assert.Equal(t, "ETH/USD", result.Config["symbol"])
	assert.Equal(t, "2.0.0", result.CustomBotVersion.Version)
	assert.Equal(t, int32(1), result.SegmentId, "SegmentId should not change on update")
}

// TestSubscriber_BotScheduleUpdated_NotFound_CreatesSchedule verifies that updating a
// non-existent bot schedule creates it (robust upsert behavior)
func TestSubscriber_BotScheduleUpdated_NotFound_CreatesSchedule(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}

	ctx := context.Background()
	os.Setenv("NATS_URL", sharedInfra.NATSURL)
	os.Setenv("MONGO_URI", sharedInfra.MongoURI)
	defer os.Unsetenv("NATS_URL")
	defer os.Unsetenv("MONGO_URI")

	db := sharedInfra.GetDatabase("bot_schedule_updated_not_found")
	collectionName := "bot_schedules"

	opts := SubscriberOptions{
		NATSUrl:                    sharedInfra.NATSURL,
		DBName:                     db.Name(),
		CollectionName:             collectionName,
		MaxRetries:                 3,
		MaxBotSchedulePerPartition: 10,
		Logger:                     &testLogger{t: t},
	}

	subscriber, err := NewNATSSubscriber(ctx, opts)
	require.NoError(t, err)
	defer subscriber.Stop()

	err = subscriber.Start(ctx)
	require.NoError(t, err)

	// Publish bot-schedule.updated for non-existent schedule
	updatedEvent := BotScheduleEvent{
		ID:     "new-schedule-from-update",
		Config: map[string]interface{}{
			"created":  "from update",
			"schedule": "0 0 * * *",
		},
		Custom: CustomBotScheduleVersionData{
			Version:   "1.0.0",
			CreatedAt: time.Now(),
			UpdatedAt: time.Now(),
		},
	}

	payload, _ := json.Marshal(updatedEvent)
	err = sharedInfra.natsConn.Publish(SubjectBotScheduleUpdated, payload)
	require.NoError(t, err)

	time.Sleep(200 * time.Millisecond)

	// Verify schedule was created with partition assignment
	collection := db.Collection(collectionName)
	var result model.BotSchedule
	err = collection.FindOne(ctx, bson.M{"id": "new-schedule-from-update"}).Decode(&result)
	require.NoError(t, err)
	assert.Equal(t, "from update", result.Config["created"])
	assert.True(t, result.SegmentId > 0, "Should have partition assigned")
}

// TestSubscriber_BotScheduleDeleted_RemovesFromMongo verifies that a bot-schedule.deleted event
// removes the bot schedule from MongoDB and updates partition count
func TestSubscriber_BotScheduleDeleted_RemovesFromMongo(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}

	ctx := context.Background()
	os.Setenv("NATS_URL", sharedInfra.NATSURL)
	os.Setenv("MONGO_URI", sharedInfra.MongoURI)
	defer os.Unsetenv("NATS_URL")
	defer os.Unsetenv("MONGO_URI")

	db := sharedInfra.GetDatabase("bot_schedule_deleted")
	collectionName := "bot_schedules"

	opts := SubscriberOptions{
		NATSUrl:                    sharedInfra.NATSURL,
		DBName:                     db.Name(),
		CollectionName:             collectionName,
		MaxRetries:                 3,
		MaxBotSchedulePerPartition: 10,
		Logger:                     &testLogger{t: t},
	}

	subscriber, err := NewNATSSubscriber(ctx, opts)
	require.NoError(t, err)
	defer subscriber.Stop()

	err = subscriber.Start(ctx)
	require.NoError(t, err)

	// Insert schedule to delete
	scheduleToDelete := model.BotSchedule{
		ID:        "delete-me",
		SegmentId: 1,
		Config:    map[string]interface{}{"test": "value"},
		CustomBotVersion: model.CustomBotVersion{Version: "1.0.0"},
	}

	collection := db.Collection(collectionName)
	_, err = collection.InsertOne(ctx, scheduleToDelete)
	require.NoError(t, err)

	// Create partition with initial count
	partitions := db.Collection("partitions")
	_, err = partitions.InsertOne(ctx, bson.M{"_id": int32(1), "bot_count": 5})
	require.NoError(t, err)

	// Publish bot-schedule.deleted event
	deleteEvent := BotScheduleEvent{
		ID: "delete-me",
	}

	payload, _ := json.Marshal(deleteEvent)
	err = sharedInfra.natsConn.Publish(SubjectBotScheduleDeleted, payload)
	require.NoError(t, err)

	time.Sleep(200 * time.Millisecond)

	// Verify schedule was deleted
	count, err := collection.CountDocuments(ctx, bson.M{"id": "delete-me"})
	require.NoError(t, err)
	assert.Equal(t, int64(0), count, "Schedule should be deleted")

	// Verify partition count was decremented
	var partition struct {
		BotCount int32 `bson:"bot_count"`
	}
	err = partitions.FindOne(ctx, bson.M{"_id": int32(1)}).Decode(&partition)
	require.NoError(t, err)
	assert.Equal(t, int32(4), partition.BotCount, "Partition count should be decremented")
}

// TestSubscriber_BotScheduleDeleted_NotFound_NoError verifies that deleting a
// non-existent bot schedule doesn't cause an error
func TestSubscriber_BotScheduleDeleted_NotFound_NoError(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}

	ctx := context.Background()
	os.Setenv("NATS_URL", sharedInfra.NATSURL)
	os.Setenv("MONGO_URI", sharedInfra.MongoURI)
	defer os.Unsetenv("NATS_URL")
	defer os.Unsetenv("MONGO_URI")

	db := sharedInfra.GetDatabase("bot_schedule_deleted_not_found")
	collectionName := "bot_schedules"

	opts := SubscriberOptions{
		NATSUrl:                    sharedInfra.NATSURL,
		DBName:                     db.Name(),
		CollectionName:             collectionName,
		MaxRetries:                 3,
		MaxBotSchedulePerPartition: 10,
		Logger:                     &testLogger{t: t},
	}

	subscriber, err := NewNATSSubscriber(ctx, opts)
	require.NoError(t, err)
	defer subscriber.Stop()

	err = subscriber.Start(ctx)
	require.NoError(t, err)

	// Publish delete for non-existent schedule
	deleteEvent := BotScheduleEvent{ID: "does-not-exist"}
	payload, _ := json.Marshal(deleteEvent)

	err = sharedInfra.natsConn.Publish(SubjectBotScheduleDeleted, payload)
	require.NoError(t, err)

	time.Sleep(200 * time.Millisecond)

	// Should not crash or error - verify subscriber still running
	collection := db.Collection(collectionName)
	count, err := collection.CountDocuments(ctx, bson.M{})
	require.NoError(t, err)
	assert.Equal(t, int64(0), count)
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
	collectionName := "bot_schedules"

	opts := SubscriberOptions{
		NATSUrl:                    sharedInfra.NATSURL,
		DBName:                     db.Name(),
		CollectionName:             collectionName,
		MaxRetries:                 3,
		MaxBotSchedulePerPartition: 10,
		Logger:                     &testLogger{t: t},
	}

	subscriber, err := NewNATSSubscriber(ctx, opts)
	require.NoError(t, err)
	defer subscriber.Stop()

	err = subscriber.Start(ctx)
	require.NoError(t, err)

	// Publish invalid JSON
	invalidPayload := []byte(`{invalid json}`)
	err = sharedInfra.natsConn.Publish(SubjectBotScheduleCreated, invalidPayload)
	require.NoError(t, err)

	time.Sleep(200 * time.Millisecond)

	// Subscriber should still be running - publish valid message
	validEvent := BotScheduleEvent{
		ID:     "after-invalid",
		Config: map[string]interface{}{
			"test":     "value",
			"schedule": "0 0 * * *",
		},
		Custom: CustomBotScheduleVersionData{
			Version:   "1.0.0",
			CreatedAt: time.Now(),
			UpdatedAt: time.Now(),
		},
	}
	validPayload, _ := json.Marshal(validEvent)
	sharedInfra.natsConn.Publish(SubjectBotScheduleCreated, validPayload)

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
	collectionName := "bot_schedules"

	opts := SubscriberOptions{
		NATSUrl:                    sharedInfra.NATSURL,
		DBName:                     db.Name(),
		CollectionName:             collectionName,
		MaxRetries:                 3,
		MaxBotSchedulePerPartition: 10,
		Logger:                     &testLogger{t: t},
	}

	subscriber, err := NewNATSSubscriber(ctx, opts)
	require.NoError(t, err)

	err = subscriber.Start(ctx)
	require.NoError(t, err)

	// Publish some events
	for i := 0; i < 5; i++ {
		event := BotScheduleEvent{
			ID:     fmt.Sprintf("shutdown-schedule-%d", i),
			Config: map[string]interface{}{
				"index":    i,
				"schedule": "0 0 * * *",
			},
			Custom: CustomBotScheduleVersionData{
				Version:   "1.0.0",
				CreatedAt: time.Now(),
				UpdatedAt: time.Now(),
			},
		}
		payload, _ := json.Marshal(event)
		sharedInfra.natsConn.Publish(SubjectBotScheduleCreated, payload)
	}

	// Wait longer to ensure all messages are processed
	time.Sleep(500 * time.Millisecond)

	// Graceful shutdown
	err = subscriber.Stop()
	assert.NoError(t, err)

	// Verify subscriber is stopped - publishing should not be processed
	laterEvent := BotScheduleEvent{ID: "after-shutdown"}
	laterPayload, _ := json.Marshal(laterEvent)
	sharedInfra.natsConn.Publish(SubjectBotScheduleCreated, laterPayload)

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
