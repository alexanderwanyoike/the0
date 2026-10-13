package subscriber

import (
	"context"
	"fmt"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/mongo"

	"runtime/internal/model"
)

func findSchedule(t *testing.T, db *mongo.Database, id string) model.BotSchedule {
	t.Helper()
	var schedule model.BotSchedule
	require.NoError(t, db.Collection(scheduleCollection).FindOne(context.Background(), bson.M{"id": id}).Decode(&schedule))
	return schedule
}

func TestSchedules_Created_IsStoredWithItsNextRun(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	scheduleEvents.ResetStream(t)
	db := mongoServer.Database()
	startScheduleSubscriber(t, db)

	created := testScheduleEvent("test-schedule-123", "0 0 * * *")
	created.Config = map[string]interface{}{"symbol": "ETH/USD", "interval": "daily", "schedule": "0 0 * * *"}
	created.Custom.FilePath = "bots/test-schedule/v1.0.0.zip"
	created.Custom.CreatedAt = time.Now()
	created.Custom.UpdatedAt = time.Now()
	scheduleEvents.PublishJSON(t, SubjectBotScheduleCreated, created)
	scheduleEvents.WaitForHandled(t)

	result := findSchedule(t, db, "test-schedule-123")
	assert.Equal(t, "ETH/USD", result.Config["symbol"])
	assert.Equal(t, "daily", result.Config["interval"])
	assert.Equal(t, "python3.11", result.CustomBotVersion.Config.Runtime)
	assert.Greater(t, result.NextExecutionTime, time.Now().Unix(), "a new schedule first runs at its cron's next tick")
}

func TestSchedules_CreatedTwice_IsStoredOnce(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	scheduleEvents.ResetStream(t)
	db := mongoServer.Database()
	startScheduleSubscriber(t, db)

	scheduleEvents.PublishJSON(t, SubjectBotScheduleCreated, testScheduleEvent("duplicate-schedule", "0 0 * * *"))
	scheduleEvents.PublishJSON(t, SubjectBotScheduleCreated, testScheduleEvent("duplicate-schedule", "0 0 * * *"))
	scheduleEvents.WaitForHandled(t)

	assert.Equal(t, int64(1), count(t, db.Collection(scheduleCollection), bson.M{"id": "duplicate-schedule"}),
		"Should only have one schedule, not duplicates")
}

func TestSchedules_Updated_ReplacesConfigAndVersion(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	scheduleEvents.ResetStream(t)
	db := mongoServer.Database()
	insertSchedule(t, db, "update-test-schedule")
	startScheduleSubscriber(t, db)

	updated := BotScheduleEvent{
		ID:     "update-test-schedule",
		Config: map[string]interface{}{"schedule": "0 12 * * *", "symbol": "ETH/USD"},
		Custom: CustomBotScheduleVersionData{Version: "2.0.0", CreatedAt: time.Now(), UpdatedAt: time.Now()},
	}
	scheduleEvents.PublishJSON(t, SubjectBotScheduleUpdated, updated)
	scheduleEvents.WaitForHandled(t)

	result := findSchedule(t, db, "update-test-schedule")
	assert.Equal(t, "0 12 * * *", result.Config["schedule"])
	assert.Equal(t, "ETH/USD", result.Config["symbol"])
	assert.Equal(t, "2.0.0", result.CustomBotVersion.Version)
}

func TestSchedules_EnabledAgain_NextRunsAtTheCronsNextTick(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	ctx := context.Background()
	scheduleEvents.ResetStream(t)
	db := mongoServer.Database()
	missedWhileDisabled := time.Now().Add(-48 * time.Hour).Unix()
	_, err := db.Collection(scheduleCollection).InsertOne(ctx, model.BotSchedule{
		ID:                "enabled-again",
		NextExecutionTime: missedWhileDisabled,
		Config:            map[string]interface{}{"symbol": "BTC/USD", "schedule": "0 9 * * *", "enabled": false},
		CustomBotVersion:  model.CustomBotVersion{Version: "1.0.0"},
	})
	require.NoError(t, err)
	startScheduleSubscriber(t, db)

	enabled := testScheduleEvent("enabled-again", "0 9 * * *")
	enabled.Config["enabled"] = true
	scheduleEvents.PublishJSON(t, SubjectBotScheduleUpdated, enabled)
	scheduleEvents.WaitForHandled(t)

	assert.Greater(t, findSchedule(t, db, "enabled-again").NextExecutionTime, time.Now().Unix(),
		"a schedule enabled again waits for its cron's next tick, as its new CronJob does in Kubernetes, "+
			"rather than running at once for a tick it missed while disabled")
}

func TestSchedules_UpdatedButNotStored_IsStored(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	scheduleEvents.ResetStream(t)
	db := mongoServer.Database()
	startScheduleSubscriber(t, db)

	updated := BotScheduleEvent{
		ID:     "new-schedule-from-update",
		Config: map[string]interface{}{"created": "from update", "schedule": "0 0 * * *"},
		Custom: CustomBotScheduleVersionData{Version: "1.0.0", CreatedAt: time.Now(), UpdatedAt: time.Now()},
	}
	scheduleEvents.PublishJSON(t, SubjectBotScheduleUpdated, updated)
	scheduleEvents.WaitForHandled(t)

	assert.Equal(t, "from update", findSchedule(t, db, "new-schedule-from-update").Config["created"])
}

func TestSchedules_Deleted_IsRemoved(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	scheduleEvents.ResetStream(t)
	db := mongoServer.Database()
	insertSchedule(t, db, "delete-me")
	startScheduleSubscriber(t, db)

	scheduleEvents.PublishJSON(t, SubjectBotScheduleDeleted, BotScheduleEvent{ID: "delete-me"})
	scheduleEvents.WaitForHandled(t)

	assert.Zero(t, count(t, db.Collection(scheduleCollection), bson.M{"id": "delete-me"}), "Schedule should be deleted")
}

func TestSchedules_DeletedButNotStored_IsAcknowledged(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	scheduleEvents.ResetStream(t)
	db := mongoServer.Database()
	startScheduleSubscriber(t, db)

	scheduleEvents.PublishJSON(t, SubjectBotScheduleDeleted, BotScheduleEvent{ID: "does-not-exist"})
	scheduleEvents.WaitForHandled(t)

	assert.Zero(t, count(t, db.Collection(scheduleCollection), bson.M{}))
}

func TestSchedules_InvalidJSON_IsDroppedAndLaterEventsHandled(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	scheduleEvents.ResetStream(t)
	db := mongoServer.Database()
	startScheduleSubscriber(t, db)

	scheduleEvents.Publish(t, SubjectBotScheduleCreated, []byte(`{invalid json}`))
	scheduleEvents.PublishJSON(t, SubjectBotScheduleCreated, testScheduleEvent("after-invalid", "0 0 * * *"))

	requireCount(t, db.Collection(scheduleCollection), bson.M{"id": "after-invalid"}, 1, "Subscriber should recover from invalid JSON")
}

func TestSchedules_Stopped_LeavesLaterEventsForTheNextStart(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	scheduleEvents.ResetStream(t)
	db := mongoServer.Database()
	subscriber := startScheduleSubscriber(t, db)
	for i := range 5 {
		scheduleEvents.PublishJSON(t, SubjectBotScheduleCreated, testScheduleEvent(fmt.Sprintf("shutdown-schedule-%d", i), "0 0 * * *"))
	}
	scheduleEvents.WaitForHandled(t)

	require.NoError(t, subscriber.Stop())
	scheduleEvents.PublishJSON(t, SubjectBotScheduleCreated, testScheduleEvent("after-shutdown", "0 0 * * *"))
	scheduleEvents.WaitForWaiting(t, 1)

	collection := db.Collection(scheduleCollection)
	assert.Zero(t, count(t, collection, bson.M{"id": "after-shutdown"}), "Message after shutdown should not be processed")
	assert.Equal(t, int64(5), count(t, collection, bson.M{"id": bson.M{"$regex": "^shutdown-schedule-"}}),
		"Messages before shutdown should be processed")
}
