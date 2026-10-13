package docker

import (
	"context"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/mongo"
)

// Kubernetes mode runs a Bot unless its config.enabled is false, so Docker
// mode, reading the same stored documents, must run exactly the same ones.

func TestReconcile_RunsEveryBotItsConfigDoesNotDisable(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	client, container, uri := setupMongoTestContainer(t)
	defer container.Terminate(context.Background())
	defer client.Disconnect(context.Background())

	db := client.Database("which_bots_run")
	insertDocuments(t, db.Collection("bots"),
		storedBot("no-config", nil),
		storedBot("config-without-enabled", bson.M{"symbol": "BTC/USD"}),
		storedBot("enabled", bson.M{"enabled": true}),
		storedBot("disabled", bson.M{"enabled": false}),
		storedBot("disabled-as-text", bson.M{"enabled": "false"}),
		withTopLevelEnabled(storedBot("top-level-disabled", bson.M{}), false),
	)

	service, err := NewBotService(BotServiceConfig{MongoURI: uri, DBName: db.Name(), Collection: "bots"})
	require.NoError(t, err)
	defer service.Stop()
	require.NoError(t, service.initMongoDB(context.Background()))
	runner := NewMockDockerRunner()
	service.runner = runner

	service.reconcile()
	service.wg.Wait()

	assert.ElementsMatch(t,
		[]string{"no-config", "config-without-enabled", "enabled", "disabled-as-text", "top-level-disabled"},
		startedIDs(runner))
}

func TestScheduleCheck_RunsEveryDueScheduleItsConfigDoesNotDisable(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	client, container, uri := setupMongoTestContainer(t)
	defer container.Terminate(context.Background())
	defer client.Disconnect(context.Background())

	db := client.Database("which_schedules_run")
	past := time.Now().Add(-time.Minute)
	future := time.Now().Add(time.Hour)
	insertDocuments(t, db.Collection("bot_schedules"),
		storedSchedule("due", bson.M{"schedule": "0 9 * * *"}, past),
		storedSchedule("due-but-disabled", bson.M{"schedule": "0 9 * * *", "enabled": false}, past),
		storedSchedule("due-disabled-as-text", bson.M{"schedule": "0 9 * * *", "enabled": "false"}, past),
		withTopLevelEnabled(storedSchedule("due-top-level-disabled", bson.M{"schedule": "0 9 * * *"}, past), false),
		storedSchedule("not-due", bson.M{"schedule": "0 9 * * *"}, future),
	)

	service, err := NewScheduleService(ScheduleServiceConfig{
		MongoURI:   uri,
		NATSUrl:    "nats://unused:4222",
		DBName:     db.Name(),
		Collection: "bot_schedules",
	})
	require.NoError(t, err)
	defer service.Stop()
	require.NoError(t, service.initMongoDB(context.Background()))
	runner := NewMockDockerRunner()
	service.runner = runner

	service.checkAndExecuteSchedules()
	service.state.executionWg.Wait()

	assert.ElementsMatch(t,
		[]string{"due", "due-disabled-as-text", "due-top-level-disabled"},
		startedIDs(runner))
}

// storedBot is a Bot document as the subscriber stores it.
func storedBot(id string, config bson.M) bson.M {
	return bson.M{
		"id":     id,
		"config": config,
		"custom": bson.M{
			"version":  "1.0.0",
			"filePath": "acme/1.0.0/code.zip",
			"config": bson.M{
				"name":        "acme",
				"runtime":     "python3.11",
				"entrypoints": bson.M{"bot": "main.py"},
			},
		},
	}
}

func storedSchedule(id string, config bson.M, nextRun time.Time) bson.M {
	schedule := storedBot(id, config)
	schedule["next_execution_time"] = nextRun.Unix()
	return schedule
}

// withTopLevelEnabled sets the top-level field that nothing writes, to show
// it is not read either.
func withTopLevelEnabled(document bson.M, enabled bool) bson.M {
	document["enabled"] = enabled
	return document
}

func insertDocuments(t *testing.T, collection *mongo.Collection, documents ...bson.M) {
	t.Helper()
	for _, document := range documents {
		_, err := collection.InsertOne(context.Background(), document)
		require.NoError(t, err)
	}
}

func startedIDs(runner *MockDockerRunner) []string {
	runner.mu.Lock()
	defer runner.mu.Unlock()
	ids := make([]string, 0, len(runner.startCalls))
	for _, executable := range runner.startCalls {
		ids = append(ids, executable.ID)
	}
	return ids
}
