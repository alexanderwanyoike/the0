package desired_test

import (
	"context"
	"os"
	"sync"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/mongo"

	"runtime/internal/desired"
	"runtime/internal/model"
	"runtime/internal/mongotest"
)

var (
	mongoOnce   sync.Once
	mongoServer *mongotest.Server
	mongoErr    error
)

func TestMain(m *testing.M) {
	code := m.Run()
	if mongoServer != nil {
		_ = mongoServer.Terminate(context.Background())
	}
	os.Exit(code)
}

// repository is a MongoRepository over collections no other test uses.
func repository(t *testing.T) (*desired.MongoRepository, *mongo.Collection, *mongo.Collection) {
	t.Helper()
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	mongoOnce.Do(func() { mongoServer, mongoErr = mongotest.Start(context.Background()) })
	require.NoError(t, mongoErr)
	db := mongoServer.Database()
	bots, schedules := db.Collection("bots"), db.Collection("bot_schedules")
	return desired.NewMongoRepository(bots, schedules), bots, schedules
}

func version(name string) model.CustomBotVersion {
	return model.CustomBotVersion{
		Version:  "1.0.0",
		FilePath: name + "/1.0.0/code.zip",
		Config: model.APIBotConfig{
			Name:        name,
			Runtime:     "python3.11",
			Type:        "scheduled",
			Entrypoints: map[string]string{"bot": "main.py", "query": "query.py"},
		},
	}
}

func TestMongoRepository_Bots_AreEveryStoredBot(t *testing.T) {
	repo, bots, _ := repository(t)
	stored := []model.Bot{
		{ID: "enabled", Config: map[string]interface{}{"symbol": "BTC/USD"}, CustomBotVersion: version("acme")},
		{ID: "disabled", Config: map[string]interface{}{"enabled": false}, CustomBotVersion: version("acme")},
	}
	for _, bot := range stored {
		_, err := bots.InsertOne(context.Background(), bot)
		require.NoError(t, err)
	}

	got, err := repo.Bots(context.Background())

	require.NoError(t, err)
	assert.ElementsMatch(t, stored, got)
}

func TestMongoRepository_Schedules_AreEveryStoredSchedule(t *testing.T) {
	repo, _, schedules := repository(t)
	nextRun := time.Date(2026, 10, 14, 9, 0, 0, 0, time.UTC).Unix()
	created := model.BotSchedule{
		ID:                "created",
		NextExecutionTime: nextRun,
		Config:            map[string]interface{}{"schedule": "0 9 * * *", "enabled": false},
		CustomBotVersion:  version("acme"),
	}
	_, err := schedules.InsertOne(context.Background(), created)
	require.NoError(t, err)
	// An update event stores the version as a map, not as the struct a
	// create stores.
	_, err = schedules.InsertOne(context.Background(), bson.M{
		"id":                  "updated",
		"next_execution_time": nextRun,
		"config":              bson.M{"schedule": "0 10 * * *"},
		"custom": bson.M{
			"version":   "1.0.0",
			"filePath":  "acme/1.0.0/code.zip",
			"createdAt": "",
			"updatedAt": "",
			"config":    version("acme").Config,
		},
	})
	require.NoError(t, err)

	got, err := repo.Schedules(context.Background())

	require.NoError(t, err)
	updated := model.BotSchedule{
		ID:                "updated",
		NextExecutionTime: nextRun,
		Config:            map[string]interface{}{"schedule": "0 10 * * *"},
		CustomBotVersion:  version("acme"),
	}
	assert.ElementsMatch(t, []model.BotSchedule{created, updated}, got)
}

func TestMongoRepository_SetNextExecutionTime_MovesOnlyThatSchedule(t *testing.T) {
	repo, _, schedules := repository(t)
	earlier := time.Date(2026, 10, 13, 9, 0, 0, 0, time.UTC)
	for _, id := range []string{"ran", "other"} {
		_, err := schedules.InsertOne(context.Background(), model.BotSchedule{
			ID:                id,
			NextExecutionTime: earlier.Unix(),
			Config:            map[string]interface{}{"schedule": "0 9 * * *"},
			CustomBotVersion:  version("acme"),
		})
		require.NoError(t, err)
	}
	next := earlier.Add(24 * time.Hour)

	require.NoError(t, repo.SetNextExecutionTime(context.Background(), "ran", next))

	got, err := repo.Schedules(context.Background())
	require.NoError(t, err)
	nextRuns := map[string]int64{}
	for _, schedule := range got {
		nextRuns[schedule.ID] = schedule.NextExecutionTime
	}
	assert.Equal(t, map[string]int64{"ran": next.Unix(), "other": earlier.Unix()}, nextRuns)
}

func TestMongoRepository_SetNextExecutionTime_DoesNotBringBackADeletedSchedule(t *testing.T) {
	repo, _, schedules := repository(t)

	require.NoError(t, repo.SetNextExecutionTime(context.Background(), "deleted-while-running", time.Now()))

	count, err := schedules.CountDocuments(context.Background(), bson.M{})
	require.NoError(t, err)
	assert.Zero(t, count)
}

func TestMongoRepository_DecidesThroughTheSharedRules(t *testing.T) {
	repo, bots, schedules := repository(t)
	_, err := bots.InsertOne(context.Background(), model.Bot{
		ID: "disabled", Config: map[string]interface{}{"enabled": false}, CustomBotVersion: version("acme"),
	})
	require.NoError(t, err)
	_, err = schedules.InsertOne(context.Background(), model.BotSchedule{
		ID:                "due",
		NextExecutionTime: time.Now().Add(-time.Minute).Unix(),
		Config:            map[string]interface{}{"schedule": "0 9 * * *"},
		CustomBotVersion:  version("acme"),
	})
	require.NoError(t, err)
	state := desired.New(repo)

	running, err := state.Bots(context.Background())
	require.NoError(t, err)
	due, err := state.DueSchedules(context.Background(), time.Now())
	require.NoError(t, err)

	assert.Empty(t, running)
	assert.Equal(t, []string{"due"}, scheduleIDs(due))
}
