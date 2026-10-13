package subscriber

import (
	"context"
	"fmt"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"go.mongodb.org/mongo-driver/bson"

	"runtime/internal/model"
)

func TestBots_Created_IsStored(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	botEvents.ResetStream(t)
	db := mongoServer.Database()
	startBotSubscriber(t, db)

	created := testBotMessage("test-bot-123")
	created.Config = map[string]interface{}{"symbol": "BTC/USD", "amount": 100.0}
	created.CustomBotVersion.FilePath = "bots/test-bot/v1.0.0.zip"
	created.CustomBotVersion.CreatedAt = time.Now().Format(time.RFC3339)
	created.CustomBotVersion.UpdatedAt = time.Now().Format(time.RFC3339)
	botEvents.PublishJSON(t, SubjectBotCreated, created)
	botEvents.WaitForHandled(t)

	var result model.Bot
	require.NoError(t, db.Collection(botCollection).FindOne(context.Background(), bson.M{"id": "test-bot-123"}).Decode(&result))
	assert.Equal(t, "test-bot-123", result.ID)
	assert.Equal(t, "BTC/USD", result.Config["symbol"])
	assert.Equal(t, 100.0, result.Config["amount"])
	assert.Equal(t, "python3.11", result.CustomBotVersion.Config.Runtime)
	assert.Equal(t, "main.py", result.CustomBotVersion.Config.Entrypoints["bot"])
}

func TestBots_CreatedTwice_IsStoredOnce(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	botEvents.ResetStream(t)
	db := mongoServer.Database()
	startBotSubscriber(t, db)

	botEvents.PublishJSON(t, SubjectBotCreated, testBotMessage("duplicate-bot"))
	botEvents.PublishJSON(t, SubjectBotCreated, testBotMessage("duplicate-bot"))
	botEvents.WaitForHandled(t)

	assert.Equal(t, int64(1), count(t, db.Collection(botCollection), bson.M{"id": "duplicate-bot"}),
		"Should only have one bot, not duplicates")
}

func TestBots_Updated_ReplacesConfigAndVersion(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	botEvents.ResetStream(t)
	db := mongoServer.Database()
	insertBot(t, db, "update-test-bot")
	startBotSubscriber(t, db)

	updated := BotMessage{
		ID:     "update-test-bot",
		Config: map[string]interface{}{"symbol": "ETH/USD", "amount": 200.0},
		CustomBotVersion: model.CustomBotVersion{
			Config: model.APIBotConfig{
				Runtime:     "nodejs20",
				Entrypoints: map[string]string{"bot": "index.js"},
			},
			Version: "2.0.0",
		},
	}
	botEvents.PublishJSON(t, SubjectBotUpdated, updated)
	botEvents.WaitForHandled(t)

	var result model.Bot
	require.NoError(t, db.Collection(botCollection).FindOne(context.Background(), bson.M{"id": "update-test-bot"}).Decode(&result))
	assert.Equal(t, "ETH/USD", result.Config["symbol"])
	assert.Equal(t, 200.0, result.Config["amount"])
	assert.Equal(t, "nodejs20", result.CustomBotVersion.Config.Runtime)
	assert.Equal(t, "index.js", result.CustomBotVersion.Config.Entrypoints["bot"])
	assert.Equal(t, "2.0.0", result.CustomBotVersion.Version)
}

func TestBots_UpdatedButNotStored_IsStored(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	botEvents.ResetStream(t)
	db := mongoServer.Database()
	startBotSubscriber(t, db)

	updated := testBotMessage("new-bot-from-update")
	updated.Config = map[string]interface{}{"created": "from update"}
	botEvents.PublishJSON(t, SubjectBotUpdated, updated)
	botEvents.WaitForHandled(t)

	var result model.Bot
	require.NoError(t, db.Collection(botCollection).FindOne(context.Background(), bson.M{"id": "new-bot-from-update"}).Decode(&result))
	assert.Equal(t, "from update", result.Config["created"])
}

func TestBots_Deleted_IsRemoved(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	botEvents.ResetStream(t)
	db := mongoServer.Database()
	insertBot(t, db, "delete-me")
	startBotSubscriber(t, db)

	botEvents.PublishJSON(t, SubjectBotDeleted, BotMessage{ID: "delete-me"})
	botEvents.WaitForHandled(t)

	assert.Zero(t, count(t, db.Collection(botCollection), bson.M{"id": "delete-me"}), "Bot should be deleted")
}

func TestBots_DeletedButNotStored_IsAcknowledged(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	botEvents.ResetStream(t)
	db := mongoServer.Database()
	startBotSubscriber(t, db)

	botEvents.PublishJSON(t, SubjectBotDeleted, BotMessage{ID: "does-not-exist"})
	botEvents.WaitForHandled(t)

	assert.Zero(t, count(t, db.Collection(botCollection), bson.M{}))
}

func TestBots_WrappedEventFormat_IsStored(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	botEvents.ResetStream(t)
	db := mongoServer.Database()
	startBotSubscriber(t, db)

	botEvents.PublishJSON(t, SubjectBotCreated, BotEvent{
		Type:      "bot.created",
		EntityID:  "wrapped-bot",
		Timestamp: time.Now().Format(time.RFC3339),
		Data: BotEventData{
			ID:     "wrapped-bot",
			Config: map[string]interface{}{"wrapped": true},
			Custom: CustomBotVersionData{
				Config: model.APIBotConfig{
					Runtime:     "python3.11",
					Entrypoints: map[string]string{"bot": "main.py"},
				},
				Version:   "1.0.0",
				FilePath:  "bots/wrapped/v1.0.0.zip",
				CreatedAt: time.Now(),
				UpdatedAt: time.Now(),
			},
		},
	})
	botEvents.WaitForHandled(t)

	var result model.Bot
	require.NoError(t, db.Collection(botCollection).FindOne(context.Background(), bson.M{"id": "wrapped-bot"}).Decode(&result))
	assert.Equal(t, true, result.Config["wrapped"])
	assert.Equal(t, "python3.11", result.CustomBotVersion.Config.Runtime)
}

func TestBots_InvalidJSON_IsDroppedAndLaterEventsHandled(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	botEvents.ResetStream(t)
	db := mongoServer.Database()
	startBotSubscriber(t, db)

	botEvents.Publish(t, SubjectBotCreated, []byte(`{invalid json}`))
	botEvents.PublishJSON(t, SubjectBotCreated, testBotMessage("after-invalid"))

	requireCount(t, db.Collection(botCollection), bson.M{"id": "after-invalid"}, 1, "Subscriber should recover from invalid JSON")
}

func TestBots_Stopped_LeavesLaterEventsForTheNextStart(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	botEvents.ResetStream(t)
	db := mongoServer.Database()
	subscriber := startBotSubscriber(t, db)
	for i := range 5 {
		botEvents.PublishJSON(t, SubjectBotCreated, testBotMessage(fmt.Sprintf("shutdown-bot-%d", i)))
	}
	botEvents.WaitForHandled(t)

	require.NoError(t, subscriber.Stop())
	botEvents.PublishJSON(t, SubjectBotCreated, testBotMessage("after-shutdown"))
	botEvents.WaitForWaiting(t, 1)

	collection := db.Collection(botCollection)
	assert.Zero(t, count(t, collection, bson.M{"id": "after-shutdown"}), "Message after shutdown should not be processed")
	assert.Equal(t, int64(5), count(t, collection, bson.M{"id": bson.M{"$regex": "^shutdown-bot-"}}),
		"Messages before shutdown should be processed")
}
