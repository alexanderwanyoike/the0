package natssubscriber

import (
	"testing"
	"time"

	"github.com/stretchr/testify/require"
	"go.mongodb.org/mongo-driver/bson"
)

func TestDelivery_BotCreatedWhileStopped_IsHandledOnStart(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	sharedInfra.resetEventsStream(t)
	db := sharedInfra.GetDatabase("created_while_stopped")

	publishEvent(t, SubjectBotCreated, testBotMessage("created-while-stopped"))
	startSubscriber(t, db)

	err := WaitForDocument(db.Collection(testCollection), bson.M{"id": "created-while-stopped"}, 10*time.Second)
	require.NoError(t, err, "a bot created while the runtime was down should be stored once it starts")
}

func TestDelivery_BotDeletedWhileStopped_IsRemovedOnStart(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	sharedInfra.resetEventsStream(t)
	db := sharedInfra.GetDatabase("deleted_while_stopped")
	insertBot(t, db, "deleted-while-stopped", 1)

	publishEvent(t, SubjectBotDeleted, BotMessage{ID: "deleted-while-stopped"})
	startSubscriber(t, db)

	err := WaitForDocumentCount(db.Collection(testCollection), bson.M{"id": "deleted-while-stopped"}, 0, 10*time.Second)
	require.NoError(t, err, "a bot deleted while the runtime was down should stop being run once it starts")
}

func TestDelivery_FailedEvent_IsRetried(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	sharedInfra.resetEventsStream(t)
	db := sharedInfra.GetDatabase("failed_event_retried")
	rejectWrites(t, db)
	startSubscriber(t, db)

	publishEvent(t, SubjectBotCreated, testBotMessage("retried-bot"))
	waitForCreateAttempts(t, db, 1)
	acceptWrites(t, db)

	err := WaitForDocument(db.Collection(testCollection), bson.M{"id": "retried-bot"}, 15*time.Second)
	require.NoError(t, err, "a create that failed should be retried until the bot is stored")
}
