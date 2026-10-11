package natssubscriber

import (
	"testing"
)

// writtenCollections is everything the subscriber writes: its own collection
// and the partition and counter bookkeeping.
var writtenCollections = []string{testCollection, "partitions", "counters"}

func TestRepeat_BotCreated_LeavesTheSameState(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	events.ResetStream(t)
	db := sharedInfra.GetDatabase("bot_created_repeatedly")
	startSubscriber(t, db)

	events.RequireRepeatIsHarmless(t, db, writtenCollections, SubjectBotCreated, testBotMessage("created-repeatedly"))
}

func TestRepeat_BotUpdated_LeavesTheSameState(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	events.ResetStream(t)
	db := sharedInfra.GetDatabase("bot_updated_repeatedly")
	insertBot(t, db, "updated-repeatedly", 1)
	startSubscriber(t, db)

	updated := testBotMessage("updated-repeatedly")
	updated.Config = map[string]interface{}{"symbol": "ETH/USD", "amount": 200.0}
	events.RequireRepeatIsHarmless(t, db, writtenCollections, SubjectBotUpdated, updated)
}

func TestRepeat_BotDeleted_LeavesTheSameState(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	events.ResetStream(t)
	db := sharedInfra.GetDatabase("bot_deleted_repeatedly")
	insertBot(t, db, "deleted-repeatedly", 2)
	startSubscriber(t, db)

	events.RequireRepeatIsHarmless(t, db, writtenCollections, SubjectBotDeleted, BotMessage{ID: "deleted-repeatedly"})
}
