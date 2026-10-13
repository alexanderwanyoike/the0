package subscriber

import (
	"testing"

	"github.com/stretchr/testify/require"
	"go.mongodb.org/mongo-driver/bson"

	"runtime/internal/botevents/eventstest"
)

func TestDelivery_CreatedWhileStopped_IsHandledOnStart(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	for _, k := range kinds() {
		t.Run(k.name, func(t *testing.T) {
			k.events.ResetStream(t)
			db := mongoServer.Database()

			k.events.PublishJSON(t, k.created, k.createdEvent("created-while-stopped"))
			k.start(t, db)

			requireCount(t, db.Collection(k.collection), bson.M{"id": "created-while-stopped"}, 1,
				"a %s created while the runtime was down should be stored once it starts", k.name)
		})
	}
}

func TestDelivery_DeletedWhileStopped_IsRemovedOnStart(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	for _, k := range kinds() {
		t.Run(k.name, func(t *testing.T) {
			k.events.ResetStream(t)
			db := mongoServer.Database()
			k.insert(t, db, "deleted-while-stopped")

			k.events.PublishJSON(t, k.deleted, k.deletedEvent("deleted-while-stopped"))
			k.start(t, db)

			requireCount(t, db.Collection(k.collection), bson.M{"id": "deleted-while-stopped"}, 0,
				"a %s deleted while the runtime was down should stop running once it starts", k.name)
		})
	}
}

func TestDelivery_NATSConnection_HasNoReconnectLimit(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	botEvents.ResetStream(t)
	subscriber := startSubscriber(t, mongoServer.Database(), true, true)

	require.Equal(t, -1, subscriber.natsConn.Opts.MaxReconnect,
		"the subscriber must keep reconnecting however long NATS is away, or it stops consuming until restarted")
}

func TestDelivery_FailedEvent_IsRetried(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	for _, k := range kinds() {
		t.Run(k.name, func(t *testing.T) {
			k.events.ResetStream(t)
			db := mongoServer.Database()
			eventstest.RejectWrites(t, db, k.collection)
			k.start(t, db)

			k.events.PublishJSON(t, k.created, k.createdEvent("retried"))
			k.events.WaitForRetry(t)
			eventstest.AcceptWrites(t, db, k.collection)

			requireCount(t, db.Collection(k.collection), bson.M{"id": "retried"}, 1,
				"a create that failed should be retried until the %s is stored", k.name)
		})
	}
}

func TestDelivery_RetriedCreate_IsNotOvertakenByItsDelete(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	for _, k := range kinds() {
		t.Run(k.name, func(t *testing.T) {
			k.events.ResetStream(t)
			db := mongoServer.Database()
			eventstest.RejectWrites(t, db, k.collection)
			k.start(t, db)

			k.events.PublishJSON(t, k.created, k.createdEvent("created-then-deleted"))
			k.events.WaitForRetry(t)
			k.events.PublishJSON(t, k.deleted, k.deletedEvent("created-then-deleted"))
			eventstest.AcceptWrites(t, db, k.collection)
			k.events.WaitForHandled(t)

			require.Zero(t, count(t, db.Collection(k.collection), bson.M{"id": "created-then-deleted"}),
				"the delete must apply after the create it follows, or the deleted %s keeps running", k.name)
		})
	}
}
