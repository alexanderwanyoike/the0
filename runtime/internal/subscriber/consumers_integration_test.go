package subscriber

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"go.mongodb.org/mongo-driver/bson"

	"runtime/internal/botevents"
)

// A running installation consumes THE0_EVENTS through two durable consumers,
// one per kind. JetStream keeps each one's position by name and subjects, so
// the subscriber must keep both exactly for an upgrade to lose and replay
// nothing.

func TestConsumers_OneSubscriberHandlesEachKindOnItsOwnDurableConsumer(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	botEvents.ResetStream(t)
	db := mongoServer.Database()
	subscriber := startSubscriber(t, db, true, true)
	require.True(t, subscriber.IsReady())

	for _, k := range kinds() {
		k.events.PublishJSON(t, k.created, k.createdEvent("one-subscriber-"+k.name))
	}
	for _, k := range kinds() {
		k.events.WaitForHandled(t)
		config := k.events.Info(t).Config
		assert.Equal(t, k.consumer, config.Durable)
		assert.Equal(t, k.subjects, config.FilterSubjects)
		assert.Equal(t, int64(1), count(t, db.Collection(k.collection), bson.M{"id": "one-subscriber-" + k.name}),
			"the %s event should be stored in the %s collection", k.name, k.collection)
	}
}

func TestConsumers_AnUpgradedRuntimeResumesWhereItsOldConsumerStopped(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	for _, k := range kinds() {
		t.Run(k.name, func(t *testing.T) {
			k.events.ResetStream(t)
			db := mongoServer.Database()

			// The runtime before the upgrade: a consumer of the same name and
			// subjects, which is all its configuration was derived from.
			nc, err := botevents.Connect(natsURL)
			require.NoError(t, err)
			old, err := botevents.NewConsumer(nc, botevents.Config{
				Name:     k.consumer,
				Subjects: k.subjects,
				Handler:  func(context.Context, string, []byte) error { return nil },
				Logger:   &testLogger{t: t},
			})
			require.NoError(t, err)
			require.NoError(t, old.Start(context.Background()))
			k.events.PublishJSON(t, k.created, k.createdEvent("handled-before-the-upgrade"))
			k.events.WaitForHandled(t)
			old.Stop()
			nc.Close()

			k.events.PublishJSON(t, k.created, k.createdEvent("published-during-the-upgrade"))
			k.start(t, db)
			k.events.WaitForHandled(t)

			collection := db.Collection(k.collection)
			assert.Zero(t, count(t, collection, bson.M{"id": "handled-before-the-upgrade"}),
				"an event the old runtime acknowledged must not be replayed")
			assert.Equal(t, int64(1), count(t, collection, bson.M{"id": "published-during-the-upgrade"}),
				"an event published while no runtime ran must be handled")
		})
	}
}
