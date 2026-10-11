package subscriber

import (
	"context"
	"testing"

	"github.com/stretchr/testify/require"
	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/mongo"
	"go.mongodb.org/mongo-driver/mongo/options"
)

// The driver writes map fields in Go's random iteration order, so only some
// repeats of an update are byte-identical no-ops; this many makes sure some
// are.
const repeats = 50

// snapshot is everything the subscriber writes: the schedules and the
// partition and counter bookkeeping.
func snapshot(t *testing.T, db *mongo.Database) map[string][]bson.M {
	t.Helper()
	state := map[string][]bson.M{}
	for _, name := range []string{testCollection, "partitions", "counters"} {
		cursor, err := db.Collection(name).Find(context.Background(), bson.M{}, options.Find().SetSort(bson.M{"_id": 1}))
		require.NoError(t, err)
		var docs []bson.M
		require.NoError(t, cursor.All(context.Background(), &docs))
		state[name] = docs
	}
	return state
}

// requireRepeatIsHarmless delivers event once and then again and again, and
// requires every repeat to be acknowledged on its first delivery and to leave
// the state the first delivery left.
func requireRepeatIsHarmless(t *testing.T, db *mongo.Database, subject string, event any) {
	t.Helper()
	publishEvent(t, subject, event)
	waitForEventsHandled(t)
	once := snapshot(t, db)

	for range repeats {
		publishEvent(t, subject, event)
	}
	waitForEventsHandled(t)
	require.Equal(t, once, snapshot(t, db), "repeating a %s event must leave the state one delivery left", subject)
	require.Equal(t, uint64(1+repeats), consumerInfo(t).Delivered.Consumer,
		"every repeated %s event should be acknowledged on its first delivery, not retried", subject)
}

func TestRepeat_ScheduleUpdated_LeavesTheSameState(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	sharedInfra.resetEventsStream(t)
	db := sharedInfra.GetDatabase("schedule_updated_repeatedly")
	insertSchedule(t, db, "updated-repeatedly", 1)
	startSubscriber(t, db)

	requireRepeatIsHarmless(t, db, SubjectBotScheduleUpdated, testScheduleEvent("updated-repeatedly", "0 12 * * *"))
}
