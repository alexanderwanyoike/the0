package subscriber

import (
	"context"
	"testing"
	"time"

	"github.com/stretchr/testify/require"
	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/mongo"
)

func countSchedules(db *mongo.Database, id string) int64 {
	count, err := db.Collection(testCollection).CountDocuments(context.Background(), bson.M{"id": id})
	if err != nil {
		return -1
	}
	return count
}

func TestDelivery_ScheduleCreatedWhileStopped_IsHandledOnStart(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	sharedInfra.resetEventsStream(t)
	db := sharedInfra.GetDatabase("created_while_stopped")

	publishEvent(t, SubjectBotScheduleCreated, testScheduleEvent("created-while-stopped", "0 0 * * *"))
	startSubscriber(t, db)

	err := waitFor(10*time.Second, func() bool { return countSchedules(db, "created-while-stopped") == 1 })
	require.NoError(t, err, "a schedule created while the runtime was down should be stored once it starts")
}

func TestDelivery_ScheduleDeletedWhileStopped_IsRemovedOnStart(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	sharedInfra.resetEventsStream(t)
	db := sharedInfra.GetDatabase("deleted_while_stopped")
	insertSchedule(t, db, "deleted-while-stopped", 1)

	publishEvent(t, SubjectBotScheduleDeleted, BotScheduleEvent{ID: "deleted-while-stopped"})
	startSubscriber(t, db)

	err := waitFor(10*time.Second, func() bool { return countSchedules(db, "deleted-while-stopped") == 0 })
	require.NoError(t, err, "a schedule deleted while the runtime was down should stop running once it starts")
}
