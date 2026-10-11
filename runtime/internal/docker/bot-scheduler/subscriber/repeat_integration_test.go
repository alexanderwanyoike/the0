package subscriber

import (
	"testing"
)

// writtenCollections is everything the subscriber writes: its own collection
// and the partition and counter bookkeeping.
var writtenCollections = []string{testCollection, "partitions", "counters"}

func TestRepeat_ScheduleCreated_LeavesTheSameState(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	events.ResetStream(t)
	db := sharedInfra.GetDatabase("schedule_created_repeatedly")
	startSubscriber(t, db)

	events.RequireRepeatIsHarmless(t, db, writtenCollections, SubjectBotScheduleCreated, testScheduleEvent("created-repeatedly", "0 0 * * *"))
}

func TestRepeat_ScheduleDeleted_LeavesTheSameState(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	events.ResetStream(t)
	db := sharedInfra.GetDatabase("schedule_deleted_repeatedly")
	insertSchedule(t, db, "deleted-repeatedly", 2)
	startSubscriber(t, db)

	events.RequireRepeatIsHarmless(t, db, writtenCollections, SubjectBotScheduleDeleted, BotScheduleEvent{ID: "deleted-repeatedly"})
}

func TestRepeat_ScheduleUpdated_LeavesTheSameState(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	events.ResetStream(t)
	db := sharedInfra.GetDatabase("schedule_updated_repeatedly")
	insertSchedule(t, db, "updated-repeatedly", 1)
	startSubscriber(t, db)

	events.RequireRepeatIsHarmless(t, db, writtenCollections, SubjectBotScheduleUpdated, testScheduleEvent("updated-repeatedly", "0 12 * * *"))
}
