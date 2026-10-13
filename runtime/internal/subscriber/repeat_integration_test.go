package subscriber

import (
	"testing"
)

func TestRepeat_Created_LeavesTheSameState(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	for _, k := range kinds() {
		t.Run(k.name, func(t *testing.T) {
			k.events.ResetStream(t)
			db := mongoServer.Database()
			k.start(t, db)

			k.events.RequireRepeatIsHarmless(t, db, []string{k.collection}, k.created, k.createdEvent("created-repeatedly"))
		})
	}
}

func TestRepeat_Updated_LeavesTheSameState(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	for _, k := range kinds() {
		t.Run(k.name, func(t *testing.T) {
			k.events.ResetStream(t)
			db := mongoServer.Database()
			k.insert(t, db, "updated-repeatedly")
			k.start(t, db)

			k.events.RequireRepeatIsHarmless(t, db, []string{k.collection}, k.updated, k.updatedEvent("updated-repeatedly"))
		})
	}
}

func TestRepeat_Deleted_LeavesTheSameState(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	for _, k := range kinds() {
		t.Run(k.name, func(t *testing.T) {
			k.events.ResetStream(t)
			db := mongoServer.Database()
			k.insert(t, db, "deleted-repeatedly")
			k.start(t, db)

			k.events.RequireRepeatIsHarmless(t, db, []string{k.collection}, k.deleted, k.deletedEvent("deleted-repeatedly"))
		})
	}
}
