package subscriber

import (
	"context"
	"encoding/json"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/mongo"

	"runtime/internal/botevents"
	"runtime/internal/botevents/eventstest"
	"runtime/internal/contracts"
	"runtime/internal/model"
)

// eventSubjects pairs each event contract with the subject the subscriber
// consumes it on, in the order THE0_EVENTS lists them.
var eventSubjects = []struct {
	contract string
	subject  string
}{
	{"bot-created", SubjectBotCreated},
	{"bot-updated", SubjectBotUpdated},
	{"bot-deleted", SubjectBotDeleted},
	{"bot-schedule-created", SubjectBotScheduleCreated},
	{"bot-schedule-updated", SubjectBotScheduleUpdated},
	{"bot-schedule-deleted", SubjectBotScheduleDeleted},
}

// customFieldsTheRuntimeDrops are fields of custom.config the runtime does
// not store: the api serves bot frontends itself.
var customFieldsTheRuntimeDrops = []string{"hasFrontend"}

type eventContract struct {
	Subject string          `json:"subject"`
	Payload json.RawMessage `json:"payload"`
}

// loadEvent returns the contract and its payload decoded without the
// runtime's types, so a field the runtime names differently shows up as a
// difference instead of decoding to the same zero value on both sides.
func loadEvent(t *testing.T, name string) (eventContract, map[string]interface{}) {
	t.Helper()
	var contract eventContract
	contracts.Load(t, "events/"+name+".json", &contract)
	var payload map[string]interface{}
	require.NoError(t, json.Unmarshal(contract.Payload, &payload))
	return contract, payload
}

func TestContracts_EventSubjectsAreTheOnesTheSubscriberConsumes(t *testing.T) {
	for _, event := range eventSubjects {
		contract, _ := loadEvent(t, event.contract)
		assert.Equal(t, contract.Subject, event.subject, event.contract)
	}
}

func TestContracts_StreamsAreTheOnesTheApiCreates(t *testing.T) {
	var streams map[string][]string
	contracts.Load(t, "events/streams.json", &streams)

	var consumed []string
	for _, event := range eventSubjects {
		consumed = append(consumed, event.subject)
	}
	assert.Equal(t, consumed, streams[botevents.StreamName])
	assert.Equal(t, streams[eventstest.Stream.Name], eventstest.Stream.Subjects,
		"the test stream stands in for the api's")
}

func asJSONObject(t *testing.T, v interface{}) map[string]interface{} {
	t.Helper()
	encoded, err := json.Marshal(v)
	require.NoError(t, err)
	var object map[string]interface{}
	require.NoError(t, json.Unmarshal(encoded, &object))
	return object
}

// The schedule subscriber re-encodes these times, so only the instant is
// agreed, not the text.
func normaliseTimes(t *testing.T, custom map[string]interface{}) {
	t.Helper()
	for _, field := range []string{"createdAt", "updatedAt"} {
		text, _ := custom[field].(string)
		at, err := time.Parse(time.RFC3339Nano, text)
		require.NoError(t, err, "custom.%s %q", field, text)
		custom[field] = at.UTC().Format(time.RFC3339Nano)
	}
}

func requireStoredAsSent(t *testing.T, sent map[string]interface{}, stored model.Bot) {
	t.Helper()
	assert.Equal(t, sent["id"], stored.ID)
	assert.Equal(t, sent["config"], asJSONObject(t, stored.Config))

	want := asJSONObject(t, sent["custom"])
	for _, field := range customFieldsTheRuntimeDrops {
		delete(want["config"].(map[string]interface{}), field)
	}
	got := asJSONObject(t, stored.CustomBotVersion)
	normaliseTimes(t, want)
	normaliseTimes(t, got)
	assert.Equal(t, want, got)
}

// Bots and schedules are stored in the same shape apart from a schedule's
// next run, which the contract does not carry.
func findStored(t *testing.T, collection *mongo.Collection, id string) model.Bot {
	t.Helper()
	var bot model.Bot
	require.NoError(t, collection.FindOne(context.Background(), bson.M{"id": id}).Decode(&bot))
	return bot
}

func TestContracts_ApiEventsAreStoredAsSent(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	kinds := []struct {
		prefix     string
		events     *eventstest.Events
		collection string
	}{
		{"bot", &botEvents, botCollection},
		{"bot-schedule", &scheduleEvents, scheduleCollection},
	}
	for _, kind := range kinds {
		t.Run(kind.prefix, func(t *testing.T) {
			created, createdPayload := loadEvent(t, kind.prefix+"-created")
			updated, updatedPayload := loadEvent(t, kind.prefix+"-updated")
			deleted, _ := loadEvent(t, kind.prefix+"-deleted")
			id, _ := createdPayload["id"].(string)

			kind.events.ResetStream(t)
			db := mongoServer.Database()
			startSubscriber(t, db, kind.collection == botCollection, kind.collection == scheduleCollection)
			collection := db.Collection(kind.collection)

			kind.events.Publish(t, created.Subject, created.Payload)
			kind.events.WaitForHandled(t)
			requireStoredAsSent(t, createdPayload, findStored(t, collection, id))

			kind.events.Publish(t, updated.Subject, updated.Payload)
			kind.events.WaitForHandled(t)
			requireStoredAsSent(t, updatedPayload, findStored(t, collection, id))

			kind.events.Publish(t, deleted.Subject, deleted.Payload)
			kind.events.WaitForHandled(t)
			assert.Equal(t, int64(0), count(t, collection, bson.M{"id": id}))
		})
	}
}
