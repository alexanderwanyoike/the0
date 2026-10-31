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

type eventContract struct {
	Subject string          `json:"subject"`
	Payload json.RawMessage `json:"payload"`
}

// eventPayload is the payload as the api means it, decoded independently of
// the runtime's own message types.
type eventPayload struct {
	ID     string                 `json:"id"`
	Config map[string]interface{} `json:"config"`
	Custom *struct {
		Config struct {
			Name        string            `json:"name"`
			Version     string            `json:"version"`
			Type        string            `json:"type"`
			Runtime     string            `json:"runtime"`
			Entrypoints map[string]string `json:"entrypoints"`
		} `json:"config"`
		CreatedAt string `json:"createdAt"`
		UpdatedAt string `json:"updatedAt"`
		FilePath  string `json:"filePath"`
		Version   string `json:"version"`
	} `json:"custom"`
}

func loadEvent(t *testing.T, name string) (eventContract, eventPayload) {
	t.Helper()
	var contract eventContract
	contracts.Load(t, "events/"+name+".json", &contract)
	var payload eventPayload
	require.NoError(t, json.Unmarshal(contract.Payload, &payload))
	return contract, payload
}

func TestContracts_EventSubjectsAreTheOnesTheSubscriberConsumes(t *testing.T) {
	for name, subject := range map[string]string{
		"bot-created":          SubjectBotCreated,
		"bot-updated":          SubjectBotUpdated,
		"bot-deleted":          SubjectBotDeleted,
		"bot-schedule-created": SubjectBotScheduleCreated,
		"bot-schedule-updated": SubjectBotScheduleUpdated,
		"bot-schedule-deleted": SubjectBotScheduleDeleted,
	} {
		contract, _ := loadEvent(t, name)
		assert.Equal(t, contract.Subject, subject, name)
	}
}

func TestContracts_StreamsAreTheOnesTheApiCreates(t *testing.T) {
	var streams map[string][]string
	contracts.Load(t, "events/streams.json", &streams)

	assert.Equal(t, []string{
		SubjectBotCreated, SubjectBotUpdated, SubjectBotDeleted,
		SubjectBotScheduleCreated, SubjectBotScheduleUpdated, SubjectBotScheduleDeleted,
	}, streams[botevents.StreamName])
	assert.Equal(t, streams[eventstest.Stream.Name], eventstest.Stream.Subjects,
		"the test stream stands in for the api's")
}

func requireStoredAsSent(t *testing.T, sent eventPayload, stored model.Bot) {
	t.Helper()
	assert.Equal(t, sent.ID, stored.ID)
	storedConfig, err := json.Marshal(stored.Config)
	require.NoError(t, err)
	sentConfig, err := json.Marshal(sent.Config)
	require.NoError(t, err)
	assert.JSONEq(t, string(sentConfig), string(storedConfig))

	custom := stored.CustomBotVersion
	assert.Equal(t, sent.Custom.Version, custom.Version)
	assert.Equal(t, sent.Custom.FilePath, custom.FilePath)
	assert.Equal(t, sent.Custom.Config.Name, custom.Config.Name)
	assert.Equal(t, sent.Custom.Config.Version, custom.Config.Version)
	assert.Equal(t, sent.Custom.Config.Type, custom.Config.Type)
	assert.Equal(t, sent.Custom.Config.Runtime, custom.Config.Runtime)
	assert.Equal(t, sent.Custom.Config.Entrypoints, custom.Config.Entrypoints)
	assertSameInstant(t, sent.Custom.CreatedAt, custom.CreatedAt)
	assertSameInstant(t, sent.Custom.UpdatedAt, custom.UpdatedAt)
}

// The schedule subscriber re-encodes these times, so only the instant is
// agreed, not the text.
func assertSameInstant(t *testing.T, sent, stored string) {
	t.Helper()
	want, err := time.Parse(time.RFC3339Nano, sent)
	require.NoError(t, err)
	got, err := time.Parse(time.RFC3339Nano, stored)
	require.NoError(t, err, "stored time %q", stored)
	assert.True(t, want.Equal(got), "sent %s, stored %s", sent, stored)
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

			kind.events.ResetStream(t)
			db := mongoServer.Database()
			startSubscriber(t, db, kind.collection == botCollection, kind.collection == scheduleCollection)
			collection := db.Collection(kind.collection)

			kind.events.Publish(t, created.Subject, created.Payload)
			kind.events.WaitForHandled(t)
			requireStoredAsSent(t, createdPayload, findStored(t, collection, createdPayload.ID))

			kind.events.Publish(t, updated.Subject, updated.Payload)
			kind.events.WaitForHandled(t)
			requireStoredAsSent(t, updatedPayload, findStored(t, collection, updatedPayload.ID))

			kind.events.Publish(t, deleted.Subject, deleted.Payload)
			kind.events.WaitForHandled(t)
			assert.Equal(t, int64(0), count(t, collection, bson.M{"id": createdPayload.ID}))
		})
	}
}
