package subscriber

import (
	"testing"

	"go.mongodb.org/mongo-driver/mongo"

	"runtime/internal/botevents/eventstest"
)

// kind is one of the two kinds of event the subscriber stores, for the
// behaviours both share.
type kind struct {
	name       string
	consumer   string
	subjects   []string
	events     eventstest.Events
	collection string

	created, updated, deleted string
	createdEvent              func(id string) any
	// updatedEvent differs from what insert stores.
	updatedEvent func(id string) any
	deletedEvent func(id string) any

	start  func(t *testing.T, db *mongo.Database) *Subscriber
	insert func(t *testing.T, db *mongo.Database, id string)
}

// kinds is called inside tests because TestMain sets up their events.
func kinds() []kind {
	return []kind{
		{
			name:       "bot",
			consumer:   "runtime-bot-events",
			subjects:   []string{"the0.bot.created", "the0.bot.updated", "the0.bot.deleted"},
			events:     botEvents,
			collection: botCollection,
			created:    SubjectBotCreated,
			updated:    SubjectBotUpdated,
			deleted:    SubjectBotDeleted,
			createdEvent: func(id string) any {
				return testBotMessage(id)
			},
			updatedEvent: func(id string) any {
				updated := testBotMessage(id)
				updated.Config = map[string]interface{}{"symbol": "ETH/USD", "amount": 200.0}
				return updated
			},
			deletedEvent: func(id string) any {
				return BotMessage{ID: id}
			},
			start:  startBotSubscriber,
			insert: insertBot,
		},
		{
			name:       "schedule",
			consumer:   "runtime-bot-schedule-events",
			subjects:   []string{"the0.bot-schedule.created", "the0.bot-schedule.updated", "the0.bot-schedule.deleted"},
			events:     scheduleEvents,
			collection: scheduleCollection,
			created:    SubjectBotScheduleCreated,
			updated:    SubjectBotScheduleUpdated,
			deleted:    SubjectBotScheduleDeleted,
			createdEvent: func(id string) any {
				return testScheduleEvent(id, "0 0 * * *")
			},
			updatedEvent: func(id string) any {
				return testScheduleEvent(id, "0 12 * * *")
			},
			deletedEvent: func(id string) any {
				return BotScheduleEvent{ID: id}
			},
			start:  startScheduleSubscriber,
			insert: insertSchedule,
		},
	}
}
