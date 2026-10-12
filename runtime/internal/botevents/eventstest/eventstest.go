// Package eventstest is test support for code that consumes THE0_EVENTS: a
// JetStream NATS container, the api's stream, and waits on a consumer's
// progress so tests never assert after a fixed sleep.
package eventstest

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"testing"
	"time"

	"runtime/internal/botevents"

	"github.com/nats-io/nats.go/jetstream"
	"github.com/stretchr/testify/require"
	"github.com/testcontainers/testcontainers-go"
	"github.com/testcontainers/testcontainers-go/wait"
	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/mongo"
	"go.mongodb.org/mongo-driver/mongo/options"
)

// Stream mirrors the THE0_EVENTS stream the api creates on startup
// (api/src/nats/nats.service.ts). The runtime only binds to it.
var Stream = jetstream.StreamConfig{
	Name: botevents.StreamName,
	Subjects: []string{
		"the0.bot.created",
		"the0.bot.updated",
		"the0.bot.deleted",
		"the0.bot-schedule.created",
		"the0.bot-schedule.updated",
		"the0.bot-schedule.deleted",
	},
	Retention: jetstream.LimitsPolicy,
	MaxAge:    7 * 24 * time.Hour,
	Storage:   jetstream.FileStorage,
}

const waitTimeout = 15 * time.Second

// StartNATS starts the NATS version the chart and Docker Compose run, with
// JetStream enabled, and returns its client URL.
func StartNATS(ctx context.Context) (testcontainers.Container, string, error) {
	container, err := testcontainers.GenericContainer(ctx, testcontainers.GenericContainerRequest{
		ContainerRequest: testcontainers.ContainerRequest{
			Image:        "nats:2.10-alpine",
			Cmd:          []string{"-js"},
			ExposedPorts: []string{"4222/tcp"},
			WaitingFor: wait.ForAll(
				wait.ForListeningPort("4222/tcp"),
				wait.ForLog("Server is ready"),
			).WithDeadline(60 * time.Second),
		},
		Started: true,
	})
	if err != nil {
		return nil, "", fmt.Errorf("failed to start NATS container: %w", err)
	}
	url, err := container.PortEndpoint(ctx, "4222/tcp", "nats")
	if err != nil {
		_ = container.Terminate(ctx)
		return nil, "", fmt.Errorf("failed to get NATS endpoint: %w", err)
	}
	return container, url, nil
}

// Events drives THE0_EVENTS and one durable consumer bound to it.
type Events struct {
	JS       jetstream.JetStream
	Consumer string
}

func (e Events) DeleteStream(t testing.TB) {
	t.Helper()
	if err := e.JS.DeleteStream(context.Background(), Stream.Name); err != nil && !errors.Is(err, jetstream.ErrStreamNotFound) {
		t.Fatalf("failed to delete %s: %v", Stream.Name, err)
	}
}

func (e Events) CreateStream(t testing.TB) {
	t.Helper()
	_, err := e.JS.CreateStream(context.Background(), Stream)
	require.NoError(t, err)
}

// ResetStream recreates THE0_EVENTS, dropping earlier tests' events and
// consumers, so each test starts from an empty stream.
func (e Events) ResetStream(t testing.TB) {
	t.Helper()
	e.DeleteStream(t)
	e.CreateStream(t)
}

// Publish publishes like the api does: into JetStream, waiting for the
// stream to store the event.
func (e Events) Publish(t testing.TB, subject string, payload []byte) {
	t.Helper()
	_, err := e.JS.Publish(context.Background(), subject, payload)
	require.NoError(t, err)
}

func (e Events) PublishJSON(t testing.TB, subject string, event any) {
	t.Helper()
	payload, err := json.Marshal(event)
	require.NoError(t, err)
	e.Publish(t, subject, payload)
}

// Info reports the consumer. Its Delivered.Consumer counts every delivery,
// redeliveries included.
func (e Events) Info(t testing.TB) *jetstream.ConsumerInfo {
	t.Helper()
	info, err := e.info()
	require.NoError(t, err)
	return info
}

func (e Events) info() (*jetstream.ConsumerInfo, error) {
	consumer, err := e.JS.Consumer(context.Background(), Stream.Name, e.Consumer)
	if err != nil {
		return nil, err
	}
	return consumer.Info(context.Background())
}

// WaitForHandled waits until the consumer has nothing left to deliver and
// nothing awaiting acknowledgement, so a test can assert on the end state
// without guessing how long handling takes.
func (e Events) WaitForHandled(t testing.TB) {
	t.Helper()
	require.Eventually(t, func() bool {
		info, err := e.info()
		return err == nil && info.NumPending == 0 && info.NumAckPending == 0
	}, waitTimeout, 50*time.Millisecond, "every published event should be handled")
}

// WaitForWaiting waits until n events are stored for the consumer and not
// yet delivered, which is how a stopped subscriber leaves new events.
func (e Events) WaitForWaiting(t testing.TB, n uint64) {
	t.Helper()
	require.Eventually(t, func() bool {
		info, err := e.info()
		return err == nil && info.NumPending == n
	}, waitTimeout, 50*time.Millisecond, "%d event(s) should be waiting for the next start", n)
}

// Repeats is how many times RequireRepeatIsHarmless repeats an event. The
// driver writes map fields in Go's random iteration order, so only some
// repeats of an update are byte-identical no-ops; this many makes sure some
// are.
const Repeats = 50

// RequireRepeatIsHarmless delivers event once and then Repeats more times,
// and requires every repeat to be acknowledged on its first delivery and to
// leave collections as the first delivery left them.
func (e Events) RequireRepeatIsHarmless(t testing.TB, db *mongo.Database, collections []string, subject string, event any) {
	t.Helper()
	e.PublishJSON(t, subject, event)
	e.WaitForHandled(t)
	once := Snapshot(t, db, collections...)

	for range Repeats {
		e.PublishJSON(t, subject, event)
	}
	e.WaitForHandled(t)
	require.Equal(t, once, Snapshot(t, db, collections...), "repeating a %s event must leave the state one delivery left", subject)
	require.Equal(t, uint64(1+Repeats), e.Info(t).Delivered.Consumer,
		"every repeated %s event should be acknowledged on its first delivery, not retried", subject)
}

// Snapshot is every document in collections, in _id order.
func Snapshot(t testing.TB, db *mongo.Database, collections ...string) map[string][]bson.M {
	t.Helper()
	state := map[string][]bson.M{}
	for _, name := range collections {
		cursor, err := db.Collection(name).Find(context.Background(), bson.M{}, options.Find().SetSort(bson.M{"_id": 1}))
		require.NoError(t, err)
		var docs []bson.M
		require.NoError(t, cursor.All(context.Background(), &docs))
		state[name] = docs
	}
	return state
}

// RejectWrites makes every write to collection fail, as a store that is down
// would.
func RejectWrites(t testing.TB, db *mongo.Database, collection string) {
	t.Helper()
	validator := bson.M{"field-no-document-has": bson.M{"$exists": true}}
	err := db.CreateCollection(context.Background(), collection, options.CreateCollection().SetValidator(validator))
	require.NoError(t, err)
}

func AcceptWrites(t testing.TB, db *mongo.Database, collection string) {
	t.Helper()
	cmd := bson.D{{Key: "collMod", Value: collection}, {Key: "validator", Value: bson.M{}}}
	require.NoError(t, db.RunCommand(context.Background(), cmd).Err())
}

// WaitForCreateAttempts waits until a subscriber's create handler has run n
// times. It claims a partition slot before writing, so the claims count its
// attempts even when the write itself is rejected.
func WaitForCreateAttempts(t testing.TB, db *mongo.Database, n int) {
	t.Helper()
	require.Eventually(t, func() bool {
		var partition struct {
			BotCount int `bson:"bot_count"`
		}
		err := db.Collection("partitions").FindOne(context.Background(), bson.M{}).Decode(&partition)
		return err == nil && partition.BotCount >= n
	}, waitTimeout, 50*time.Millisecond, "the create handler should have run %d time(s)", n)
}
