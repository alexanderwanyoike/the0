package subscriber

import (
	"context"
	"flag"
	"fmt"
	"os"
	"testing"
	"time"

	"github.com/nats-io/nats.go"
	"github.com/nats-io/nats.go/jetstream"
	"github.com/stretchr/testify/require"
	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/mongo"

	"runtime/internal/botevents/eventstest"
	"runtime/internal/model"
	"runtime/internal/mongotest"
)

// Integration tests share one NATS and one MongoDB container and must not
// call t.Parallel.
var (
	natsURL        string
	natsConn       *nats.Conn
	mongoServer    *mongotest.Server
	botEvents      eventstest.Events
	scheduleEvents eventstest.Events
)

const (
	botCollection      = "bots"
	scheduleCollection = "bot_schedules"
)

func TestMain(m *testing.M) {
	flag.Parse()
	if testing.Short() {
		os.Exit(m.Run())
	}

	ctx := context.Background()
	natsContainer, url, err := eventstest.StartNATS(ctx)
	if err != nil {
		panic(err.Error())
	}
	mongoServer, err = mongotest.Start(ctx)
	if err != nil {
		_ = natsContainer.Terminate(ctx)
		panic(err.Error())
	}
	natsURL = url
	natsConn, err = nats.Connect(url, nats.Timeout(10*time.Second))
	if err != nil {
		panic(fmt.Sprintf("failed to connect to NATS: %v", err))
	}
	js, err := jetstream.New(natsConn)
	if err != nil {
		panic(fmt.Sprintf("failed to create JetStream context: %v", err))
	}
	botEvents = eventstest.Events{JS: js, Consumer: BotConsumerName}
	scheduleEvents = eventstest.Events{JS: js, Consumer: ScheduleConsumerName}

	code := m.Run()

	natsConn.Close()
	_ = mongoServer.Terminate(ctx)
	_ = natsContainer.Terminate(ctx)
	os.Exit(code)
}

// startSubscriber starts a subscriber that stores bot events, schedule
// events or both in db, and stops it when the test ends.
func startSubscriber(t *testing.T, db *mongo.Database, bots, schedules bool) *Subscriber {
	t.Helper()
	t.Setenv("MONGO_URI", mongoServer.URI)
	opts := Options{NATSUrl: natsURL, Logger: &testLogger{t: t}}
	if bots {
		opts.Bots = &Collection{DB: db.Name(), Name: botCollection}
	}
	if schedules {
		opts.Schedules = &Collection{DB: db.Name(), Name: scheduleCollection}
	}
	subscriber, err := New(context.Background(), opts)
	require.NoError(t, err)
	t.Cleanup(func() { _ = subscriber.Stop() })
	require.NoError(t, subscriber.Start(context.Background()))
	return subscriber
}

func startBotSubscriber(t *testing.T, db *mongo.Database) *Subscriber {
	return startSubscriber(t, db, true, false)
}

func startScheduleSubscriber(t *testing.T, db *mongo.Database) *Subscriber {
	return startSubscriber(t, db, false, true)
}

func testBotMessage(id string) BotMessage {
	return BotMessage{
		ID:     id,
		Config: map[string]interface{}{"symbol": "BTC/USD"},
		CustomBotVersion: model.CustomBotVersion{
			Config: model.APIBotConfig{
				Runtime:     "python3.11",
				Entrypoints: map[string]string{"bot": "main.py"},
			},
			Version: "1.0.0",
		},
	}
}

func testScheduleEvent(id, schedule string) BotScheduleEvent {
	return BotScheduleEvent{
		ID:     id,
		Config: map[string]interface{}{"symbol": "BTC/USD", "schedule": schedule},
		Custom: CustomBotScheduleVersionData{
			Config: model.APIBotConfig{
				Runtime:     "python3.11",
				Entrypoints: map[string]string{"bot": "main.py"},
			},
			Version: "1.0.0",
		},
	}
}

// insertBot stores a bot as an earlier created event would have.
func insertBot(t *testing.T, db *mongo.Database, id string) {
	t.Helper()
	_, err := db.Collection(botCollection).InsertOne(context.Background(), model.Bot{
		ID:               id,
		Config:           map[string]interface{}{"symbol": "BTC/USD"},
		CustomBotVersion: testBotMessage(id).CustomBotVersion,
	})
	require.NoError(t, err)
}

// insertSchedule stores a schedule as an earlier created event would have.
func insertSchedule(t *testing.T, db *mongo.Database, id string) {
	t.Helper()
	_, err := db.Collection(scheduleCollection).InsertOne(context.Background(), model.BotSchedule{
		ID:                id,
		NextExecutionTime: time.Now().Add(time.Hour).Unix(),
		Config:            map[string]interface{}{"symbol": "BTC/USD", "schedule": "0 0 * * *"},
		CustomBotVersion:  model.CustomBotVersion{Version: "1.0.0"},
	})
	require.NoError(t, err)
}

func count(t *testing.T, collection *mongo.Collection, filter bson.M) int64 {
	t.Helper()
	n, err := collection.CountDocuments(context.Background(), filter)
	require.NoError(t, err)
	return n
}

// requireCount waits until filter matches want documents.
func requireCount(t *testing.T, collection *mongo.Collection, filter bson.M, want int64, msgAndArgs ...interface{}) {
	t.Helper()
	require.Eventually(t, func() bool {
		n, err := collection.CountDocuments(context.Background(), filter)
		return err == nil && n == want
	}, 20*time.Second, 50*time.Millisecond, msgAndArgs...)
}

// testLogger is a simple logger that logs to testing.T
type testLogger struct {
	t *testing.T
}

func (l *testLogger) Info(msg string, keysAndValues ...interface{}) {
	l.t.Logf("[INFO] %s %v", msg, keysAndValues)
}

func (l *testLogger) Error(msg string, keysAndValues ...interface{}) {
	l.t.Logf("[ERROR] %s %v", msg, keysAndValues)
}

func (l *testLogger) Debug(msg string, keysAndValues ...interface{}) {
	l.t.Logf("[DEBUG] %s %v", msg, keysAndValues)
}

func (l *testLogger) Warn(msg string, keysAndValues ...interface{}) {
	l.t.Logf("[WARN] %s %v", msg, keysAndValues)
}
