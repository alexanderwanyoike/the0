package subscriber

import (
	"context"
	"fmt"
	"os"
	"runtime/internal/botevents/eventstest"
	"runtime/internal/model"
	"testing"
	"time"

	"github.com/nats-io/nats.go"
	"github.com/nats-io/nats.go/jetstream"
	"github.com/stretchr/testify/require"
	"github.com/testcontainers/testcontainers-go"
	"github.com/testcontainers/testcontainers-go/wait"
	"go.mongodb.org/mongo-driver/mongo"
	"go.mongodb.org/mongo-driver/mongo/options"
)

// Shared test infrastructure for all subscriber integration tests
// This optimization reduces test execution time by creating containers once
// instead of multiple times per test. Tests remain isolated through unique
// database/collection names.
//
// IMPORTANT: Tests using sharedInfra MUST NOT call t.Parallel() because they
// share the same containers. Tests run sequentially by default in Go.
var sharedInfra *TestInfrastructure

// events is THE0_EVENTS and this subscriber's durable consumer on it.
var events eventstest.Events

// TestInfrastructure manages NATS and MongoDB testcontainers for integration tests
type TestInfrastructure struct {
	natsContainer  testcontainers.Container
	mongoContainer testcontainers.Container
	natsConn       *nats.Conn
	js             jetstream.JetStream
	mongoClient    *mongo.Client
	NATSURL        string
	MongoURI       string
}

// Close terminates all containers and closes connections
func (ti *TestInfrastructure) Close() {
	if ti.natsConn != nil {
		ti.natsConn.Close()
	}
	if ti.mongoClient != nil {
		ti.mongoClient.Disconnect(context.Background())
	}
	if ti.natsContainer != nil {
		ti.natsContainer.Terminate(context.Background())
	}
	if ti.mongoContainer != nil {
		ti.mongoContainer.Terminate(context.Background())
	}
}

// GetDatabase returns a MongoDB database handle with a unique name for test isolation
func (ti *TestInfrastructure) GetDatabase(testName string) *mongo.Database {
	dbName := fmt.Sprintf("test_%s_%d", testName, time.Now().UnixNano())
	return ti.mongoClient.Database(dbName)
}

const testCollection = "bot_schedules"

// startSubscriber starts a subscriber writing to db and stops it when the
// test ends.
func startSubscriber(t *testing.T, db *mongo.Database) *NATSSubscriber {
	t.Helper()
	t.Setenv("MONGO_URI", sharedInfra.MongoURI)
	subscriber, err := NewNATSSubscriber(context.Background(), SubscriberOptions{
		NATSUrl:        sharedInfra.NATSURL,
		DBName:         db.Name(),
		CollectionName: testCollection,
		MaxRetries:     3,
		Logger:         &testLogger{t: t},
	})
	require.NoError(t, err)
	t.Cleanup(func() { _ = subscriber.Stop() })
	require.NoError(t, subscriber.Start(context.Background()))
	return subscriber
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

// insertSchedule stores a schedule as an earlier created event would have.
func insertSchedule(t *testing.T, db *mongo.Database, id string) {
	t.Helper()
	_, err := db.Collection(testCollection).InsertOne(context.Background(), model.BotSchedule{
		ID:                id,
		NextExecutionTime: time.Now().Add(time.Hour).Unix(),
		Config:            map[string]interface{}{"symbol": "BTC/USD", "schedule": "0 0 * * *"},
		CustomBotVersion:  model.CustomBotVersion{Version: "1.0.0"},
	})
	require.NoError(t, err)
}

// TestMain runs once before all tests in the package
// This is the standard Go pattern for shared test setup/teardown
func TestMain(m *testing.M) {
	// Start test infrastructure once for all tests
	sharedInfra = startTestInfrastructure()
	events = eventstest.Events{JS: sharedInfra.js, Consumer: ConsumerName}

	// Run all tests sequentially (do not use -parallel flag with this package)
	exitCode := m.Run()

	// Cleanup after all tests complete
	if sharedInfra != nil {
		sharedInfra.Close()
	}

	os.Exit(exitCode)
}

// startTestInfrastructure creates NATS and MongoDB testcontainers
func startTestInfrastructure() *TestInfrastructure {
	ctx := context.Background()

	natsC, natsURL, err := eventstest.StartNATS(ctx)
	if err != nil {
		panic(err.Error())
	}

	// Start MongoDB container
	mongoReq := testcontainers.ContainerRequest{
		Image:        "mongo:latest",
		ExposedPorts: []string{"27017/tcp"},
		WaitingFor: wait.ForAll(
			wait.ForListeningPort("27017/tcp"),
			wait.ForLog("Waiting for connections"),
		).WithDeadline(60 * time.Second),
	}

	mongoC, err := testcontainers.GenericContainer(ctx, testcontainers.GenericContainerRequest{
		ContainerRequest: mongoReq,
		Started:          true,
	})
	if err != nil {
		natsC.Terminate(ctx) // Cleanup NATS if MongoDB fails
		panic(fmt.Sprintf("Failed to start MongoDB container: %v", err))
	}

	mongoHost, err := mongoC.Host(ctx)
	if err != nil {
		natsC.Terminate(ctx)
		mongoC.Terminate(ctx)
		panic(fmt.Sprintf("Failed to get MongoDB host: %v", err))
	}

	mongoPort, err := mongoC.MappedPort(ctx, "27017")
	if err != nil {
		natsC.Terminate(ctx)
		mongoC.Terminate(ctx)
		panic(fmt.Sprintf("Failed to get MongoDB port: %v", err))
	}

	mongoURI := fmt.Sprintf("mongodb://%s:%s", mongoHost, mongoPort.Port())

	// Connect NATS client
	nc, err := nats.Connect(natsURL, nats.Timeout(10*time.Second))
	if err != nil {
		natsC.Terminate(ctx)
		mongoC.Terminate(ctx)
		panic(fmt.Sprintf("Failed to connect to NATS: %v", err))
	}

	js, err := jetstream.New(nc)
	if err != nil {
		nc.Close()
		_ = natsC.Terminate(ctx)
		_ = mongoC.Terminate(ctx)
		panic(fmt.Sprintf("Failed to create JetStream context: %v", err))
	}

	// Connect MongoDB client
	clientOpts := options.Client().
		ApplyURI(mongoURI).
		SetServerSelectionTimeout(10 * time.Second).
		SetConnectTimeout(10 * time.Second)

	mongoClient, err := mongo.Connect(ctx, clientOpts)
	if err != nil {
		nc.Close()
		natsC.Terminate(ctx)
		mongoC.Terminate(ctx)
		panic(fmt.Sprintf("Failed to connect to MongoDB: %v", err))
	}

	// Verify MongoDB connection
	if err := mongoClient.Ping(ctx, nil); err != nil {
		nc.Close()
		mongoClient.Disconnect(ctx)
		natsC.Terminate(ctx)
		mongoC.Terminate(ctx)
		panic(fmt.Sprintf("Failed to ping MongoDB: %v", err))
	}

	return &TestInfrastructure{
		natsContainer:  natsC,
		mongoContainer: mongoC,
		natsConn:       nc,
		js:             js,
		mongoClient:    mongoClient,
		NATSURL:        natsURL,
		MongoURI:       mongoURI,
	}
}
