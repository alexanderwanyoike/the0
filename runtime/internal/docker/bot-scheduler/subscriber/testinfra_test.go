package subscriber

import (
	"context"
	"errors"
	"fmt"
	"os"
	"testing"
	"time"

	"github.com/nats-io/nats.go"
	"github.com/nats-io/nats.go/jetstream"
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

// eventsStream mirrors the THE0_EVENTS stream the api creates on startup
// (api/src/nats/nats.service.ts). The runtime only binds to it.
var eventsStream = jetstream.StreamConfig{
	Name: "THE0_EVENTS",
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

// resetEventsStream recreates THE0_EVENTS, dropping earlier tests' events and
// consumers, so each test starts from an empty stream.
func (ti *TestInfrastructure) resetEventsStream(t *testing.T) {
	t.Helper()
	ctx := context.Background()
	if err := ti.js.DeleteStream(ctx, eventsStream.Name); err != nil && !errors.Is(err, jetstream.ErrStreamNotFound) {
		t.Fatalf("failed to delete %s: %v", eventsStream.Name, err)
	}
	if _, err := ti.js.CreateStream(ctx, eventsStream); err != nil {
		t.Fatalf("failed to create %s: %v", eventsStream.Name, err)
	}
}

// TestMain runs once before all tests in the package
// This is the standard Go pattern for shared test setup/teardown
func TestMain(m *testing.M) {
	// Start test infrastructure once for all tests
	sharedInfra = startTestInfrastructure()

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

	// Start NATS container
	natsReq := testcontainers.ContainerRequest{
		Image:        "nats:2.10-alpine",
		Cmd:          []string{"-js"},
		ExposedPorts: []string{"4222/tcp"},
		WaitingFor: wait.ForAll(
			wait.ForListeningPort("4222/tcp"),
			wait.ForLog("Server is ready"),
		).WithDeadline(60 * time.Second),
	}

	natsC, err := testcontainers.GenericContainer(ctx, testcontainers.GenericContainerRequest{
		ContainerRequest: natsReq,
		Started:          true,
	})
	if err != nil {
		panic(fmt.Sprintf("Failed to start NATS container: %v", err))
	}

	natsHost, err := natsC.Host(ctx)
	if err != nil {
		panic(fmt.Sprintf("Failed to get NATS host: %v", err))
	}

	natsPort, err := natsC.MappedPort(ctx, "4222")
	if err != nil {
		panic(fmt.Sprintf("Failed to get NATS port: %v", err))
	}

	natsURL := fmt.Sprintf("nats://%s:%s", natsHost, natsPort.Port())

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
		natsC.Terminate(ctx)
		mongoC.Terminate(ctx)
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
