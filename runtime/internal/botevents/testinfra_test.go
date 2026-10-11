package botevents

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"os"
	"sync"
	"testing"
	"time"

	"github.com/nats-io/nats.go"
	"github.com/nats-io/nats.go/jetstream"
	"github.com/stretchr/testify/require"
	"github.com/testcontainers/testcontainers-go"
	"github.com/testcontainers/testcontainers-go/wait"
)

// Integration tests share one NATS container and must not call t.Parallel.
var (
	natsConn *nats.Conn
	js       jetstream.JetStream
)

const testSubject = "the0.bot.created"

// eventsStream mirrors the THE0_EVENTS stream the api creates on startup
// (api/src/nats/nats.service.ts).
var eventsStream = jetstream.StreamConfig{
	Name: StreamName,
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

func TestMain(m *testing.M) {
	flag.Parse()
	if testing.Short() {
		os.Exit(m.Run())
	}

	ctx := context.Background()
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
		panic(fmt.Sprintf("failed to start NATS container: %v", err))
	}
	url, err := container.PortEndpoint(ctx, "4222/tcp", "nats")
	if err != nil {
		panic(fmt.Sprintf("failed to get NATS endpoint: %v", err))
	}
	natsConn, err = nats.Connect(url)
	if err != nil {
		panic(fmt.Sprintf("failed to connect to NATS: %v", err))
	}
	js, err = jetstream.New(natsConn)
	if err != nil {
		panic(fmt.Sprintf("failed to create JetStream context: %v", err))
	}

	code := m.Run()

	natsConn.Close()
	_ = container.Terminate(ctx)
	os.Exit(code)
}

func deleteEventsStream(t *testing.T) {
	t.Helper()
	if err := js.DeleteStream(context.Background(), StreamName); err != nil && !errors.Is(err, jetstream.ErrStreamNotFound) {
		t.Fatalf("failed to delete %s: %v", StreamName, err)
	}
}

func createEventsStream(t *testing.T) {
	t.Helper()
	_, err := js.CreateStream(context.Background(), eventsStream)
	require.NoError(t, err)
}

func publish(t *testing.T, payload string) {
	t.Helper()
	_, err := js.Publish(context.Background(), testSubject, []byte(payload))
	require.NoError(t, err)
}

// recorder is a Handler that remembers the payloads it handled.
type recorder struct {
	mu      sync.Mutex
	handled []string
}

func (r *recorder) handle(_ context.Context, _ string, data []byte) error {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.handled = append(r.handled, string(data))
	return nil
}

func (r *recorder) has(payload string) bool {
	r.mu.Lock()
	defer r.mu.Unlock()
	for _, h := range r.handled {
		if h == payload {
			return true
		}
	}
	return false
}

func (r *recorder) waitFor(t *testing.T, payload string) {
	t.Helper()
	require.Eventually(t, func() bool { return r.has(payload) }, 15*time.Second, 50*time.Millisecond,
		"event %q should be handled", payload)
}

// newConsumer builds a consumer that records what it handles and is stopped
// when the test ends.
func newConsumer(t *testing.T, handler Handler) *Consumer {
	t.Helper()
	consumer, err := NewConsumer(natsConn, Config{
		Name:     "test-consumer",
		Subjects: []string{testSubject},
		Handler:  handler,
		Logger:   testLogger{t: t},
	})
	require.NoError(t, err)
	t.Cleanup(consumer.Stop)
	return consumer
}

type testLogger struct{ t *testing.T }

func (l testLogger) Info(msg string, kv ...interface{})  { l.t.Logf("[INFO] %s %v", msg, kv) }
func (l testLogger) Error(msg string, kv ...interface{}) { l.t.Logf("[ERROR] %s %v", msg, kv) }
func (l testLogger) Debug(msg string, kv ...interface{}) { l.t.Logf("[DEBUG] %s %v", msg, kv) }
func (l testLogger) Warn(msg string, kv ...interface{})  { l.t.Logf("[WARN] %s %v", msg, kv) }
