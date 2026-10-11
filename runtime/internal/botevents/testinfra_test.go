package botevents_test

import (
	"context"
	"flag"
	"fmt"
	"os"
	"sync"
	"testing"
	"time"

	"runtime/internal/botevents"
	"runtime/internal/botevents/eventstest"

	"github.com/nats-io/nats.go"
	"github.com/nats-io/nats.go/jetstream"
	"github.com/stretchr/testify/require"
)

// Integration tests share one NATS container and must not call t.Parallel.
var (
	natsConn *nats.Conn
	events   eventstest.Events
)

const (
	testSubject  = "the0.bot.created"
	testConsumer = "test-consumer"
)

func TestMain(m *testing.M) {
	flag.Parse()
	if testing.Short() {
		os.Exit(m.Run())
	}

	ctx := context.Background()
	container, url, err := eventstest.StartNATS(ctx)
	if err != nil {
		panic(err.Error())
	}
	natsConn, err = nats.Connect(url)
	if err != nil {
		panic(fmt.Sprintf("failed to connect to NATS: %v", err))
	}
	js, err := jetstream.New(natsConn)
	if err != nil {
		panic(fmt.Sprintf("failed to create JetStream context: %v", err))
	}
	events = eventstest.Events{JS: js, Consumer: testConsumer}

	code := m.Run()

	natsConn.Close()
	_ = container.Terminate(ctx)
	os.Exit(code)
}

func publish(t *testing.T, payload string) {
	t.Helper()
	events.Publish(t, testSubject, []byte(payload))
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

func (r *recorder) all() []string {
	r.mu.Lock()
	defer r.mu.Unlock()
	return append([]string(nil), r.handled...)
}

func (r *recorder) count() int {
	r.mu.Lock()
	defer r.mu.Unlock()
	return len(r.handled)
}

func (r *recorder) waitFor(t *testing.T, payload string) {
	t.Helper()
	require.Eventually(t, func() bool { return r.has(payload) }, 15*time.Second, 50*time.Millisecond,
		"event %q should be handled", payload)
}

// newConsumer builds a consumer around handler and stops it when the test
// ends.
func newConsumer(t *testing.T, handler botevents.Handler) *botevents.Consumer {
	t.Helper()
	consumer, err := botevents.NewConsumer(natsConn, botevents.Config{
		Name:     testConsumer,
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
