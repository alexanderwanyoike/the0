package botevents_test

import (
	"context"
	"errors"
	"sync/atomic"
	"testing"
	"time"

	"runtime/internal/botevents"

	"github.com/stretchr/testify/require"
)

// flaky fails its first failures attempts, as a handler does while Mongo is
// down, and then records the event.
type flaky struct {
	recorder
	failures int32
	attempts atomic.Int32
}

func (f *flaky) handle(ctx context.Context, subject string, data []byte) error {
	if f.attempts.Add(1) <= f.failures {
		return errors.New("mongo unavailable")
	}
	return f.recorder.handle(ctx, subject, data)
}

func TestRetry_EventFailingMoreThanTwentyTimes_IsStillApplied(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	events.ResetStream(t)
	handler := &flaky{failures: 25}
	consumer := newConsumer(t, handler.handle)
	botevents.SetRetryBackoff(consumer, time.Millisecond, 10*time.Millisecond)
	require.NoError(t, consumer.Start(context.Background()))

	publish(t, "keeps-failing")

	handler.waitFor(t, "keeps-failing")
}
