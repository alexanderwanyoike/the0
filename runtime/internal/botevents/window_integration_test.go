package botevents_test

import (
	"context"
	"testing"
	"time"

	"runtime/internal/botevents"
	"runtime/internal/botevents/eventstest"

	"github.com/stretchr/testify/require"
)

// storedAt is when the stream stored the event at sequence seq.
func storedAt(t *testing.T, seq uint64) time.Time {
	t.Helper()
	stream, err := events.JS.Stream(context.Background(), eventstest.Stream.Name)
	require.NoError(t, err)
	msg, err := stream.GetMsg(context.Background(), seq)
	require.NoError(t, err)
	return msg.Time
}

func TestStartWindow_NewConsumerDeliversOnlyEventsInsideTheWindow(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	events.ResetStream(t)
	publish(t, "older-than-the-window")
	publish(t, "inside-the-window")
	older, inside := storedAt(t, 1), storedAt(t, 2)
	require.True(t, inside.After(older))
	windowStart := older.Add(inside.Sub(older) / 2)

	handled := &recorder{}
	consumer := newConsumer(t, handled.handle)
	botevents.SetClock(consumer, func() time.Time { return windowStart.Add(botevents.StartWindow) })
	require.NoError(t, consumer.Start(context.Background()))

	require.Equal(t, []string{"inside-the-window"}, handled.all(),
		"a new consumer must not walk back through history older than its window")
}

func TestStartWindow_ExistingConsumerResumesWhereItLeftOff(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	events.ResetStream(t)
	first := &recorder{}
	consumer := newConsumer(t, first.handle)
	require.NoError(t, consumer.Start(context.Background()))
	publish(t, "handled-before-restart")
	first.waitFor(t, "handled-before-restart")
	consumer.Stop()

	publish(t, "published-while-down")
	restarted := &recorder{}
	consumer = newConsumer(t, restarted.handle)
	botevents.SetClock(consumer, func() time.Time { return time.Now().Add(24 * time.Hour) })
	require.NoError(t, consumer.Start(context.Background()))

	require.Equal(t, []string{"published-while-down"}, restarted.all(),
		"a consumer that already exists keeps its position, whatever the window says")
}
