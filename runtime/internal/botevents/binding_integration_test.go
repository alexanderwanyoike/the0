package botevents

import (
	"context"
	"testing"

	"github.com/stretchr/testify/require"
)

func TestBinding_StartedBeforeTheStreamExists_HandlesEventsOnceItDoes(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	deleteEventsStream(t)
	events := &recorder{}
	consumer := newConsumer(t, events.handle)

	require.NoError(t, consumer.Start(context.Background()),
		"the runtime can start before the api has created the stream")

	createEventsStream(t)
	publish(t, "first-event")
	events.waitFor(t, "first-event")
}

func TestBinding_StreamRecreatedWhileRunning_KeepsHandlingEvents(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	deleteEventsStream(t)
	createEventsStream(t)
	events := &recorder{}
	consumer := newConsumer(t, events.handle)
	require.NoError(t, consumer.Start(context.Background()))
	publish(t, "before-recreate")
	events.waitFor(t, "before-recreate")

	deleteEventsStream(t)
	createEventsStream(t)
	publish(t, "after-recreate")
	events.waitFor(t, "after-recreate")
}
