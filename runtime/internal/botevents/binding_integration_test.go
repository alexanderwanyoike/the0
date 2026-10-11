package botevents_test

import (
	"context"
	"testing"

	"github.com/stretchr/testify/require"
)

func TestBinding_StartedBeforeTheStreamExists_HandlesEventsOnceItDoes(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	events.DeleteStream(t)
	handled := &recorder{}
	consumer := newConsumer(t, handled.handle)

	require.NoError(t, consumer.Start(context.Background()),
		"the runtime can start before the api has created the stream")

	events.CreateStream(t)
	publish(t, "first-event")
	handled.waitFor(t, "first-event")
}

func TestBinding_StreamRecreatedWhileRunning_KeepsHandlingEvents(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	events.DeleteStream(t)
	events.CreateStream(t)
	handled := &recorder{}
	consumer := newConsumer(t, handled.handle)
	require.NoError(t, consumer.Start(context.Background()))
	publish(t, "before-recreate")
	handled.waitFor(t, "before-recreate")

	events.DeleteStream(t)
	events.CreateStream(t)
	publish(t, "after-recreate")
	handled.waitFor(t, "after-recreate")
}
