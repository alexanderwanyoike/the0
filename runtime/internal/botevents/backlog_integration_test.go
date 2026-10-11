package botevents

import (
	"context"
	"fmt"
	"testing"

	"github.com/stretchr/testify/require"
)

func TestBacklog_StartReturnsOnceTheBacklogIsHandled(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}
	deleteEventsStream(t)
	createEventsStream(t)
	const backlog = 50
	for i := range backlog {
		publish(t, fmt.Sprintf("event-%d", i))
	}
	events := &recorder{}
	consumer := newConsumer(t, events.handle)

	require.NoError(t, consumer.Start(context.Background()))

	require.Equal(t, backlog, events.count(),
		"the first reconcile after Start must not see a half-applied backlog")
}
