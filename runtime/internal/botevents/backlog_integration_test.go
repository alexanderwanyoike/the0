package botevents_test

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
	events.DeleteStream(t)
	events.CreateStream(t)
	const backlog = 50
	for i := range backlog {
		publish(t, fmt.Sprintf("event-%d", i))
	}
	handled := &recorder{}
	consumer := newConsumer(t, handled.handle)

	require.NoError(t, consumer.Start(context.Background()))

	require.Equal(t, backlog, handled.count(),
		"the first reconcile after Start must not see a half-applied backlog")
}
