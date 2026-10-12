package botevents

import (
	"time"

	"github.com/nats-io/nats.go"
)

// Connect opens a NATS connection for a Consumer. It reconnects for as long
// as NATS is away: a connection that gives up ends consumption until the
// runtime restarts.
func Connect(url string) (*nats.Conn, error) {
	return nats.Connect(url, nats.MaxReconnects(-1), nats.ReconnectWait(time.Second))
}
