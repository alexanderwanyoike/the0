// Package botevents consumes the bot and schedule lifecycle events the api
// publishes into the THE0_EVENTS JetStream stream.
package botevents

import (
	"context"
	"fmt"
	"runtime/internal/util"
	"sync"
	"sync/atomic"
	"time"

	"github.com/nats-io/nats.go"
	"github.com/nats-io/nats.go/jetstream"
)

// StreamName is the stream the api creates and owns; the runtime only binds
// consumers to it.
const StreamName = "THE0_EVENTS"

const (
	ackWait = time.Minute
	// Handlers give up well inside ackWait, so the server never redelivers an
	// event while its first attempt is still running.
	handlerTimeout = ackWait / 2
	// Lifecycle events are rare and dropping one (a delete especially) leaves
	// a bot in the wrong state, so a failing event is retried for about a
	// quarter of an hour, long enough to ride out a Mongo restart.
	maxDeliver = 20
)

// Handler applies one event. An error has the event redelivered later,
// unless it is Permanent.
type Handler func(ctx context.Context, subject string, data []byte) error

type Config struct {
	// Name of the durable consumer. Every runtime replica binds to the same
	// name, so replicas share one position in the stream and each event is
	// handled by one of them.
	Name     string
	Subjects []string
	Handler  Handler
	Logger   util.Logger
}

type Consumer struct {
	js       jetstream.JetStream
	cfg      Config
	consume  jetstream.ConsumeContext
	ready    atomic.Bool
	inflight sync.WaitGroup
	stopOnce sync.Once
}

func NewConsumer(nc *nats.Conn, cfg Config) (*Consumer, error) {
	js, err := jetstream.New(nc)
	if err != nil {
		return nil, fmt.Errorf("failed to create JetStream context: %w", err)
	}
	return &Consumer{js: js, cfg: cfg}, nil
}

func consumerConfig(cfg Config) jetstream.ConsumerConfig {
	return jetstream.ConsumerConfig{
		Durable:        cfg.Name,
		FilterSubjects: cfg.Subjects,
		// Only applies when the consumer is first created: it then replays what
		// the stream still holds, so nothing published before the runtime first
		// bound is lost. The handlers are safe to repeat.
		DeliverPolicy: jetstream.DeliverAllPolicy,
		AckPolicy:     jetstream.AckExplicitPolicy,
		AckWait:       ackWait,
		MaxDeliver:    maxDeliver,
		// One event in flight across all replicas: a retried event must not be
		// overtaken by a later one for the same bot, or a delete could land
		// before the create it follows and leave the bot running.
		MaxAckPending: 1,
	}
}

func (c *Consumer) Start(ctx context.Context) error {
	cons, err := c.js.CreateOrUpdateConsumer(ctx, StreamName, consumerConfig(c.cfg))
	if err != nil {
		return fmt.Errorf("failed to bind consumer %s to stream %s: %w", c.cfg.Name, StreamName, err)
	}
	cc, err := cons.Consume(c.handle, jetstream.ConsumeErrHandler(func(_ jetstream.ConsumeContext, err error) {
		c.cfg.Logger.Warn("Event consumer error", "consumer", c.cfg.Name, "error", err)
	}))
	if err != nil {
		return fmt.Errorf("failed to consume %s: %w", c.cfg.Name, err)
	}
	c.consume = cc
	c.ready.Store(true)
	c.cfg.Logger.Info("Consuming lifecycle events", "stream", StreamName, "consumer", c.cfg.Name, "subjects", c.cfg.Subjects)
	return nil
}

func (c *Consumer) Ready() bool {
	return c.ready.Load()
}

// Stop waits for the event being handled, if any. An event received but not
// yet handled stays unacknowledged and is redelivered after ackWait.
func (c *Consumer) Stop() {
	c.stopOnce.Do(func() {
		c.ready.Store(false)
		if c.consume != nil {
			c.consume.Stop()
			<-c.consume.Closed()
		}
		c.inflight.Wait()
	})
}

func (c *Consumer) handle(msg jetstream.Msg) {
	c.inflight.Add(1)
	defer c.inflight.Done()

	delivered := uint64(1)
	var sequence uint64
	if meta, err := msg.Metadata(); err == nil {
		delivered = meta.NumDelivered
		sequence = meta.Sequence.Stream
	}
	log := []interface{}{"consumer", c.cfg.Name, "subject", msg.Subject(), "sequence", sequence, "delivery", delivered}
	c.cfg.Logger.Info("Received lifecycle event", log...)

	ctx, cancel := context.WithTimeout(context.Background(), handlerTimeout)
	err := c.cfg.Handler(ctx, msg.Subject(), msg.Data())
	cancel()

	var settleErr error
	switch v, delay := settle(err, delivered); v {
	case verdictAck:
		c.cfg.Logger.Info("Handled lifecycle event", log...)
		settleErr = msg.Ack()
	case verdictRetry:
		c.cfg.Logger.Error("Failed to handle lifecycle event, will retry", append(log, "retry_in", delay.String(), "error", err)...)
		settleErr = msg.NakWithDelay(delay)
	case verdictDrop:
		c.cfg.Logger.Error("Dropping lifecycle event that cannot be handled", append(log, "error", err)...)
		settleErr = msg.Term()
	}
	if settleErr != nil {
		c.cfg.Logger.Error("Failed to settle lifecycle event", append(log, "error", settleErr)...)
	}
}
