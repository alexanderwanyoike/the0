// Package botevents consumes the bot and schedule lifecycle events the api
// publishes into the THE0_EVENTS JetStream stream.
package botevents

import (
	"context"
	"errors"
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
	maxDeliver     = 20
	rebindInterval = 2 * time.Second
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
	ready    atomic.Bool
	inflight sync.WaitGroup
	stop     chan struct{}
	done     chan struct{}
	stopOnce sync.Once
}

func NewConsumer(nc *nats.Conn, cfg Config) (*Consumer, error) {
	js, err := jetstream.New(nc)
	if err != nil {
		return nil, fmt.Errorf("failed to create JetStream context: %w", err)
	}
	return &Consumer{js: js, cfg: cfg, stop: make(chan struct{})}, nil
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

// The runtime can start before the api has created the stream (Docker
// Compose only waits for NATS to be healthy), so a missing stream is not an
// error: the consumer binds once it exists.
func (c *Consumer) Start(ctx context.Context) error {
	cons, err := c.bind(ctx)
	if err != nil && !errors.Is(err, jetstream.ErrStreamNotFound) {
		return fmt.Errorf("failed to bind consumer %s to stream %s: %w", c.cfg.Name, StreamName, err)
	}
	if err != nil {
		c.cfg.Logger.Warn("Event stream not created yet, binding once the api creates it",
			"stream", StreamName, "consumer", c.cfg.Name)
	}
	c.done = make(chan struct{})
	go c.run(cons)
	return nil
}

func (c *Consumer) Ready() bool {
	return c.ready.Load()
}

// Stop waits for the event being handled, if any. An event received but not
// yet handled stays unacknowledged and is redelivered after ackWait.
func (c *Consumer) Stop() {
	c.stopOnce.Do(func() {
		close(c.stop)
		if c.done != nil {
			<-c.done
		}
		c.inflight.Wait()
	})
}

func (c *Consumer) bind(ctx context.Context) (jetstream.Consumer, error) {
	return c.js.CreateOrUpdateConsumer(ctx, StreamName, consumerConfig(c.cfg))
}

func (c *Consumer) run(cons jetstream.Consumer) {
	defer close(c.done)
	for {
		if cons != nil {
			c.consume(cons)
		}
		select {
		case <-c.stop:
			return
		case <-time.After(rebindInterval):
		}
		var err error
		if cons, err = c.bind(context.Background()); err != nil {
			c.cfg.Logger.Warn("Failed to bind event consumer, will try again",
				"stream", StreamName, "consumer", c.cfg.Name, "retry_in", rebindInterval.String(), "error", err)
		}
	}
}

// The consume context also closes on its own when the consumer is deleted
// (for example with its stream), which sends run back to binding.
func (c *Consumer) consume(cons jetstream.Consumer) {
	cc, err := cons.Consume(c.handle, jetstream.ConsumeErrHandler(func(_ jetstream.ConsumeContext, err error) {
		c.cfg.Logger.Warn("Event consumer error", "consumer", c.cfg.Name, "error", err)
	}))
	if err != nil {
		c.cfg.Logger.Error("Failed to consume events", "consumer", c.cfg.Name, "error", err)
		return
	}
	c.ready.Store(true)
	c.cfg.Logger.Info("Consuming lifecycle events", "stream", StreamName, "consumer", c.cfg.Name, "subjects", c.cfg.Subjects)
	select {
	case <-c.stop:
	case <-cc.Closed():
	}
	c.ready.Store(false)
	cc.Stop()
	<-cc.Closed()
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
