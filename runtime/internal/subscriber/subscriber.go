// Package subscriber stores the Bots and schedules the api announces on
// THE0_EVENTS in the runtime's own MongoDB collections, from which the
// desired module decides what runs in both Docker and Kubernetes mode.
// Events are delivered durably and may arrive more than once, so every
// handler is safe to repeat.
package subscriber

import (
	"context"
	"fmt"
	"os"
	"sync"

	"github.com/nats-io/nats.go"
	"go.mongodb.org/mongo-driver/mongo"
	mongoOptions "go.mongodb.org/mongo-driver/mongo/options"

	"runtime/internal/botevents"
	"runtime/internal/util"
)

const (
	SubjectBotCreated = "the0.bot.created"
	SubjectBotUpdated = "the0.bot.updated"
	SubjectBotDeleted = "the0.bot.deleted"

	SubjectBotScheduleCreated = "the0.bot-schedule.created"
	SubjectBotScheduleUpdated = "the0.bot-schedule.updated"
	SubjectBotScheduleDeleted = "the0.bot-schedule.deleted"
)

// Each kind keeps the durable consumer it had before the bot and schedule
// subscribers were merged. JetStream keeps a consumer's position by name, so
// a new name would start a new consumer at its start window instead of where
// the running installation's consumer stopped.
const (
	BotConsumerName      = "runtime-bot-events"
	ScheduleConsumerName = "runtime-bot-schedule-events"
)

// Collection is a MongoDB collection the subscriber stores one kind in.
type Collection struct {
	DB   string
	Name string
}

type Options struct {
	NATSUrl string
	// Bots, when set, is where bot events are stored.
	Bots *Collection
	// Schedules, when set, is where schedule events are stored.
	Schedules *Collection
	Logger    util.Logger
}

type Subscriber struct {
	mongoClient *mongo.Client
	natsConn    *nats.Conn
	bots        *mongo.Collection
	schedules   *mongo.Collection
	consumers   []*botevents.Consumer
	logger      util.Logger
	stopOnce    sync.Once
	stopErr     error
}

// New connects to NATS and to the MongoDB that MONGO_URI names.
func New(ctx context.Context, opts Options) (*Subscriber, error) {
	if opts.Logger == nil {
		opts.Logger = &util.DefaultLogger{}
	}
	if opts.NATSUrl == "" {
		return nil, fmt.Errorf("NATS_URL environment variable is not set")
	}
	if opts.Bots == nil && opts.Schedules == nil {
		return nil, fmt.Errorf("subscriber stores neither bot nor schedule events")
	}

	mongoUri := os.Getenv("MONGO_URI")
	if mongoUri == "" {
		return nil, fmt.Errorf("MONGO_URI environment variable is not set")
	}
	mongoClient, err := mongo.Connect(ctx, mongoOptions.Client().ApplyURI(mongoUri))
	if err != nil {
		return nil, fmt.Errorf("failed to connect to MongoDB: %v", err)
	}
	if err := mongoClient.Ping(ctx, nil); err != nil {
		_ = mongoClient.Disconnect(context.Background())
		return nil, fmt.Errorf("failed to ping MongoDB: %v", err)
	}

	natsConn, err := botevents.Connect(opts.NATSUrl)
	if err != nil {
		_ = mongoClient.Disconnect(context.Background())
		return nil, fmt.Errorf("failed to connect to NATS: %v", err)
	}

	s := &Subscriber{mongoClient: mongoClient, natsConn: natsConn, logger: opts.Logger}
	if opts.Bots != nil {
		s.bots = mongoClient.Database(opts.Bots.DB).Collection(opts.Bots.Name)
	}
	if opts.Schedules != nil {
		s.schedules = mongoClient.Database(opts.Schedules.DB).Collection(opts.Schedules.Name)
	}
	return s, nil
}

// Start binds a durable consumer for each kind the subscriber stores and
// returns once each has handled its backlog (see botevents.Consumer.Start).
func (s *Subscriber) Start(ctx context.Context) error {
	s.logger.Info("Starting NATS Subscriber...")
	if s.bots != nil {
		if err := s.startConsumer(ctx, botevents.Config{
			Name:     BotConsumerName,
			Subjects: []string{SubjectBotCreated, SubjectBotUpdated, SubjectBotDeleted},
			Handler:  s.handleBot,
			Logger:   s.logger,
		}); err != nil {
			return err
		}
	}
	if s.schedules != nil {
		if err := s.startConsumer(ctx, botevents.Config{
			Name:     ScheduleConsumerName,
			Subjects: []string{SubjectBotScheduleCreated, SubjectBotScheduleUpdated, SubjectBotScheduleDeleted},
			Handler:  s.handleSchedule,
			Logger:   s.logger,
		}); err != nil {
			return err
		}
	}
	s.logger.Info("NATS Subscriber started successfully")
	return nil
}

func (s *Subscriber) startConsumer(ctx context.Context, cfg botevents.Config) error {
	consumer, err := botevents.NewConsumer(s.natsConn, cfg)
	if err != nil {
		return err
	}
	if err := consumer.Start(ctx); err != nil {
		return err
	}
	s.consumers = append(s.consumers, consumer)
	return nil
}

// IsReady reports whether every consumer is bound to THE0_EVENTS and
// receiving events.
func (s *Subscriber) IsReady() bool {
	if len(s.consumers) == 0 {
		return false
	}
	for _, consumer := range s.consumers {
		if !consumer.Ready() {
			return false
		}
	}
	return true
}

// Stop waits for the events being handled, then closes the connections. It
// is safe to call more than once.
func (s *Subscriber) Stop() error {
	s.stopOnce.Do(func() {
		s.logger.Info("Stopping NATS Subscriber")
		for _, consumer := range s.consumers {
			consumer.Stop()
		}
		if s.natsConn != nil {
			s.natsConn.Close()
		}
		if err := s.mongoClient.Disconnect(context.Background()); err != nil {
			s.stopErr = fmt.Errorf("failed to disconnect from MongoDB: %v", err)
			return
		}
		s.logger.Info("NATS Subscriber stopped successfully")
	})
	return s.stopErr
}
