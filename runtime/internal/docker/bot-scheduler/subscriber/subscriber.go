package subscriber

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"runtime/internal/botevents"
	"runtime/internal/constants"
	"runtime/internal/desired"
	"runtime/internal/model"
	"runtime/internal/util"
	"strconv"
	"sync"
	"time"

	"github.com/nats-io/nats.go"
	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/mongo"
	mongoOptions "go.mongodb.org/mongo-driver/mongo/options"
)

const (
	SubjectBotScheduleCreated = "the0.bot-schedule.created"
	SubjectBotScheduleUpdated = "the0.bot-schedule.updated"
	SubjectBotScheduleDeleted = "the0.bot-schedule.deleted"
)

// ConsumerName names this subscriber's durable consumer (botevents.Config.Name).
const ConsumerName = "runtime-bot-schedule-events"

type BotScheduleMessage struct {
	ID     string                 `json:"id"`
	Config map[string]interface{} `json:"config"`
	Custom map[string]interface{} `json:"custom"`
}

// Bot schedule event structure - matches the format from API service
type BotScheduleEvent struct {
	ID     string                       `json:"id"`
	Config map[string]interface{}       `json:"config"`
	Custom CustomBotScheduleVersionData `json:"custom"`
}

type CustomBotScheduleVersionData struct {
	Config    model.APIBotConfig `json:"config"`
	CreatedAt time.Time          `json:"createdAt"`
	UpdatedAt time.Time          `json:"updatedAt"`
	FilePath  string             `json:"filePath"`
	Version   string             `json:"version"`
}

type SubscriberOptions struct {
	NATSUrl        string
	DBName         string
	CollectionName string
	MaxRetries     int
	Logger         util.Logger
}

func DefaultOptions() SubscriberOptions {
	return SubscriberOptions{
		NATSUrl:        os.Getenv("NATS_URL"),
		DBName:         constants.BOT_SCHEDULER_DB_NAME,
		CollectionName: constants.BOT_SCHEDULE_COLLECTION,
		MaxRetries:     getDefaultMaxRetries(),
		Logger:         &util.DefaultLogger{},
	}
}

func getDefaultMaxRetries() int {
	maxRetries := os.Getenv("MAX_RETRIES")
	if maxRetries == "" {
		return 3
	}

	retries, err := strconv.Atoi(maxRetries)
	if err != nil {
		return 3
	}

	return retries
}

type Subscriber interface {
	Start(ctx context.Context) error
	Stop() error
}

type NATSSubscriber struct {
	mongoClient    *mongo.Client
	natsConn       *nats.Conn
	dbName         string
	collectionName string
	maxRetries     int
	events         *botevents.Consumer
	stopChan       chan struct{}
	wg             sync.WaitGroup
	logger         util.Logger
}

func NewNATSSubscriber(
	ctx context.Context,
	options ...SubscriberOptions,
) (*NATSSubscriber, error) {
	// Set the default options
	opts := DefaultOptions()
	if len(options) > 0 {
		opts = options[0]
		if opts.Logger == nil {
			opts.Logger = &util.DefaultLogger{}
		}

		if opts.MaxRetries <= 0 {
			opts.MaxRetries = getDefaultMaxRetries()
		}
	}

	// Validate NATS URL
	if opts.NATSUrl == "" {
		return nil, fmt.Errorf("NATS_URL environment variable is not set")
	}

	// Get MongoDB URI from environment variable
	mongoUri := os.Getenv("MONGO_URI")
	if mongoUri == "" {
		return nil, fmt.Errorf("MONGO_URI environment variable is not set")
	}

	mongoClient, err := mongo.Connect(ctx, mongoOptions.Client().ApplyURI(mongoUri))
	if err != nil {
		return nil, fmt.Errorf("failed to connect to MongoDB: %v", err)
	}

	// Verify MongoDB connection
	if err := mongoClient.Ping(ctx, nil); err != nil {
		return nil, fmt.Errorf("failed to ping MongoDB: %v", err)
	}

	// Create NATS connection
	natsConn, err := botevents.Connect(opts.NATSUrl)
	if err != nil {
		return nil, fmt.Errorf("failed to connect to NATS: %v", err)
	}

	return &NATSSubscriber{
		mongoClient:    mongoClient,
		natsConn:       natsConn,
		dbName:         opts.DBName,
		collectionName: opts.CollectionName,
		maxRetries:     opts.MaxRetries,
		stopChan:       make(chan struct{}),
		wg:             sync.WaitGroup{},
		logger:         opts.Logger,
	}, nil
}

func (s *NATSSubscriber) Start(ctx context.Context) error {
	s.logger.Info("Starting NATS Subscriber...")

	events, err := botevents.NewConsumer(s.natsConn, botevents.Config{
		Name:     ConsumerName,
		Subjects: []string{SubjectBotScheduleCreated, SubjectBotScheduleUpdated, SubjectBotScheduleDeleted},
		Handler:  s.handle,
		Logger:   s.logger,
	})
	if err != nil {
		return err
	}
	if err := events.Start(ctx); err != nil {
		return err
	}
	s.events = events

	s.logger.Info("NATS Subscriber started successfully")
	return nil
}

func (s *NATSSubscriber) handle(ctx context.Context, subject string, data []byte) error {
	switch subject {
	case SubjectBotScheduleCreated:
		return s.handleCreate(ctx, data)
	case SubjectBotScheduleUpdated:
		return s.handleUpdate(ctx, data)
	case SubjectBotScheduleDeleted:
		return s.handleDelete(ctx, data)
	default:
		return botevents.Permanent(fmt.Errorf("unknown subject: %s", subject))
	}
}

// IsReady reports whether the subscriber is bound to THE0_EVENTS and
// receiving events.
func (s *NATSSubscriber) IsReady() bool {
	return s.events != nil && s.events.Ready()
}

func (s *NATSSubscriber) Stop() error {
	s.logger.Info("Stopping NATS Subscriber")

	// Stop all message processing
	close(s.stopChan)

	// Wait for all goroutines to finish
	s.wg.Wait()

	if s.events != nil {
		s.events.Stop()
	}

	// Close NATS connection
	if s.natsConn != nil {
		s.natsConn.Close()
	}

	// Disconnect from MongoDB
	if err := s.mongoClient.Disconnect(context.Background()); err != nil {
		return fmt.Errorf("failed to disconnect from MongoDB: %v", err)
	}

	s.logger.Info("NATS Subscriber stopped successfully")
	return nil
}

func (s *NATSSubscriber) parseEventMessage(data []byte) (*BotScheduleMessage, error) {
	var event BotScheduleEvent
	if err := json.Unmarshal(data, &event); err != nil {
		return nil, fmt.Errorf("failed to unmarshal event: %v", err)
	}

	// Convert event data to BotScheduleMessage format
	botScheduleMessage := &BotScheduleMessage{
		ID:     event.ID,
		Config: event.Config,
		Custom: map[string]interface{}{
			"config":    event.Custom.Config,
			"filePath":  event.Custom.FilePath,
			"createdAt": event.Custom.CreatedAt.Format(time.RFC3339),
			"updatedAt": event.Custom.UpdatedAt.Format(time.RFC3339),
			"version":   event.Custom.Version,
		},
	}

	return botScheduleMessage, nil
}

func (s *NATSSubscriber) handleCreate(ctx context.Context, data []byte) error {
	message, err := s.parseEventMessage(data)
	if err != nil {
		return botevents.Permanent(fmt.Errorf("failed to parse message: %v", err))
	}

	collection := s.mongoClient.Database(s.dbName).Collection(s.collectionName)
	filter := bson.M{"id": message.ID}
	count, err := collection.CountDocuments(ctx, filter)

	if err != nil {
		return fmt.Errorf("failed to check if bot schedule exists: %v", err)
	}

	if count > 0 {
		s.logger.Info("Bot schedule already exists, skipping creation", "id", message.ID)
		return nil
	}

	return s.upsertBotSchedule(ctx, collection, *message)
}

func (s *NATSSubscriber) handleUpdate(ctx context.Context, data []byte) error {
	message, err := s.parseEventMessage(data)
	if err != nil {
		return botevents.Permanent(fmt.Errorf("failed to parse message: %v", err))
	}

	collection := s.mongoClient.Database(s.dbName).Collection(s.collectionName)
	filter := bson.M{"id": message.ID}
	var existingBot model.BotSchedule
	err = collection.FindOne(ctx, filter).Decode(&existingBot)
	if err != nil {
		if err == mongo.ErrNoDocuments {
			// If not found, robust upsert: create a new bot schedule
			s.logger.Info("Bot schedule not found, creating new one", "id", message.ID)
			return s.upsertBotSchedule(ctx, collection, *message)
		}
		return fmt.Errorf("failed to find bot schedule: %v", err)
	}

	// Prepare update document
	update := bson.M{
		"$set": bson.M{
			"config": message.Config,
			"custom": message.Custom,
		},
	}

	// Recalculate next execution time if the schedule is updated
	existingSchedule, existingOk := existingBot.Config["schedule"].(string)
	newSchedule, newOk := message.Config["schedule"].(string)
	if !newOk || newSchedule == "" {
		return botevents.Permanent(fmt.Errorf("missing or invalid schedule in message config"))
	}
	if !existingOk || existingSchedule != newSchedule {
		nextTime, err := desired.NextExecutionTime(
			newSchedule,
			time.Now(),
		)
		if err != nil {
			return botevents.Permanent(fmt.Errorf("failed to calculate next execution time: %v", err))
		}
		update["$set"].(bson.M)["next_execution_time"] = nextTime.Unix()
	}

	result, err := collection.UpdateOne(ctx, filter, update)
	if err != nil {
		return fmt.Errorf("failed to update bot schedule: %v", err)
	}
	if result.MatchedCount == 0 {
		return fmt.Errorf("no bot schedule found with ID: %s", message.ID)
	}

	s.logger.Info("Updated bot schedule", "id", message.ID)
	return nil
}

func (s *NATSSubscriber) handleDelete(ctx context.Context, data []byte) error {
	message, err := s.parseEventMessage(data)
	if err != nil {
		return botevents.Permanent(fmt.Errorf("failed to parse message: %v", err))
	}

	collection := s.mongoClient.Database(s.dbName).Collection(s.collectionName)
	filter := bson.M{"id": message.ID}

	result, err := collection.DeleteOne(ctx, filter)
	if err != nil {
		return fmt.Errorf("failed to delete bot schedule: %v", err)
	}
	if result.DeletedCount == 0 {
		s.logger.Info("Bot schedule not found, nothing to delete", "id", message.ID)
		return nil
	}
	s.logger.Info("Deleted bot schedule from collection", "id", message.ID)

	return nil
}

// upsertBotSchedule inserts or updates a bot schedule in the collection.
func (s *NATSSubscriber) upsertBotSchedule(
	ctx context.Context,
	collection *mongo.Collection,
	message BotScheduleMessage,
) error {
	schedule, ok := message.Config["schedule"].(string)
	if !ok || schedule == "" {
		return botevents.Permanent(fmt.Errorf("missing or invalid schedule in message config"))
	}

	nextTime, err := desired.NextExecutionTime(
		schedule,
		time.Now(),
	)
	if err != nil {
		return botevents.Permanent(fmt.Errorf("failed to calculate next execution time: %v", err))
	}

	// Convert entire Custom map to CustomBotVersion struct using JSON marshaling
	var customBotVersion model.CustomBotVersion
	s.logger.Info("DEBUG: Converting Custom to CustomBotVersion", "custom_keys", getMapKeys(message.Custom))

	// Marshal the entire Custom map and unmarshal it into the struct
	customBytes, err := json.Marshal(message.Custom)
	if err != nil {
		s.logger.Error("DEBUG: Failed to marshal entire Custom map", "error", err)
		customBotVersion = model.CustomBotVersion{} // Use empty struct as fallback
	} else {
		err = json.Unmarshal(customBytes, &customBotVersion)
		if err != nil {
			s.logger.Error("DEBUG: Failed to unmarshal Custom map to struct", "error", err)
			customBotVersion = model.CustomBotVersion{} // Use empty struct as fallback
		} else {
			s.logger.Info("DEBUG: Successfully converted Custom map", "runtime", customBotVersion.Config.Runtime, "version", customBotVersion.Version)
		}
	}

	botSchedule := model.BotSchedule{
		ID:                message.ID,
		NextExecutionTime: nextTime.Unix(),
		Config:            message.Config,
		CustomBotVersion:  customBotVersion,
	}

	opts := mongoOptions.Update().SetUpsert(true)
	filter := bson.M{"id": botSchedule.ID}
	update := bson.M{"$set": botSchedule}

	_, err = collection.UpdateOne(ctx, filter, update, opts)
	if err != nil {
		return fmt.Errorf("failed to insert or update bot schedule: %v", err)
	}

	s.logger.Info("Upserted bot schedule", "id", botSchedule.ID)
	return nil
}

// Helper function to get map keys for debugging
func getMapKeys(m map[string]interface{}) []string {
	keys := make([]string, 0, len(m))
	for k := range m {
		keys = append(keys, k)
	}
	return keys
}

type MockSubscriber struct {
	// Mocked methods and fields for testing
	SubscribeCalled bool
}

func (m *MockSubscriber) Start(ctx context.Context) error {
	m.SubscribeCalled = true
	return nil
}

func (m *MockSubscriber) Stop() error {
	m.SubscribeCalled = false
	return nil
}
