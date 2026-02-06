package natssubscriber

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"runtime/internal/model"
	"runtime/internal/constants"
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
	SubjectBotCreated = "the0.bot.created"
	SubjectBotUpdated = "the0.bot.updated"
	SubjectBotDeleted = "the0.bot.deleted"
)

type BotMessage struct {
	ID               string                 `json:"id"`
	Config           map[string]interface{} `json:"config"`
	CustomBotVersion model.CustomBotVersion `json:"custom"`
}

// NATS event structure - matches the format from API service
type BotEvent struct {
	Type      string       `json:"type"`
	EntityID  string       `json:"entity_id"`
	Data      BotEventData `json:"data"`
	Timestamp string       `json:"timestamp"`
}

type BotEventData struct {
	ID        string                 `json:"id"`
	Config    map[string]interface{} `json:"config"`
	Custom    CustomBotVersionData   `json:"custom"`
	TopicName string                 `json:"topicName,omitempty"`
}

type CustomBotVersionData struct {
	Config    model.APIBotConfig `json:"config"`
	CreatedAt time.Time          `json:"createdAt"`
	UpdatedAt time.Time          `json:"updatedAt"`
	FilePath  string             `json:"filePath"`
	Version   string             `json:"version"`
}

type SubscriberOptions struct {
	NATSUrl            string
	DBName             string
	CollectionName     string
	MaxRetries         int
	MaxBotPerPartition int
	Logger             util.Logger
}

func DefaultOptions() SubscriberOptions {
	return SubscriberOptions{
		NATSUrl:            os.Getenv("NATS_URL"),
		DBName:             constants.BOT_RUNNER_DB_NAME,
		CollectionName:     constants.BOT_RUNNER_COLLECTION,
		MaxBotPerPartition: getDefaultMaxBotsPerPartition(),
		MaxRetries:         getDefaultMaxRetries(),
		Logger:             &util.DefaultLogger{},
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

func getDefaultMaxBotsPerPartition() int {
	maxBotsPerPartition := os.Getenv("MAX_BOTS_PER_PARTITION")
	if maxBotsPerPartition == "" {
		return 10
	}

	maxBots, err := strconv.Atoi(maxBotsPerPartition)
	if err != nil {
		return 10
	}

	return maxBots
}

type Subscriber interface {
	Start(ctx context.Context) error
	Stop() error
}

type NATSSubscriber struct {
	mongoClient         *mongo.Client
	natsConn            *nats.Conn
	dbName              string
	collectionName      string
	maxRetries          int
	maxBotsPerPartition int
	subscriptions       []*nats.Subscription
	stopChan            chan struct{}
	wg                  sync.WaitGroup
	logger              util.Logger
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
	natsConn, err := nats.Connect(opts.NATSUrl)
	if err != nil {
		return nil, fmt.Errorf("failed to connect to NATS: %v", err)
	}

	return &NATSSubscriber{
		mongoClient:         mongoClient,
		natsConn:            natsConn,
		dbName:              opts.DBName,
		collectionName:      opts.CollectionName,
		maxRetries:          opts.MaxRetries,
		maxBotsPerPartition: opts.MaxBotPerPartition,
		subscriptions:       make([]*nats.Subscription, 0),
		stopChan:            make(chan struct{}),
		wg:                  sync.WaitGroup{},
		logger:              opts.Logger,
	}, nil
}

func (s *NATSSubscriber) Start(ctx context.Context) error {
	s.logger.Info("Starting NATS Subscriber...")

	subjects := []string{
		SubjectBotCreated,
		SubjectBotUpdated,
		SubjectBotDeleted,
	}

	for _, subject := range subjects {
		if err := s.setupSubscription(ctx, subject); err != nil {
			return err
		}
	}

	// Flush to ensure subscriptions are active on server before returning
	if err := s.natsConn.Flush(); err != nil {
		return fmt.Errorf("failed to flush NATS connection: %v", err)
	}

	s.logger.Info("NATS Subscriber started successfully")
	return nil
}

func (s *NATSSubscriber) setupSubscription(ctx context.Context, subject string) error {
	// Create queue subscription for load balancing between multiple instances
	queueGroup := "runtime"

	sub, err := s.natsConn.QueueSubscribe(subject, queueGroup, func(msg *nats.Msg) {
		s.logger.Info("Received message", "subject", subject, "data", string(msg.Data))

		// Process the message based on the subject
		var err error
		switch subject {
		case SubjectBotCreated:
			err = s.handleCreate(ctx, msg)
		case SubjectBotUpdated:
			err = s.handleUpdate(ctx, msg)
		case SubjectBotDeleted:
			err = s.handleDelete(ctx, msg)
		default:
			err = fmt.Errorf("unknown subject: %s", subject)
		}

		if err != nil {
			s.logger.Error("Failed to process message", "subject", subject, "error", err)
			// In NATS, we can implement retry logic here or send to a dead letter queue
			// For now, we'll just log the error and continue
			return
		}

		s.logger.Info("Message processed successfully", "subject", subject)
	})

	if err != nil {
		return fmt.Errorf("failed to subscribe to subject %s: %v", subject, err)
	}

	s.subscriptions = append(s.subscriptions, sub)
	s.logger.Info("Subscribed to subject", "subject", subject, "queue_group", queueGroup)

	return nil
}

func (s *NATSSubscriber) Stop() error {
	s.logger.Info("Stopping NATS Subscriber")

	// Stop all message processing
	close(s.stopChan)

	// Wait for all goroutines to finish
	s.wg.Wait()

	// Unsubscribe from all subjects
	for _, sub := range s.subscriptions {
		if err := sub.Unsubscribe(); err != nil {
			s.logger.Error("Failed to unsubscribe", "subscription", sub.Subject, "error", err)
		}
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

func (s *NATSSubscriber) parseEventMessage(data []byte) (*BotMessage, error) {
	// Try to parse as direct BotMessage format first (current API format)
	var botMessage BotMessage
	if err := json.Unmarshal(data, &botMessage); err == nil {
		// Successfully parsed as direct format, check if it has data
		s.logger.Info("Parsed direct BotMessage format", "id", botMessage.ID, "config_exists", botMessage.Config != nil)
		if botMessage.ID != "" {
			return &botMessage, nil
		}
		s.logger.Info("Direct format parsed but ID is empty, trying wrapped format")
	} else {
		s.logger.Info("Failed to parse as direct format", "error", err)
	}

	// Fall back to wrapped BotEvent format for compatibility
	var event BotEvent
	if err := json.Unmarshal(data, &event); err != nil {
		return nil, fmt.Errorf("failed to unmarshal event in both direct and wrapped formats: %v", err)
	}

	// Convert event data to BotMessage format
	botMessage = BotMessage{
		ID:     event.Data.ID,
		Config: event.Data.Config,
		CustomBotVersion: model.CustomBotVersion{
			Config:    event.Data.Custom.Config,
			FilePath:  event.Data.Custom.FilePath,
			CreatedAt: event.Data.Custom.CreatedAt.Format(time.RFC3339),
			UpdatedAt: event.Data.Custom.UpdatedAt.Format(time.RFC3339),
			Version:   event.Data.Custom.Version,
		},
	}

	return &botMessage, nil
}

func (s *NATSSubscriber) getNextSequenceValue(ctx context.Context, sequenceName string) (int32, error) {
	collection := s.mongoClient.Database(s.dbName).Collection("counters")
	filter := bson.M{"_id": sequenceName}
	update := bson.M{"$inc": bson.M{"seq": 1}}
	opts := mongoOptions.FindOneAndUpdate().SetUpsert(true).SetReturnDocument(mongoOptions.After)

	var counter struct {
		Seq int32 `bson:"seq"`
	}

	err := collection.FindOneAndUpdate(ctx, filter, update, opts).Decode(&counter)
	if err != nil {
		return 0, fmt.Errorf("failed to get next sequence value: %v", err)
	}

	return counter.Seq, nil
}

func (s *NATSSubscriber) findOrCreatePartition(ctx context.Context) (int32, error) {
	partitionsCollection := s.mongoClient.Database(s.dbName).Collection("partitions")

	// Step 1: Atomically find a partition with space and claim it
	filter := bson.M{"bot_count": bson.M{"$lt": s.maxBotsPerPartition}}
	update := bson.M{"$inc": bson.M{"bot_count": 1}}
	opts := mongoOptions.FindOneAndUpdate().SetReturnDocument(mongoOptions.After)

	var partition struct {
		ID int32 `bson:"_id"`
	}

	err := partitionsCollection.FindOneAndUpdate(ctx, filter, update, opts).Decode(&partition)

	if err == nil {
		s.logger.Info("Found existing partition", "partition_id", partition.ID)
		return partition.ID, nil
	}

	if err != mongo.ErrNoDocuments {
		return 0, fmt.Errorf("failed to find or create partition: %w", err)
	}

	// Step 2: If no partition found, create a new one
	s.logger.Info("No partition found, creating a new one")
	newPartitionID, err := s.getNextSequenceValue(ctx, "partition_id")
	if err != nil {
		return 0, err
	}
	newPartition := bson.M{"_id": newPartitionID, "bot_count": 1}
	_, err = partitionsCollection.InsertOne(ctx, newPartition)
	if err != nil {
		// This can happen in a race condition. A robust implementation would retry
		s.logger.Error("Failed to create new partition, retrying", "error", err)
		return 0, fmt.Errorf("failed to create new partition: %w", err)
	}

	s.logger.Info("Created new partition", "partition_id", newPartitionID)
	return newPartitionID, nil
}

func (s *NATSSubscriber) handleCreate(ctx context.Context, msg *nats.Msg) error {
	message, err := s.parseEventMessage(msg.Data)
	if err != nil {
		return fmt.Errorf("failed to parse message: %v", err)
	}

	// Debug logging to see what we actually parsed
	s.logger.Info("DEBUG handleCreate: Parsed message", "id", message.ID, "config_nil", message.Config == nil, "custom_version", message.CustomBotVersion.Version, "custom_runtime", message.CustomBotVersion.Config.Runtime)

	collection := s.mongoClient.Database(s.dbName).Collection(s.collectionName)
	filter := bson.M{"id": message.ID}
	count, err := collection.CountDocuments(ctx, filter)

	if err != nil {
		return fmt.Errorf("failed to check if bot exists: %v", err)
	}

	if count > 0 {
		s.logger.Info("Bot already exists, skipping creation", "id", message.ID)
		return nil
	}

	segmentId, err := s.findOrCreatePartition(ctx)
	if err != nil {
		return fmt.Errorf("failed to find or create partition to bot %s: %v", message.ID, err)
	}

	return s.upsertBot(ctx, collection, *message, segmentId)
}

func (s *NATSSubscriber) handleUpdate(ctx context.Context, msg *nats.Msg) error {
	message, err := s.parseEventMessage(msg.Data)
	if err != nil {
		return fmt.Errorf("failed to parse message: %v", err)
	}

	collection := s.mongoClient.Database(s.dbName).Collection(s.collectionName)
	filter := bson.M{"id": message.ID}
	var existingBot model.Bot
	err = collection.FindOne(ctx, filter).Decode(&existingBot)
	if err != nil {
		if err == mongo.ErrNoDocuments {
			// If not found, robust upsert: create a new bot
			s.logger.Info("Bot not found, creating new bot", "id", message.ID)
			segmentId, err := s.findOrCreatePartition(ctx)
			if err != nil {
				return fmt.Errorf("failed to find or create partition during upsert for bot %s: %w", message.ID, err)
			}
			return s.upsertBot(ctx, collection, *message, segmentId)
		}
		return fmt.Errorf("failed to find bot: %v", err)
	}

	// Prepare update document
	update := bson.M{
		"$set": bson.M{
			"config": message.Config,
			"custom": message.CustomBotVersion,
		},
	}

	_, err = collection.UpdateOne(ctx, filter, update)
	if err != nil {
		return fmt.Errorf("failed to update bot: %v", err)
	}

	s.logger.Info("Updated bot", "id", message.ID)
	return nil
}

// upsertBot inserts or updates a bot in the collection.
func (s *NATSSubscriber) upsertBot(
	ctx context.Context,
	collection *mongo.Collection,
	message BotMessage,
	segmentID int32,
) error {
	bot := model.Bot{
		ID:               message.ID,
		SegmentId:        segmentID,
		Config:           message.Config,
		CustomBotVersion: message.CustomBotVersion,
	}

	opts := mongoOptions.Update().SetUpsert(true)
	filter := bson.M{"id": bot.ID}
	update := bson.M{"$set": bot}

	_, err := collection.UpdateOne(ctx, filter, update, opts)
	if err != nil {
		return fmt.Errorf("failed to insert or update bot: %v", err)
	}

	s.logger.Info("Upserted bot", "id", bot.ID, "segment_id", bot.SegmentId)
	return nil
}

func (s *NATSSubscriber) handleDelete(ctx context.Context, msg *nats.Msg) error {
	message, err := s.parseEventMessage(msg.Data)
	if err != nil {
		return fmt.Errorf("failed to parse message: %v", err)
	}

	collection := s.mongoClient.Database(s.dbName).Collection(s.collectionName)
	filter := bson.M{"id": message.ID}

	var botToDelete model.Bot
	err = collection.FindOne(ctx, filter).Decode(&botToDelete)
	if err != nil {
		if err == mongo.ErrNoDocuments {
			s.logger.Info("Bot not found, nothing to delete", "id", message.ID)
			return nil // No action needed if bot does not exist
		}
		return fmt.Errorf("failed to find bot: %v", err)
	}

	_, err = collection.DeleteOne(ctx, filter)
	if err != nil {
		return fmt.Errorf("failed to delete bot: %v", err)
	}
	s.logger.Info("Deleted bot from collection", "id", message.ID)

	partitionsCollection := s.mongoClient.Database(s.dbName).Collection("partitions")
	partitionFilter := bson.M{"_id": botToDelete.SegmentId}
	update := bson.M{"$inc": bson.M{"bot_count": -1}}
	_, err = partitionsCollection.UpdateOne(ctx, partitionFilter, update)
	if err != nil {
		s.logger.Error("CRITICAL: Failed to update partition count after bot deletion.", "partition_id", botToDelete.SegmentId, "error", err)
		return fmt.Errorf("failed to update partition count after bot deletion: %w", err)
	}
	s.logger.Info("Updated partition count after bot deletion", "partition_id", botToDelete.SegmentId)

	return nil
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
