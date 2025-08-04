package subscriber

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"runtime/internal/bot-scheduler/model"
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
	SubjectBotScheduleCreated = "the0.bot-schedule.created"
	SubjectBotScheduleUpdated = "the0.bot-schedule.updated"
	SubjectBotScheduleDeleted = "the0.bot-schedule.deleted"
)

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
	NATSUrl                    string
	DBName                     string
	CollectionName             string
	MaxRetries                 int
	MaxBotSchedulePerPartition int
	Logger                     util.Logger
}

func DefaultOptions() SubscriberOptions {
	return SubscriberOptions{
		NATSUrl:                    os.Getenv("NATS_URL"),
		DBName:                     constants.BOT_SCHEDULER_DB_NAME,
		CollectionName:             constants.BOT_SCHEDULE_COLLECTION,
		MaxBotSchedulePerPartition: getDefaultMaxBotSchedulePerPartition(),
		MaxRetries:                 getDefaultMaxRetries(),
		Logger:                     &util.DefaultLogger{},
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

func getDefaultMaxBotSchedulePerPartition() int {
	maxBotSchedulePerPartition := os.Getenv("MAX_BOT_SCHEDULE_PER_PARTITION")
	if maxBotSchedulePerPartition == "" {
		return 10
	}

	maxBotSchedule, err := strconv.Atoi(maxBotSchedulePerPartition)
	if err != nil {
		return 10
	}

	return maxBotSchedule
}

type Subscriber interface {
	Start(ctx context.Context) error
	Stop() error
}

type NATSSubscriber struct {
	mongoClient                *mongo.Client
	natsConn                   *nats.Conn
	dbName                     string
	collectionName             string
	maxRetries                 int
	maxBotSchedulePerPartition int
	subscriptions              []*nats.Subscription
	stopChan                   chan struct{}
	wg                         sync.WaitGroup
	logger                     util.Logger
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

		if opts.MaxBotSchedulePerPartition <= 0 {
			opts.MaxBotSchedulePerPartition = getDefaultMaxBotSchedulePerPartition()
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
		mongoClient:                mongoClient,
		natsConn:                   natsConn,
		dbName:                     opts.DBName,
		collectionName:             opts.CollectionName,
		maxRetries:                 opts.MaxRetries,
		maxBotSchedulePerPartition: opts.MaxBotSchedulePerPartition,
		subscriptions:              make([]*nats.Subscription, 0),
		stopChan:                   make(chan struct{}),
		wg:                         sync.WaitGroup{},
		logger:                     opts.Logger,
	}, nil
}

func (s *NATSSubscriber) Start(ctx context.Context) error {
	s.logger.Info("Starting NATS Subscriber...")

	subjects := []string{
		SubjectBotScheduleCreated,
		SubjectBotScheduleUpdated,
		SubjectBotScheduleDeleted,
	}

	for _, subject := range subjects {
		if err := s.setupSubscription(ctx, subject); err != nil {
			return err
		}
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
		case SubjectBotScheduleCreated:
			err = s.handleCreate(ctx, msg)
		case SubjectBotScheduleUpdated:
			err = s.handleUpdate(ctx, msg)
		case SubjectBotScheduleDeleted:
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
	filter := bson.M{"bot_count": bson.M{"$lt": s.maxBotSchedulePerPartition}}
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

	// Skip UUID validation to allow any bot schedule ID format
	// if !util.ValidateUUID(message.ID) {
	//	return fmt.Errorf("invalid bot schedule ID: %s", message.ID)
	// }

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

	segmentId, err := s.findOrCreatePartition(ctx)
	if err != nil {
		return fmt.Errorf("failed to find or create partition for bot schedule %s: %v", message.ID, err)
	}

	return s.upsertBotSchedule(ctx, collection, *message, segmentId)
}

func (s *NATSSubscriber) handleUpdate(ctx context.Context, msg *nats.Msg) error {
	message, err := s.parseEventMessage(msg.Data)
	if err != nil {
		return fmt.Errorf("failed to parse message: %v", err)
	}

	// Skip UUID validation to allow any bot schedule ID format
	// if !util.ValidateUUID(message.ID) {
	//	return fmt.Errorf("invalid bot schedule ID: %s", message.ID)
	// }

	collection := s.mongoClient.Database(s.dbName).Collection(s.collectionName)
	filter := bson.M{"id": message.ID}
	var existingBot model.BotSchedule
	err = collection.FindOne(ctx, filter).Decode(&existingBot)
	if err != nil {
		if err == mongo.ErrNoDocuments {
			// If not found, robust upsert: create a new bot schedule
			s.logger.Info("Bot schedule not found, creating new one", "id", message.ID)
			segmentId, err := s.findOrCreatePartition(ctx)
			if err != nil {
				return fmt.Errorf("failed to find or create partition during upsert for bot schedule %s: %w", message.ID, err)
			}
			return s.upsertBotSchedule(ctx, collection, *message, segmentId)
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
		return fmt.Errorf("missing or invalid schedule in message config")
	}
	if !existingOk || existingSchedule != newSchedule {
		nextTime, err := util.CalculateNextExecutionTime(
			newSchedule,
			time.Now(),
		)
		if err != nil {
			return fmt.Errorf("failed to calculate next execution time: %v", err)
		}
		update["$set"].(bson.M)["next_execution_time"] = nextTime.Unix()
	}

	result, err := collection.UpdateOne(ctx, filter, update)
	if err != nil {
		return fmt.Errorf("failed to update bot schedule: %v", err)
	}
	if result.ModifiedCount == 0 {
		return fmt.Errorf("no bot schedule found with ID: %s", message.ID)
	}

	s.logger.Info("Updated bot schedule", "id", message.ID)
	return nil
}

func (s *NATSSubscriber) handleDelete(ctx context.Context, msg *nats.Msg) error {
	message, err := s.parseEventMessage(msg.Data)
	if err != nil {
		return fmt.Errorf("failed to parse message: %v", err)
	}

	// Skip UUID validation to allow any bot schedule ID format
	// if !util.ValidateUUID(message.ID) {
	//	return fmt.Errorf("invalid bot schedule ID: %s", message.ID)
	// }

	collection := s.mongoClient.Database(s.dbName).Collection(s.collectionName)
	filter := bson.M{"id": message.ID}

	var botScheduleToDelete model.BotSchedule
	err = collection.FindOne(ctx, filter).Decode(&botScheduleToDelete)
	if err != nil {
		if err == mongo.ErrNoDocuments {
			s.logger.Info("Bot schedule not found, nothing to delete", "id", message.ID)
			return nil // No action needed if bot schedule does not exist
		}
		return fmt.Errorf("failed to find bot schedule: %v", err)
	}

	_, err = collection.DeleteOne(ctx, filter)
	if err != nil {
		return fmt.Errorf("failed to delete bot schedule: %v", err)
	}
	s.logger.Info("Deleted bot schedule from collection", "id", message.ID)

	partitionsCollection := s.mongoClient.Database(s.dbName).Collection("partitions")
	partitionFilter := bson.M{"_id": botScheduleToDelete.SegmentId}
	update := bson.M{"$inc": bson.M{"bot_count": -1}}
	_, err = partitionsCollection.UpdateOne(ctx, partitionFilter, update)
	if err != nil {
		s.logger.Error("CRITICAL: Failed to update partition count after bot schedule deletion.", "partition_id", botScheduleToDelete.SegmentId, "error", err)
		return fmt.Errorf("failed to update partition count after bot schedule deletion: %w", err)
	}
	s.logger.Info("Updated partition count after bot schedule deletion", "partition_id", botScheduleToDelete.SegmentId)

	return nil
}

// upsertBotSchedule inserts or updates a bot schedule in the collection.
func (s *NATSSubscriber) upsertBotSchedule(
	ctx context.Context,
	collection *mongo.Collection,
	message BotScheduleMessage,
	segmentID int32,
) error {
	schedule, ok := message.Config["schedule"].(string)
	if !ok || schedule == "" {
		return fmt.Errorf("missing or invalid schedule in message config")
	}

	nextTime, err := util.CalculateNextExecutionTime(
		schedule,
		time.Now(),
	)
	if err != nil {
		return fmt.Errorf("failed to calculate next execution time: %v", err)
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
		SegmentId:         segmentID,
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

	s.logger.Info("Upserted bot schedule", "id", botSchedule.ID, "segment_id", botSchedule.SegmentId)
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
