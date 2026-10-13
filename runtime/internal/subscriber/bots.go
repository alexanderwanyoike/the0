package subscriber

import (
	"context"
	"encoding/json"
	"fmt"
	"time"

	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/mongo"
	mongoOptions "go.mongodb.org/mongo-driver/mongo/options"

	"runtime/internal/botevents"
	"runtime/internal/model"
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

func (s *Subscriber) handleBot(ctx context.Context, subject string, data []byte) error {
	switch subject {
	case SubjectBotCreated:
		return s.createBot(ctx, data)
	case SubjectBotUpdated:
		return s.updateBot(ctx, data)
	case SubjectBotDeleted:
		return s.deleteBot(ctx, data)
	default:
		return botevents.Permanent(fmt.Errorf("unknown subject: %s", subject))
	}
}

func (s *Subscriber) parseBotMessage(data []byte) (*BotMessage, error) {
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

func (s *Subscriber) createBot(ctx context.Context, data []byte) error {
	message, err := s.parseBotMessage(data)
	if err != nil {
		return botevents.Permanent(fmt.Errorf("failed to parse message: %v", err))
	}

	// Debug logging to see what we actually parsed
	s.logger.Info("DEBUG handleCreate: Parsed message", "id", message.ID, "config_nil", message.Config == nil, "custom_version", message.CustomBotVersion.Version, "custom_runtime", message.CustomBotVersion.Config.Runtime)

	collection := s.bots
	filter := bson.M{"id": message.ID}
	count, err := collection.CountDocuments(ctx, filter)

	if err != nil {
		return fmt.Errorf("failed to check if bot exists: %v", err)
	}

	if count > 0 {
		s.logger.Info("Bot already exists, skipping creation", "id", message.ID)
		return nil
	}

	return s.upsertBot(ctx, collection, *message)
}

func (s *Subscriber) updateBot(ctx context.Context, data []byte) error {
	message, err := s.parseBotMessage(data)
	if err != nil {
		return botevents.Permanent(fmt.Errorf("failed to parse message: %v", err))
	}

	collection := s.bots
	filter := bson.M{"id": message.ID}
	var existingBot model.Bot
	err = collection.FindOne(ctx, filter).Decode(&existingBot)
	if err != nil {
		if err == mongo.ErrNoDocuments {
			// If not found, robust upsert: create a new bot
			s.logger.Info("Bot not found, creating new bot", "id", message.ID)
			return s.upsertBot(ctx, collection, *message)
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
func (s *Subscriber) upsertBot(
	ctx context.Context,
	collection *mongo.Collection,
	message BotMessage,
) error {
	bot := model.Bot{
		ID:               message.ID,
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

	s.logger.Info("Upserted bot", "id", bot.ID)
	return nil
}

func (s *Subscriber) deleteBot(ctx context.Context, data []byte) error {
	message, err := s.parseBotMessage(data)
	if err != nil {
		return botevents.Permanent(fmt.Errorf("failed to parse message: %v", err))
	}

	collection := s.bots
	filter := bson.M{"id": message.ID}

	result, err := collection.DeleteOne(ctx, filter)
	if err != nil {
		return fmt.Errorf("failed to delete bot: %v", err)
	}
	if result.DeletedCount == 0 {
		s.logger.Info("Bot not found, nothing to delete", "id", message.ID)
		return nil
	}
	s.logger.Info("Deleted bot from collection", "id", message.ID)

	return nil
}
