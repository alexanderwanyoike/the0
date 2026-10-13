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
	"runtime/internal/desired"
	"runtime/internal/model"
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

func (s *Subscriber) handleSchedule(ctx context.Context, subject string, data []byte) error {
	switch subject {
	case SubjectBotScheduleCreated:
		return s.createSchedule(ctx, data)
	case SubjectBotScheduleUpdated:
		return s.updateSchedule(ctx, data)
	case SubjectBotScheduleDeleted:
		return s.deleteSchedule(ctx, data)
	default:
		return botevents.Permanent(fmt.Errorf("unknown subject: %s", subject))
	}
}

func (s *Subscriber) parseScheduleMessage(data []byte) (*BotScheduleMessage, error) {
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

func (s *Subscriber) createSchedule(ctx context.Context, data []byte) error {
	message, err := s.parseScheduleMessage(data)
	if err != nil {
		return botevents.Permanent(fmt.Errorf("failed to parse message: %v", err))
	}

	collection := s.schedules
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

func (s *Subscriber) updateSchedule(ctx context.Context, data []byte) error {
	message, err := s.parseScheduleMessage(data)
	if err != nil {
		return botevents.Permanent(fmt.Errorf("failed to parse message: %v", err))
	}

	collection := s.schedules
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

	newSchedule, newOk := message.Config["schedule"].(string)
	if !newOk || newSchedule == "" {
		return botevents.Permanent(fmt.Errorf("missing or invalid schedule in message config"))
	}
	if desired.Reschedules(existingBot, message.Config) {
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

func (s *Subscriber) deleteSchedule(ctx context.Context, data []byte) error {
	message, err := s.parseScheduleMessage(data)
	if err != nil {
		return botevents.Permanent(fmt.Errorf("failed to parse message: %v", err))
	}

	collection := s.schedules
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
func (s *Subscriber) upsertBotSchedule(
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
