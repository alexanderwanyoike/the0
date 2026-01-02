package controller

import (
	"context"
	"fmt"

	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/mongo"

	"runtime/internal/bot-runner/model"
	scheduleModel "runtime/internal/bot-scheduler/model"
)

// MongoBotRepository implements BotRepository using MongoDB.
type MongoBotRepository struct {
	collection *mongo.Collection
}

// NewMongoBotRepository creates a new MongoBotRepository.
func NewMongoBotRepository(client *mongo.Client, dbName, collectionName string) *MongoBotRepository {
	collection := client.Database(dbName).Collection(collectionName)
	return &MongoBotRepository{collection: collection}
}

// FindAllEnabled returns all bots that should be running.
// This uses the same query pattern as the Docker mode worker.
func (r *MongoBotRepository) FindAllEnabled(ctx context.Context) ([]model.Bot, error) {
	// Query for all enabled bots (across all segments)
	// Note: In K8s mode we don't filter by segment - the controller manages ALL bots
	filter := bson.M{
		"$or": []bson.M{
			{"config.enabled": bson.M{"$ne": false}},     // explicitly not disabled
			{"config.enabled": bson.M{"$exists": false}}, // enabled field missing = enabled by default
		},
	}

	cursor, err := r.collection.Find(ctx, filter)
	if err != nil {
		return nil, fmt.Errorf("failed to query bots: %w", err)
	}
	defer cursor.Close(ctx)

	var bots []model.Bot
	if err = cursor.All(ctx, &bots); err != nil {
		return nil, fmt.Errorf("failed to decode bots: %w", err)
	}

	return bots, nil
}

// MongoBotScheduleRepository implements BotScheduleRepository using MongoDB.
type MongoBotScheduleRepository struct {
	collection *mongo.Collection
}

// NewMongoBotScheduleRepository creates a new MongoBotScheduleRepository.
func NewMongoBotScheduleRepository(client *mongo.Client, dbName, collectionName string) *MongoBotScheduleRepository {
	collection := client.Database(dbName).Collection(collectionName)
	return &MongoBotScheduleRepository{collection: collection}
}

// FindAllEnabled returns all schedules that should be running.
// This uses the same query pattern as the Docker mode worker.
func (r *MongoBotScheduleRepository) FindAllEnabled(ctx context.Context) ([]scheduleModel.BotSchedule, error) {
	// Query for all enabled schedules
	filter := bson.M{
		"$or": []bson.M{
			{"config.enabled": bson.M{"$ne": false}},     // explicitly not disabled
			{"config.enabled": bson.M{"$exists": false}}, // enabled field missing = enabled by default
		},
	}

	cursor, err := r.collection.Find(ctx, filter)
	if err != nil {
		return nil, fmt.Errorf("failed to query schedules: %w", err)
	}
	defer cursor.Close(ctx)

	var schedules []scheduleModel.BotSchedule
	if err = cursor.All(ctx, &schedules); err != nil {
		return nil, fmt.Errorf("failed to decode schedules: %w", err)
	}

	return schedules, nil
}
