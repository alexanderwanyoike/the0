// Package docker provides a multi-collection bot resolver for query execution.
package docker

import (
	"context"
	"fmt"
	"runtime/internal/constants"
	"runtime/internal/model"

	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/mongo"
)

// MultiBotResolver resolves bots from both realtime (bot_runner) and scheduled (bot_scheduler) collections.
type MultiBotResolver struct {
	mongoClient *mongo.Client
	runner      DockerRunner
}

// NewMultiBotResolver creates a resolver that can look up bots from either collection.
func NewMultiBotResolver(mongoClient *mongo.Client, runner DockerRunner) *MultiBotResolver {
	return &MultiBotResolver{
		mongoClient: mongoClient,
		runner:      runner,
	}
}

// GetBot retrieves a bot by ID, checking both realtime and scheduled collections.
// The botType parameter hints which collection to check first ("realtime" or "scheduled").
func (r *MultiBotResolver) GetBot(ctx context.Context, botID string) (*model.Bot, error) {
	// Try realtime bots first
	bot, err := r.getBotFromCollection(ctx, constants.BOT_RUNNER_DB_NAME, constants.BOT_RUNNER_COLLECTION, botID)
	if err == nil {
		return bot, nil
	}

	// Try scheduled bots
	schedule, err := r.getScheduleFromCollection(ctx, constants.BOT_SCHEDULER_DB_NAME, constants.BOT_SCHEDULE_COLLECTION, botID)
	if err == nil {
		// Convert BotSchedule to Bot for query execution
		return r.scheduleToBot(schedule), nil
	}

	return nil, fmt.Errorf("bot not found: %s", botID)
}

// GetContainerID returns the container ID for a running realtime bot.
// Scheduled bots don't have persistent containers.
func (r *MultiBotResolver) GetContainerID(ctx context.Context, botID string) (string, bool) {
	if r.runner == nil {
		return "", false
	}

	// Check running containers for this bot
	containers, err := r.runner.ListManagedContainers(ctx, -1)
	if err != nil {
		return "", false
	}

	for _, c := range containers {
		if c.ID == botID {
			return c.ContainerID, true
		}
	}

	return "", false
}

func (r *MultiBotResolver) getBotFromCollection(ctx context.Context, dbName, collectionName, botID string) (*model.Bot, error) {
	collection := r.mongoClient.Database(dbName).Collection(collectionName)

	var bot model.Bot
	// Try both "id" and "_id" fields since storage format may vary
	err := collection.FindOne(ctx, bson.M{"$or": []bson.M{{"id": botID}, {"_id": botID}}}).Decode(&bot)
	if err != nil {
		return nil, err
	}

	return &bot, nil
}

func (r *MultiBotResolver) getScheduleFromCollection(ctx context.Context, dbName, collectionName, botID string) (*model.BotSchedule, error) {
	collection := r.mongoClient.Database(dbName).Collection(collectionName)

	var schedule model.BotSchedule
	// Try both _id and id fields
	err := collection.FindOne(ctx, bson.M{"$or": []bson.M{{"_id": botID}, {"id": botID}}}).Decode(&schedule)
	if err != nil {
		return nil, err
	}

	return &schedule, nil
}

// scheduleToBot converts a BotSchedule to a Bot model for query execution.
func (r *MultiBotResolver) scheduleToBot(schedule *model.BotSchedule) *model.Bot {
	return &model.Bot{
		ID:               schedule.ID,
		Config:           schedule.Config,
		CustomBotVersion: schedule.CustomBotVersion,
	}
}
