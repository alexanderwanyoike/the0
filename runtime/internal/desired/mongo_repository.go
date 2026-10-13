package desired

import (
	"context"
	"fmt"
	"time"

	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/mongo"

	"runtime/internal/model"
)

// MongoRepository is the Repository in the MongoDB collections the
// subscribers write.
type MongoRepository struct {
	bots      *mongo.Collection
	schedules *mongo.Collection
}

func NewMongoRepository(bots, schedules *mongo.Collection) *MongoRepository {
	return &MongoRepository{bots: bots, schedules: schedules}
}

func (r *MongoRepository) Bots(ctx context.Context) ([]model.Bot, error) {
	var bots []model.Bot
	if err := findAll(ctx, r.bots, &bots); err != nil {
		return nil, fmt.Errorf("failed to read bots: %w", err)
	}
	return bots, nil
}

func (r *MongoRepository) Schedules(ctx context.Context) ([]model.BotSchedule, error) {
	var schedules []model.BotSchedule
	if err := findAll(ctx, r.schedules, &schedules); err != nil {
		return nil, fmt.Errorf("failed to read schedules: %w", err)
	}
	return schedules, nil
}

func (r *MongoRepository) SetNextExecutionTime(ctx context.Context, scheduleID string, next time.Time) error {
	_, err := r.schedules.UpdateOne(ctx,
		bson.M{"id": scheduleID},
		bson.M{"$set": bson.M{"next_execution_time": next.Unix()}},
	)
	if err != nil {
		return fmt.Errorf("failed to set the next execution time of schedule %s: %w", scheduleID, err)
	}
	return nil
}

func findAll(ctx context.Context, collection *mongo.Collection, results interface{}) error {
	cursor, err := collection.Find(ctx, bson.M{})
	if err != nil {
		return err
	}
	defer cursor.Close(ctx)
	return cursor.All(ctx, results)
}
