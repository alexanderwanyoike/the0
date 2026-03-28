package gc

import (
	"context"

	"runtime/internal/constants"

	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/mongo"
	"go.mongodb.org/mongo-driver/mongo/options"
)

// mongoBotStore implements BotStore by querying both bot_runner and bot_scheduler collections.
type mongoBotStore struct {
	client *mongo.Client
}

// NewMongoBotStore creates a BotStore from a mongo.Client.
func NewMongoBotStore(client *mongo.Client) BotStore {
	return &mongoBotStore{client: client}
}

func (m *mongoBotStore) GetAllBotIDs(ctx context.Context) (map[string]bool, error) {
	ids := make(map[string]bool)

	// Query bot_runner.bots
	if err := m.collectIDs(ctx, constants.BOT_RUNNER_DB_NAME, constants.BOT_RUNNER_COLLECTION, ids); err != nil {
		return nil, err
	}

	// Query bot_scheduler.bot_schedules
	if err := m.collectIDs(ctx, constants.BOT_SCHEDULER_DB_NAME, constants.BOT_SCHEDULE_COLLECTION, ids); err != nil {
		return nil, err
	}

	return ids, nil
}

func (m *mongoBotStore) collectIDs(ctx context.Context, dbName, collectionName string, ids map[string]bool) error {
	collection := m.client.Database(dbName).Collection(collectionName)

	// Only fetch the "id" field for efficiency
	cursor, err := collection.Find(ctx, bson.M{}, options.Find().SetProjection(bson.M{"id": 1}))
	if err != nil {
		return err
	}
	defer cursor.Close(ctx)

	for cursor.Next(ctx) {
		var doc struct {
			ID string `bson:"id"`
		}
		if err := cursor.Decode(&doc); err != nil {
			continue
		}
		if doc.ID != "" {
			ids[doc.ID] = true
		}
	}

	return cursor.Err()
}
