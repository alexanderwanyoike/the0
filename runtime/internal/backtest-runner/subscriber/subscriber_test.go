package subscriber

import (
	"context"
	"encoding/json"
	"os"
	"runtime/internal/backtest-runner/model"
	"runtime/internal/constants"
	"runtime/internal/util"
	"strings"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/mongo"
	mongoOptions "go.mongodb.org/mongo-driver/mongo/options"
)

// Setup test MongoDB instance
func setupTestMongo(t *testing.T, dbName, collectionName string) (*mongo.Client, func()) {
	mongoURI := os.Getenv("MONGO_URI")
	if mongoURI == "" {
		mongoURI = "mongodb://localhost:27017"
	}

	client, err := mongo.Connect(
		context.Background(),
		mongoOptions.Client().ApplyURI(mongoURI),
	)
	if err != nil {
		t.Skip("MongoDB not available, skipping test")
	}

	err = client.Ping(context.Background(), nil)
	if err != nil {
		t.Skip("MongoDB not available, skipping test")
	}

	// Test if we can perform basic operations (write permissions)
	testDB := client.Database(dbName)
	testCollection := testDB.Collection("test_collection")
	testDoc := bson.M{"test": "value"}
	_, err = testCollection.InsertOne(context.Background(), testDoc)
	if err != nil {
		t.Skipf("MongoDB authentication required, skipping test: %v", err)
	}

	// Clean up test document
	testCollection.DeleteOne(context.Background(), testDoc)

	cleanup := func() {
		client.Database(dbName).Drop(context.Background())
		client.Disconnect(context.Background())
	}

	return client, cleanup
}

// Helper function to create test message
func createTestMessage(t *testing.T, id string) *BacktestMessage {
	return &BacktestMessage{
		ID: id,
		Config: map[string]interface{}{
			"param1": "value1",
		},
		CustomBotVersion: model.CustomBotVersion{
			Version:  "1.0.0",
			FilePath: "gs://test-bucket/test-backtest.zip",
			Config: model.APIBotConfig{
				Name:    "Test Backtest",
				Runtime: "python3.11",
			},
		},
	}
}

func TestParseEventMessage(t *testing.T) {
	subscriber := &NATSSubscriber{
		logger: &util.DefaultLogger{},
	}

	// Test direct payload format
	payload := struct {
		ID     string                 `json:"ID"`
		Config map[string]interface{} `json:"Config"`
		Custom struct {
			Config    model.APIBotConfig `json:"Config"`
			CreatedAt time.Time          `json:"CreatedAt"`
			UpdatedAt time.Time          `json:"UpdatedAt"`
			FilePath  string             `json:"FilePath"`
			Version   string             `json:"Version"`
		} `json:"Custom"`
	}{
		ID: "test-backtest-123",
		Config: map[string]interface{}{
			"param1": "value1",
		},
	}
	payload.Custom.Config = model.APIBotConfig{
		Name:    "Test Backtest",
		Runtime: "python3.11",
	}
	payload.Custom.FilePath = "gs://test-bucket/test.zip"
	payload.Custom.Version = "1.0.0"
	payload.Custom.CreatedAt = time.Now()
	payload.Custom.UpdatedAt = time.Now()

	data, err := json.Marshal(payload)
	require.NoError(t, err)

	message, err := subscriber.parseEventMessage(data)
	assert.NoError(t, err)
	assert.NotNil(t, message)
	assert.Equal(t, "test-backtest-123", message.ID)
	assert.Equal(t, "value1", message.Config["param1"])
	assert.Equal(t, "python3.11", message.CustomBotVersion.Config.Runtime)
}

func TestParseEventMessageInvalidJSON(t *testing.T) {
	subscriber := &NATSSubscriber{
		logger: &util.DefaultLogger{},
	}

	invalidJSON := []byte(`{"invalid": json}`)

	message, err := subscriber.parseEventMessage(invalidJSON)
	assert.Error(t, err)
	assert.Nil(t, message)
	assert.Contains(t, err.Error(), "failed to unmarshal")
}

func TestBacktestMessage(t *testing.T) {
	msg := BacktestMessage{
		ID: "test-backtest-123",
		Config: map[string]interface{}{
			"param1": "value1",
		},
		CustomBotVersion: model.CustomBotVersion{
			Version:  "1.0.0",
			FilePath: "gs://test-bucket/test.zip",
		},
	}

	assert.Equal(t, "test-backtest-123", msg.ID)
	assert.Equal(t, "value1", msg.Config["param1"])
	assert.Equal(t, "1.0.0", msg.CustomBotVersion.Version)
}

func TestSubjectConstants(t *testing.T) {
	assert.Equal(t, "the0.backtest.created", SubjectBacktestCreated)
}

func TestDefaultOptions(t *testing.T) {
	os.Setenv("NATS_URL", "nats://localhost:4222")
	os.Setenv("MAX_RETRIES", "5")
	os.Setenv("MAX_BACKTESTS_PER_PARTITION", "20")

	opts := DefaultOptions()

	assert.Equal(t, "nats://localhost:4222", opts.NATSUrl)
	assert.Equal(t, constants.BACKTEST_RUNNER_DB_NAME, opts.DBName)
	assert.Equal(t, constants.BACKTEST_COLLECTION, opts.CollectionName)
	assert.Equal(t, 5, opts.MaxRetries)
	assert.Equal(t, 20, opts.MaxBacktestsPerPartition)
}

func TestNewNATSSubscriber(t *testing.T) {
	t.Run("MissingNATSURL", func(t *testing.T) {
		os.Setenv("NATS_URL", "")
		os.Setenv("MONGO_URI", "mongodb://localhost:27017")

		_, err := NewNATSSubscriber(context.Background())
		assert.Error(t, err)
		assert.Contains(t, err.Error(), "NATS_URL")
	})

	t.Run("MissingMongoURI", func(t *testing.T) {
		os.Setenv("NATS_URL", "nats://localhost:4222")
		os.Setenv("MONGO_URI", "")

		_, err := NewNATSSubscriber(context.Background())
		assert.Error(t, err)
		assert.Contains(t, err.Error(), "MONGO_URI")
	})
}

func TestGetDefaultMaxRetries(t *testing.T) {
	// Test default value
	os.Setenv("MAX_RETRIES", "")
	assert.Equal(t, 3, getDefaultMaxRetries())

	// Test valid value
	os.Setenv("MAX_RETRIES", "5")
	assert.Equal(t, 5, getDefaultMaxRetries())

	// Test invalid value (should fall back to default)
	os.Setenv("MAX_RETRIES", "invalid")
	assert.Equal(t, 3, getDefaultMaxRetries())
}

func TestGetDefaultMaxBotsPerPartition(t *testing.T) {
	// Test default value
	os.Setenv("MAX_BACKTESTS_PER_PARTITION", "")
	assert.Equal(t, 10, getDefaultMaxBotsPerPartition())

	// Test valid value
	os.Setenv("MAX_BACKTESTS_PER_PARTITION", "15")
	assert.Equal(t, 15, getDefaultMaxBotsPerPartition())

	// Test invalid value (should fall back to default)
	os.Setenv("MAX_BACKTESTS_PER_PARTITION", "invalid")
	assert.Equal(t, 10, getDefaultMaxBotsPerPartition())
}

func TestSegmentIdConsistency(t *testing.T) {
	// Setup test MongoDB
	dbName := "test_segment_consistency_" + time.Now().Format("20060102150405")
	collectionName := "backtests"
	client, cleanup := setupTestMongo(t, dbName, collectionName)
	defer cleanup()

	t.Run("deterministic segment assignment for same backtest ID", func(t *testing.T) {
		subscriber := &NATSSubscriber{
			mongoClient:              client,
			dbName:                   dbName,
			collectionName:           collectionName,
			maxBacktestsPerPartition: 10,
			logger:                   &util.DefaultLogger{},
		}

		backtestID := "550e8400-e29b-41d4-a716-446655440000"

		// Create the same backtest multiple times and verify segment consistency
		for i := 0; i < 3; i++ {
			msg := createTestMessage(t, backtestID)
			collection := client.Database(dbName).Collection(collectionName)
			err := subscriber.upsertBacktest(context.Background(), collection, *msg, 1)
			assert.NoError(t, err)

			// Verify backtest was created/updated with consistent segment ID
			var backtest model.Backtest
			err = collection.FindOne(context.Background(), bson.M{"id": backtestID}).Decode(&backtest)
			assert.NoError(t, err)
			assert.Equal(t, int32(1), backtest.SegmentId, "Segment ID should be consistent across multiple creates (iteration %d)", i)
		}
	})

	t.Run("edge cases for segment ID assignment", func(t *testing.T) {
		subscriber := &NATSSubscriber{
			mongoClient:              client,
			dbName:                   dbName,
			collectionName:           collectionName,
			maxBacktestsPerPartition: 5,
			logger:                   &util.DefaultLogger{},
		}

		edgeCases := []struct {
			name       string
			backtestID string
		}{
			{"empty string", ""},
			{"single character", "a"},
			{"very long ID", strings.Repeat("very-long-backtest-id-", 100)},
			{"unicode characters", "测试-бот-テスト-🤖"},
			{"special characters", "backtest@domain.com/path?param=value#anchor"},
		}

		for _, tc := range edgeCases {
			t.Run(tc.name, func(t *testing.T) {
				msg := createTestMessage(t, tc.backtestID)
				collection := client.Database(dbName).Collection(collectionName)
				err := subscriber.upsertBacktest(context.Background(), collection, *msg, 2)
				assert.NoError(t, err)

				var backtest model.Backtest
				err = collection.FindOne(context.Background(), bson.M{"id": tc.backtestID}).Decode(&backtest)
				assert.NoError(t, err)

				// Verify segment is in valid range
				assert.GreaterOrEqual(t, backtest.SegmentId, int32(0))
				assert.Equal(t, int32(2), backtest.SegmentId)
			})
		}
	})
}

func TestUpsertBacktest(t *testing.T) {
	// Setup test MongoDB
	dbName := "test_upsert_" + time.Now().Format("20060102150405")
	collectionName := "backtests"
	client, cleanup := setupTestMongo(t, dbName, collectionName)
	defer cleanup()

	subscriber := &NATSSubscriber{
		mongoClient:              client,
		dbName:                   dbName,
		collectionName:           collectionName,
		maxBacktestsPerPartition: 10,
		logger:                   &util.DefaultLogger{},
	}

	t.Run("successful upsert", func(t *testing.T) {
		msg := createTestMessage(t, "upsert-test-backtest")
		collection := client.Database(dbName).Collection(collectionName)

		err := subscriber.upsertBacktest(context.Background(), collection, *msg, 3)
		assert.NoError(t, err)

		// Verify backtest was created
		var backtest model.Backtest
		err = collection.FindOne(context.Background(), bson.M{"id": "upsert-test-backtest"}).Decode(&backtest)
		assert.NoError(t, err)
		assert.Equal(t, "upsert-test-backtest", backtest.ID)
		assert.Equal(t, int32(3), backtest.SegmentId)
		assert.Equal(t, "value1", backtest.Config["param1"])
	})
}

func TestMockSubscriber(t *testing.T) {
	mock := &MockSubscriber{}

	// Test Start
	err := mock.Start(context.Background())
	assert.NoError(t, err)
	assert.True(t, mock.SubscribeCalled)

	// Test Stop
	err = mock.Stop()
	assert.NoError(t, err)
	assert.False(t, mock.SubscribeCalled)
}
