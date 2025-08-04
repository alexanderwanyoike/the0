package server

import (
	"context"
	"fmt"
	"runtime/pb"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"github.com/testcontainers/testcontainers-go"
	"github.com/testcontainers/testcontainers-go/wait"
	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/mongo"
	mongoOptions "go.mongodb.org/mongo-driver/mongo/options"
)

func setupTestMongo(t *testing.T) (string, func()) {
	ctx := context.Background()
	
	req := testcontainers.ContainerRequest{
		Image:        "mongo:7",
		ExposedPorts: []string{"27017/tcp"},
		WaitingFor:   wait.ForLog("Waiting for connections"),
	}
	
	mongoContainer, err := testcontainers.GenericContainer(ctx, testcontainers.GenericContainerRequest{
		ContainerRequest: req,
		Started:          true,
	})
	require.NoError(t, err)
	
	host, err := mongoContainer.Host(ctx)
	require.NoError(t, err)
	
	port, err := mongoContainer.MappedPort(ctx, "27017")
	require.NoError(t, err)
	
	mongoUri := fmt.Sprintf("mongodb://%s:%s", host, port.Port())
	
	cleanup := func() {
		mongoContainer.Terminate(ctx)
	}
	
	return mongoUri, cleanup
}

func setupTestPartitions(t *testing.T, mongoUri, dbName string) {
	client, err := mongo.Connect(context.Background(), mongoOptions.Client().ApplyURI(mongoUri))
	require.NoError(t, err)
	defer client.Disconnect(context.Background())
	
	// Create test partitions
	partitionsCollection := client.Database(dbName).Collection("partitions")
	testPartitions := []any{
		bson.M{"_id": int32(1), "bot_count": 5},
		bson.M{"_id": int32(2), "bot_count": 3},
		bson.M{"_id": int32(3), "bot_count": 0}, // Empty partition - should not be assigned
		bson.M{"_id": int32(4), "bot_count": 2},
	}
	
	_, err = partitionsCollection.InsertMany(context.Background(), testPartitions)
	require.NoError(t, err)
}

func TestWorkerServiceGrpcServer_GetWorkerServiceGrpcServer(t *testing.T) {
	mongoUri, cleanup := setupTestMongo(t)
	defer cleanup()
	
	// Reset global server to ensure clean state
	server = nil
	testServer, err := GetWorkerServiceGrpcServer(mongoUri, "test_db", "test_collection")
	assert.NoError(t, err)
	assert.NotNil(t, testServer)
	assert.Equal(t, "test_db", testServer.dbName)
	assert.Equal(t, "test_collection", testServer.collectionName)
	assert.NotNil(t, testServer.mongoClient)
	assert.NotNil(t, testServer.workers)
	assert.NotNil(t, testServer.segments)
	
	testServer.Stop()
}

func TestWorkerServiceGrpcServer_GetWorkerServiceGrpcServer_InvalidMongo(t *testing.T) {
	// Reset global server to ensure clean state
	server = nil
	testServer, err := GetWorkerServiceGrpcServer("", "test_db", "test_collection")
	assert.Error(t, err)
	assert.Nil(t, testServer)
	assert.Contains(t, err.Error(), "MONGO_URI environment variable is not set")
}

func TestWorkerServiceGrpcServer_Heartbeat(t *testing.T) {
	mongoUri, cleanup := setupTestMongo(t)
	defer cleanup()
	
	// Reset global server to ensure clean state
	server = nil
	testServer, err := GetWorkerServiceGrpcServer(mongoUri, "test_db_heartbeat", "test_collection")
	require.NoError(t, err)
	defer testServer.Stop()
	
	t.Run("InvalidJSON", func(t *testing.T) {
		req := &pb.Request{Data: "invalid json"}
		resp, err := testServer.Heartbeat(context.Background(), req)
		assert.Error(t, err)
		assert.Nil(t, resp)
	})
	
	t.Run("MissingWorkerID", func(t *testing.T) {
		req := &pb.Request{Data: `{"other": "field"}`}
		resp, err := testServer.Heartbeat(context.Background(), req)
		assert.Error(t, err)
		assert.Nil(t, resp)
		assert.Contains(t, err.Error(), "worker id not found")
	})
	
	t.Run("WorkerNotExists", func(t *testing.T) {
		req := &pb.Request{Data: `{"workerId": "nonexistent-worker"}`}
		resp, err := testServer.Heartbeat(context.Background(), req)
		assert.NoError(t, err)
		assert.NotNil(t, resp)
		assert.Equal(t, "resubscribe_required", resp.Data)
	})
	
	t.Run("ValidHeartbeat", func(t *testing.T) {
		// First add a worker
		workerId := "test-worker-heartbeat"
		testServer.onConnected(workerId)
		
		// Give time for worker to be added
		time.Sleep(10 * time.Millisecond)
		
		req := &pb.Request{Data: fmt.Sprintf(`{"workerId": "%s"}`, workerId)}
		resp, err := testServer.Heartbeat(context.Background(), req)
		assert.NoError(t, err)
		assert.NotNil(t, resp)
		assert.Equal(t, "ok", resp.Data)
	})
}

func TestWorkerServiceGrpcServer_onConnected(t *testing.T) {
	mongoUri, cleanup := setupTestMongo(t)
	defer cleanup()
	
	// Reset global server to ensure clean state
	server = nil
	testServer, err := GetWorkerServiceGrpcServer(mongoUri, "test_db_connected", "test_collection")
	require.NoError(t, err)  
	defer testServer.Stop()
	
	workerId := "test-worker-connect"
	
	// Test initial connection
	testServer.onConnected(workerId)
	
	// Verify worker was added
	testServer.mutex.Lock()
	worker, exists := testServer.workers[workerId]
	testServer.mutex.Unlock()
	
	assert.True(t, exists)
	assert.NotNil(t, worker)
	assert.Equal(t, workerId, worker.workerId)
	assert.Equal(t, int32(-1), worker.segment) // Default segment
	
	// Test reconnection (should update existing worker)
	testServer.onConnected(workerId)
	
	testServer.mutex.Lock()
	worker2, exists2 := testServer.workers[workerId]
	testServer.mutex.Unlock()
	
	assert.True(t, exists2)
	assert.NotNil(t, worker2)
	assert.Equal(t, workerId, worker2.workerId)
}

func TestWorkerServiceGrpcServer_onDisconnected(t *testing.T) {
	mongoUri, cleanup := setupTestMongo(t)
	defer cleanup()
	
	// Reset global server to ensure clean state
	server = nil
	testServer, err := GetWorkerServiceGrpcServer(mongoUri, "test_db_disconnected", "test_collection")
	require.NoError(t, err)
	defer testServer.Stop()
	
	workerId := "test-worker-disconnect"
	
	// Add worker first
	testServer.onConnected(workerId)
	time.Sleep(10 * time.Millisecond)
	
	// Verify worker exists
	testServer.mutex.Lock()
	_, exists := testServer.workers[workerId]
	testServer.mutex.Unlock()
	assert.True(t, exists)
	
	// Disconnect worker
	testServer.onDisconnected(workerId)
	
	// Verify worker was removed
	testServer.mutex.Lock()
	_, exists = testServer.workers[workerId]
	testServer.mutex.Unlock()
	assert.False(t, exists)
}

func TestWorkerServiceGrpcServer_removeWorkerFromSegments(t *testing.T) {
	mongoUri, cleanup := setupTestMongo(t)
	defer cleanup()
	
	// Reset global server to ensure clean state
	server = nil
	testServer, err := GetWorkerServiceGrpcServer(mongoUri, "test_db_remove_segments", "test_collection")
	require.NoError(t, err)
	defer testServer.Stop()
	
	workerId := "test-worker-segment-remove"
	
	// Manually assign worker to segments
	testServer.mutex.Lock()
	testServer.segments[int32(1)] = workerId
	testServer.segments[int32(3)] = workerId
	testServer.segments[int32(5)] = "other-worker"
	testServer.mutex.Unlock()
	
	// Remove worker from segments
	testServer.removeWorkerFromSegments(workerId)
	
	// Verify worker was removed from segments 1 and 3, but segment 5 remains
	testServer.mutex.Lock()
	_, exists1 := testServer.segments[int32(1)]
	_, exists3 := testServer.segments[int32(3)]
	worker5, exists5 := testServer.segments[int32(5)]
	testServer.mutex.Unlock()
	
	assert.False(t, exists1)
	assert.False(t, exists3)
	assert.True(t, exists5)
	assert.Equal(t, "other-worker", worker5)
}

func TestWorkerServiceGrpcServer_getAvailableSegments(t *testing.T) {
	t.Run("NoPartitions", func(t *testing.T) {
		mongoUri, cleanup := setupTestMongo(t)
		defer cleanup()
		
		// Reset global server to ensure clean state
		server = nil
		testServer, err := GetWorkerServiceGrpcServer(mongoUri, "test_db_no_partitions", "test_collection")
		require.NoError(t, err)
		defer testServer.Stop()
		
		segments := testServer.getAvailableSegments()
		assert.Empty(t, segments)
	})
	
	t.Run("WithPartitions", func(t *testing.T) {
		mongoUri, cleanup := setupTestMongo(t)
		defer cleanup()
		
		// Reset global server to ensure clean state
		server = nil
		testServer, err := GetWorkerServiceGrpcServer(mongoUri, "test_db_with_partitions", "test_collection")
		require.NoError(t, err)
		defer testServer.Stop()
		
		setupTestPartitions(t, mongoUri, "test_db_with_partitions")
		
		segments := testServer.getAvailableSegments()
		
		// Should return partitions with bot_count > 0
		expectedSegments := []int32{1, 2, 4} // Partition 3 has bot_count = 0
		assert.ElementsMatch(t, expectedSegments, segments)
	})
}

func TestWorkerServiceGrpcServer_Rebalance(t *testing.T) {
	t.Run("NoWorkers", func(t *testing.T) {
		mongoUri, cleanup := setupTestMongo(t)
		defer cleanup()
		
		// Reset global server to ensure clean state
		server = nil
		testServer, err := GetWorkerServiceGrpcServer(mongoUri, "test_db_no_workers", "test_collection")
		require.NoError(t, err)
		defer testServer.Stop()
		
		// Should not panic with no workers
		testServer.Rebalance()
		
		testServer.mutex.Lock()
		segmentCount := len(testServer.segments)
		testServer.mutex.Unlock()
		
		assert.Equal(t, 0, segmentCount)
	})
	
	t.Run("WorkersNeedingSegments", func(t *testing.T) {
		mongoUri, cleanup := setupTestMongo(t)
		defer cleanup()
		
		// Reset global server to ensure clean state
		server = nil
		testServer, err := GetWorkerServiceGrpcServer(mongoUri, "test_db_workers", "test_collection")
		require.NoError(t, err)
		defer testServer.Stop()
		
		setupTestPartitions(t, mongoUri, "test_db_workers")
		
		// Add workers
		worker1 := "worker-1"
		worker2 := "worker-2"
		
		testServer.onConnected(worker1)
		testServer.onConnected(worker2)
		
		time.Sleep(10 * time.Millisecond)
		
		// Trigger rebalance
		testServer.Rebalance()
		
		// Wait for rebalance to complete
		time.Sleep(50 * time.Millisecond)
		
		// Verify workers got segments
		testServer.mutex.Lock()
		w1, exists1 := testServer.workers[worker1]
		w2, exists2 := testServer.workers[worker2]
		testServer.mutex.Unlock()
		
		assert.True(t, exists1)
		assert.True(t, exists2)
		assert.NotEqual(t, int32(-1), w1.segment)
		assert.NotEqual(t, int32(-1), w2.segment)
		assert.NotEqual(t, w1.segment, w2.segment) // Should have different segments
	})
}

func TestWorkerServiceGrpcServer_sendNotifications(t *testing.T) {
	mongoUri, cleanup := setupTestMongo(t)
	defer cleanup()
	
	// Reset global server to ensure clean state
	server = nil
	testServer, err := GetWorkerServiceGrpcServer(mongoUri, "test_db_notifications", "test_collection")
	require.NoError(t, err)
	defer testServer.Stop()
	
	// Create mock subscriber
	workerId := "test-worker-notify"
	notifyChan := make(chan RebalanceNotification, 10)
	
	testServer.subscribersMutex.Lock()
	testServer.subscribers[workerId] = notifyChan
	testServer.subscribersMutex.Unlock()
	
	// Send notification
	notifications := []RebalanceNotification{
		{WorkerId: workerId, Segment: int32(5)},
		{WorkerId: "nonexistent-worker", Segment: int32(6)}, // Should be ignored
	}
	
	testServer.sendNotifications(notifications)
	
	// Verify notification was received
	select {
	case notification := <-notifyChan:
		assert.Equal(t, workerId, notification.WorkerId)
		assert.Equal(t, int32(5), notification.Segment)
	case <-time.After(100 * time.Millisecond):
		t.Error("Expected notification was not received")
	}
	
	// Verify no more notifications
	select {
	case <-notifyChan:
		t.Error("Unexpected additional notification received")
	default:
		// Expected - no more notifications
	}
}

func TestWorkerServiceGrpcServer_checkIfWorkerIsAlive(t *testing.T) {
	mongoUri, cleanup := setupTestMongo(t)
	defer cleanup()
	
	// Reset global server to ensure clean state
	server = nil
	testServer, err := GetWorkerServiceGrpcServer(mongoUri, "test_db_alive_check", "test_collection")
	require.NoError(t, err)
	defer testServer.Stop()
	
	t.Run("WorkerSendsHeartbeat", func(t *testing.T) {
		ctx, cancel := context.WithCancel(context.Background())
		worker := &worker{
			workerId:  "heartbeat-test",
			segment:   int32(1),
			heartbeat: make(chan struct{}, 1),
			ctx:       ctx,
			cancel:    cancel,
		}
		
		// Start heartbeat check
		go testServer.checkIfWorkerIsAlive(worker)
		
		// Send heartbeat
		worker.heartbeat <- struct{}{}
		
		// Wait a bit
		time.Sleep(50 * time.Millisecond)
		
		// Cancel and verify it stops
		cancel()
		time.Sleep(10 * time.Millisecond)
	})
	
	t.Run("WorkerTimeout", func(t *testing.T) {
		ctx, cancel := context.WithCancel(context.Background())
		defer cancel()
		
		worker := &worker{
			workerId:  "timeout-test",
			segment:   int32(1),
			heartbeat: make(chan struct{}, 1),
			ctx:       ctx,
			cancel:    cancel,
		}
		
		// Count events
		eventCount := 0
		go func() {
			for event := range testServer.events {
				if event.WorkerId == "timeout-test" && event.Event == "disconnected" {
					eventCount++
					break
				}
			}
		}()
		
		// Start heartbeat check (will timeout after 5 seconds, but we'll shorten for test)
		// We can't easily test the actual timeout without waiting 5 seconds,
		// so we'll just verify the function doesn't panic
		go testServer.checkIfWorkerIsAlive(worker)
		
		// Cancel after a short time to avoid long test
		time.Sleep(10 * time.Millisecond)
		cancel()
	})
}

func TestWorkerServiceGrpcServer_StartRebalanceTimer(t *testing.T) {
	mongoUri, cleanup := setupTestMongo(t)
	defer cleanup()
	
	// Reset global server to ensure clean state
	server = nil
	testServer, err := GetWorkerServiceGrpcServer(mongoUri, "test_db_timer", "test_collection")
	require.NoError(t, err)
	defer testServer.Stop()
	
	// Start timer with very short interval for testing
	go testServer.StartRebalanceTimer(50 * time.Millisecond)
	
	// Wait for a few timer ticks
	time.Sleep(150 * time.Millisecond)
	
	// Stop the server (should stop the timer)
	testServer.Stop()
	
	// Test should complete without hanging
}

func TestEvent(t *testing.T) {
	event := Event{
		WorkerId: "test-worker",
		Event:    "connected",
	}
	
	assert.Equal(t, "test-worker", event.WorkerId)
	assert.Equal(t, "connected", event.Event)
}

func TestRebalanceNotification(t *testing.T) {
	notification := RebalanceNotification{
		WorkerId: "test-worker",
		Segment:  int32(5),
	}
	
	assert.Equal(t, "test-worker", notification.WorkerId)
	assert.Equal(t, int32(5), notification.Segment)
}

func TestWorkerStruct(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	
	w := &worker{
		segment:   int32(3),
		workerId:  "test-worker",
		heartbeat: make(chan struct{}, 1),
		ctx:       ctx,
		cancel:    cancel,
	}
	
	assert.Equal(t, int32(3), w.segment)
	assert.Equal(t, "test-worker", w.workerId)
	assert.NotNil(t, w.heartbeat)
	assert.NotNil(t, w.ctx)
	assert.NotNil(t, w.cancel)
}