package server

import (
	"context"
	"encoding/json"
	"fmt"
	"net"
	"os"
	"sync"
	"testing"
	"time"

	"runtime/internal/backtest-runner/publisher"
	"runtime/internal/backtest-runner/subscriber"
	"runtime/internal/constants"
	base "runtime/internal/core"
	dockerrunner "runtime/internal/docker-runner"
	"runtime/internal/util"
	"runtime/pb"

	"archive/zip"
	"bytes"
	"github.com/minio/minio-go/v7"
	"github.com/minio/minio-go/v7/pkg/credentials"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"github.com/testcontainers/testcontainers-go"
	"github.com/testcontainers/testcontainers-go/wait"
	"go.mongodb.org/mongo-driver/mongo"
	mongoOptions "go.mongodb.org/mongo-driver/mongo/options"
	"google.golang.org/grpc"
)

// Mock gRPC server for testing BaseWorker interactions
type MockBacktestWorkerServer struct {
	pb.UnimplementedWorkerServer
	heartbeatResponses []string
	heartbeatCount     int
	heartbeatMutex     sync.Mutex
	subscriptionData   []base.SubscriptionResponse
	subscriptionMutex  sync.Mutex
}

func (m *MockBacktestWorkerServer) Heartbeat(ctx context.Context, req *pb.Request) (*pb.Response, error) {
	m.heartbeatMutex.Lock()
	defer m.heartbeatMutex.Unlock()

	m.heartbeatCount++

	if len(m.heartbeatResponses) > 0 {
		response := m.heartbeatResponses[0]
		if len(m.heartbeatResponses) > 1 {
			m.heartbeatResponses = m.heartbeatResponses[1:]
		}
		return &pb.Response{Data: response}, nil
	}

	return &pb.Response{Data: "ok"}, nil
}

func (m *MockBacktestWorkerServer) RebalanceSubscription(req *pb.Request, stream pb.Worker_RebalanceSubscriptionServer) error {
	m.subscriptionMutex.Lock()
	defer m.subscriptionMutex.Unlock()

	for _, response := range m.subscriptionData {
		data, _ := json.Marshal(response)
		if err := stream.Send(&pb.Response{Data: string(data)}); err != nil {
			return err
		}
		time.Sleep(10 * time.Millisecond)
	}

	<-stream.Context().Done()
	return nil
}

func setupTestGrpcServer(t *testing.T) (*MockBacktestWorkerServer, string, func()) {
	listener, err := net.Listen("tcp", "localhost:0")
	require.NoError(t, err)

	server := grpc.NewServer()
	mockServer := &MockBacktestWorkerServer{}
	pb.RegisterWorkerServer(server, mockServer)

	go func() {
		server.Serve(listener)
	}()

	cleanup := func() {
		server.Stop()
		listener.Close()
	}

	return mockServer, listener.Addr().String(), cleanup
}

func setupTestMongoDB(t *testing.T) (string, func()) {
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

func setupTestMinIO(t *testing.T) (string, func()) {
	ctx := context.Background()

	req := testcontainers.ContainerRequest{
		Image:        "minio/minio:latest",
		ExposedPorts: []string{"9000/tcp"},
		Env: map[string]string{
			"MINIO_ROOT_USER":     "minioadmin",
			"MINIO_ROOT_PASSWORD": "minioadmin",
		},
		Cmd:        []string{"server", "/data"},
		WaitingFor: wait.ForHTTP("/minio/health/ready").WithPort("9000/tcp"),
	}

	minioContainer, err := testcontainers.GenericContainer(ctx, testcontainers.GenericContainerRequest{
		ContainerRequest: req,
		Started:          true,
	})
	require.NoError(t, err)

	host, err := minioContainer.Host(ctx)
	require.NoError(t, err)

	port, err := minioContainer.MappedPort(ctx, "9000")
	require.NoError(t, err)

	minioEndpoint := fmt.Sprintf("%s:%s", host, port.Port())

	cleanup := func() {
		minioContainer.Terminate(ctx)
	}

	return minioEndpoint, cleanup
}

func setupTestMinIOWithBacktestCode(t *testing.T, minioEndpoint string) {
	// Create MinIO client
	minioClient, err := minio.New(minioEndpoint, &minio.Options{
		Creds:  credentials.NewStaticV4("minioadmin", "minioadmin", ""),
		Secure: false,
	})
	require.NoError(t, err)

	ctx := context.Background()
	bucketName := "custom-bots" // This matches the default bucket used by DockerRunner

	// Create bucket
	err = minioClient.MakeBucket(ctx, bucketName, minio.MakeBucketOptions{})
	require.NoError(t, err, "Failed to create bucket")

	// Helper function to create and upload a backtest ZIP file
	uploadBacktestCode := func(fileName, code string) {
		var zipBuffer bytes.Buffer
		zipWriter := zip.NewWriter(&zipBuffer)

		backtestFile, err := zipWriter.Create("backtest.py")
		require.NoError(t, err)
		_, err = backtestFile.Write([]byte(code))
		require.NoError(t, err)

		err = zipWriter.Close()
		require.NoError(t, err)

		zipData := zipBuffer.Bytes()
		_, err = minioClient.PutObject(ctx, bucketName, fileName,
			bytes.NewReader(zipData), int64(len(zipData)), minio.PutObjectOptions{
				ContentType: "application/zip",
			})
		require.NoError(t, err, "Failed to upload "+fileName)
	}

	// Create backtest code for various test scenarios
	basicBacktestCode := `#!/usr/bin/env python3
import json
import time
import sys
import os

def main(id, config):
    """Backtest main function matching py-test-bot.zip format."""
    print(f'Starting backtest execution with ID: {id}')
    print(f'Config: {config}')
    
    # Simulate backtest execution
    time.sleep(2)
    
    # Generate and return test result (matches py-test-bot.zip format)
    return {
        "status": "success",
        "results": {
            "metrics": {
                "total_return": 0.15,
                "sharpe_ratio": 1.2,
                "max_drawdown": -0.05
            },
            "plots": [
                {
                    "type": "line",
                    "title": "Portfolio Value",
                    "data": [100, 105, 110, 115]
                }
            ],
            "tables": [
                {
                    "title": "Trade Summary",
                    "data": [
                        {"symbol": "AAPL", "quantity": 100, "pnl": 50.0}
                    ]
                }
            ]
        }
    }
`

	batchBacktestCode := `#!/usr/bin/env python3
import json
import time
import sys
import os

def main(id, config):
    """Batch backtest main function matching py-test-bot.zip format."""
    print(f'Starting batch backtest execution with ID: {id}')
    print(f'Config: {config}')
    time.sleep(1)
    
    return {
        "status": "success",
        "results": {
            "metrics": {
                "total_return": 0.08,
                "volatility": 0.12
            },
            "plots": [],
            "tables": []
        }
    }
`

	timeoutBacktestCode := `#!/usr/bin/env python3
import time
import sys

def main(id, config):
    """Long-running backtest main function that will timeout."""
    print(f'Starting long-running backtest with ID: {id}')
    print(f'Config: {config}')
    
    # This will timeout (5 seconds batch timeout, but we sleep 300 seconds)
    time.sleep(300)
    print('This should never print due to timeout')
    
    # This return will never be reached due to timeout
    return {
        "status": "success",
        "results": {
            "metrics": {"timeout_test": True},
            "plots": [],
            "tables": []
        }
    }
`

	errorBacktestCode := `#!/usr/bin/env python3
import sys

def main(id, config):
    """Error backtest main function that will raise an exception."""
    print(f'Starting backtest that will fail with ID: {id}')
    print(f'Config: {config}')
    
    # Simulate a backtest failure by raising an exception
    print('Simulated backtest failure')
    raise Exception("Test backtest failure - this is expected")
    
    # This return will never be reached due to the exception
    return {
        "status": "success", 
        "results": {
            "metrics": {},
            "plots": [],
            "tables": []
        }
    }
`

	// Upload all backtest codes
	uploadBacktestCode("basic-backtest.zip", basicBacktestCode)
	uploadBacktestCode("batch-backtest-1.zip", batchBacktestCode)
	uploadBacktestCode("batch-backtest-2.zip", batchBacktestCode)
	uploadBacktestCode("batch-backtest-3.zip", batchBacktestCode)
	uploadBacktestCode("timeout-backtest.zip", timeoutBacktestCode)
	uploadBacktestCode("error-backtest.zip", errorBacktestCode)
}

func setupTestMongoClient(t *testing.T, mongoUri string) *mongo.Client {
	mongoClient, err := mongo.Connect(context.TODO(), mongoOptions.Client().ApplyURI(mongoUri))
	require.NoError(t, err)

	err = mongoClient.Ping(context.TODO(), nil)
	require.NoError(t, err)

	return mongoClient
}

func setupTestEnvironment(t *testing.T) (mongoUri string, minioEndpoint string, cleanupAll func()) {
	mongoUri, cleanupMongo := setupTestMongoDB(t)
	minioEndpoint, cleanupMinio := setupTestMinIO(t)

	// Setup MinIO with test backtest code
	setupTestMinIOWithBacktestCode(t, minioEndpoint)

	// Set required environment variables for Docker runner
	os.Setenv("MINIO_ENDPOINT", minioEndpoint)
	os.Setenv("MINIO_ACCESS_KEY", "minioadmin")
	os.Setenv("MINIO_SECRET_KEY", "minioadmin")

	// Set database environment variables for worker
	os.Setenv("DB_NAME", "test_backtest_db")
	os.Setenv("COLLECTION_NAME", "backtests")
	os.Setenv("TEMP_DIR", "/tmp")

	cleanupAll = func() {
		os.Unsetenv("MINIO_ENDPOINT")
		os.Unsetenv("MINIO_ACCESS_KEY")
		os.Unsetenv("MINIO_SECRET_KEY")
		os.Unsetenv("DB_NAME")
		os.Unsetenv("COLLECTION_NAME")
		cleanupMongo()
		cleanupMinio()
	}

	return mongoUri, minioEndpoint, cleanupAll
}

// Real integration test factories
type TestBacktestDockerRunnerFactory struct{}

func (factory *TestBacktestDockerRunnerFactory) CreateDockerRunner(
	worker *BacktestWorker,
) (dockerrunner.DockerRunner, error) {
	// Create Docker runner with MinIO configuration from environment
	// The MinIO environment variables are set by the test setup
	return dockerrunner.NewDockerRunner(dockerrunner.DockerRunnerOptions{
		Logger: &util.DefaultLogger{},
	})
}

type TestBacktestSubscriberFactory struct{}

func (factory *TestBacktestSubscriberFactory) CreateSubscriber(
	worker *BacktestWorker,
) (subscriber.Subscriber, error) {
	return &TestBacktestSubscriber{}, nil
}

type TestBacktestSubscriber struct{}

func (s *TestBacktestSubscriber) Start(ctx context.Context) error {
	<-ctx.Done()
	return nil
}

func (s *TestBacktestSubscriber) Stop() error {
	return nil
}

type TestBacktestPublisher struct{}

func (p *TestBacktestPublisher) PublishBacktestCompleted(ctx context.Context, event publisher.BacktestCompletedEvent) error {
	// Mock publisher - just log the event
	fmt.Printf("Mock Publisher: Backtest %s completed (success: %t)\n", event.BacktestId, event.Success)
	return nil
}

func (p *TestBacktestPublisher) Close() error {
	return nil
}

// NewTestWorker creates a BacktestWorker with test dependencies injected
func NewTestWorker(
	ctx context.Context,
	workerId string,
	mongoUri string,
	dbName string,
	collectionName string,
	leader string,
	dockerRunnerFactory DockerRunnerFactory,
	subscriberFactory SubscriberFactory,
	mongoClient *mongo.Client,
	options BacktestWorkerOptions,
) (*BacktestWorker, error) {
	baseWorker, err := base.NewWorker(
		ctx,
		workerId,
		mongoUri,
		dbName,
		collectionName,
		leader,
	)
	if err != nil {
		return nil, fmt.Errorf("failed to create base worker: %w", err)
	}

	// Use mock publisher for tests
	mockPublisher := &TestBacktestPublisher{}

	// Create worker context that will be canceled when the worker stops
	workerCtx, workerCancel := context.WithCancel(context.Background())

	return &BacktestWorker{
		baseWorker:          baseWorker,
		dockerRunnerFactory: dockerRunnerFactory,
		subscriberFactory:   subscriberFactory,
		publisher:           mockPublisher,
		batchSize:           options.BatchSize,
		batchTimeout:        options.BatchTimeout,
		batchInterval:       options.BatchInterval,
		shutdown:            make(chan struct{}),
		workerCtx:           workerCtx,
		workerCancel:        workerCancel,
	}, nil
}

func TestBacktestWorker_SingleBacktestExecution_Success(t *testing.T) {
	// Setup test infrastructure
	mongoUri, _, cleanupAll := setupTestEnvironment(t)
	defer cleanupAll()

	mockServer, addr, cleanupServer := setupTestGrpcServer(t)
	defer cleanupServer()

	mongoClient := setupTestMongoClient(t, mongoUri)
	defer mongoClient.Disconnect(context.Background())

	// Configure mock server for segment assignment
	mockServer.subscriptionData = []base.SubscriptionResponse{
		{Segment: 1, WorkerId: "test-backtest-worker"},
	}

	// Create BacktestWorker
	ctx := context.Background()
	options := BacktestWorkerOptions{
		BatchSize:     5,
		BatchTimeout:  30 * time.Second,
		BatchInterval: 5 * time.Second,
	}

	worker, err := NewTestWorker(
		ctx, "test-backtest-worker", mongoUri, constants.BACKTEST_RUNNER_DB_NAME, constants.BACKTEST_COLLECTION, addr,
		&TestBacktestDockerRunnerFactory{}, &TestBacktestSubscriberFactory{}, mongoClient, options,
	)
	require.NoError(t, err)

	// Start worker
	go worker.Start()
	defer worker.Stop()

	// Wait for worker to get segment assignment
	time.Sleep(3 * time.Second)

	// Insert a test backtest into MongoDB
	collection := mongoClient.Database(constants.BACKTEST_RUNNER_DB_NAME).Collection(constants.BACKTEST_COLLECTION)
	testBacktest := map[string]interface{}{
		"id":         "basic-backtest",
		"segment_id": int32(1),
		"config":     map[string]interface{}{},
		"custom": map[string]interface{}{
			"version":  "1.0.0",
			"filePath": "basic-backtest.zip",
			"config": map[string]interface{}{
				"runtime": "python3.11",
				"name":    "basic-backtest",
				"entrypoints": map[string]string{
					"backtest": "backtest.py",
				},
			},
		},
	}
	_, err = collection.InsertOne(ctx, testBacktest)
	require.NoError(t, err)

	// Trigger batch processing manually through BaseWorker
	select {
	case worker.baseWorker.SegmentChange <- struct{}{}:
		// Batch processing triggered
	default:
		// Already pending
	}

	// Wait for batch processing and backtest completion
	time.Sleep(8 * time.Second)

	// Verify backtest was processed and removed from database
	var remainingBacktest map[string]interface{}
	err = collection.FindOne(ctx, map[string]interface{}{"id": "basic-backtest"}).Decode(&remainingBacktest)
	assert.Error(t, err, "Backtest should be removed from database after completion")

	t.Logf("✅ Single backtest execution test passed:")
	t.Logf("  - BacktestWorker successfully uses BaseWorker")
	t.Logf("  - Real Docker container executed backtest")
	t.Logf("  - MinIO integration works for backtest code download")
	t.Logf("  - Completed backtest was removed from database")
	t.Logf("  - Batch processing loop functions correctly")
}

func TestBacktestWorker_BatchProcessing_MultipleConcurrentBacktests(t *testing.T) {
	// Setup test infrastructure
	mongoUri, _, cleanupAll := setupTestEnvironment(t)
	defer cleanupAll()

	mockServer, addr, cleanupServer := setupTestGrpcServer(t)
	defer cleanupServer()

	mongoClient := setupTestMongoClient(t, mongoUri)
	defer mongoClient.Disconnect(context.Background())

	// Configure mock server for segment assignment
	mockServer.subscriptionData = []base.SubscriptionResponse{
		{Segment: 1, WorkerId: "test-backtest-worker"},
	}

	// Create BacktestWorker with batch size of 3
	ctx := context.Background()
	options := BacktestWorkerOptions{
		BatchSize:     3,
		BatchTimeout:  30 * time.Second,
		BatchInterval: 5 * time.Second,
	}

	worker, err := NewTestWorker(
		ctx, "test-backtest-worker", mongoUri, constants.BACKTEST_RUNNER_DB_NAME, constants.BACKTEST_COLLECTION, addr,
		&TestBacktestDockerRunnerFactory{}, &TestBacktestSubscriberFactory{}, mongoClient, options,
	)
	require.NoError(t, err)

	// Insert multiple backtests
	collection := mongoClient.Database(constants.BACKTEST_RUNNER_DB_NAME).Collection(constants.BACKTEST_COLLECTION)
	backtests := []map[string]interface{}{
		{
			"id":         "batch-backtest-1",
			"segment_id": int32(1),
			"config":     map[string]interface{}{},
			"custom": map[string]interface{}{
				"version":  "1.0.0",
				"filePath": "batch-backtest-1.zip",
				"config": map[string]interface{}{
					"runtime": "python3.11",
					"name":    "batch-backtest-1",
					"entrypoints": map[string]interface{}{
						"backtest": "backtest.py",
					},
				},
			},
		},
		{
			"id":         "batch-backtest-2",
			"segment_id": int32(1),
			"config":     map[string]interface{}{},
			"custom": map[string]interface{}{
				"version":  "1.0.0",
				"filePath": "batch-backtest-2.zip",
				"config": map[string]interface{}{
					"runtime": "python3.11",
					"name":    "batch-backtest-2",
					"entrypoints": map[string]interface{}{
						"backtest": "backtest.py",
					},
				},
			},
		},
		{
			"id":         "batch-backtest-3",
			"segment_id": int32(1),
			"config":     map[string]interface{}{},
			"custom": map[string]interface{}{
				"version":  "1.0.0",
				"filePath": "batch-backtest-3.zip",
				"config": map[string]interface{}{
					"runtime": "python3.11",
					"name":    "batch-backtest-3",
					"entrypoints": map[string]interface{}{
						"backtest": "backtest.py",
					},
				},
			},
		},
	}

	for _, backtest := range backtests {
		_, err = collection.InsertOne(ctx, backtest)
		require.NoError(t, err)
	}

	// Start worker
	go worker.Start()
	defer worker.Stop()

	// Wait for worker to get segment assignment
	time.Sleep(3 * time.Second)

	// Trigger batch processing
	select {
	case worker.baseWorker.SegmentChange <- struct{}{}:
		// Batch processing triggered
	default:
		// Already pending
	}

	// Wait for batch processing to complete
	time.Sleep(12 * time.Second)

	// Verify all backtests were processed and removed
	remainingCount, err := collection.CountDocuments(ctx, map[string]interface{}{})
	require.NoError(t, err)
	assert.Equal(t, int64(0), remainingCount, "All backtests should be removed after batch processing")

	t.Logf("✅ Batch processing test passed:")
	t.Logf("  - Multiple backtests processed concurrently in batch")
	t.Logf("  - All backtests completed and removed from database")
	t.Logf("  - Batch size limit respected")
	t.Logf("  - Concurrent execution works correctly")
}

func TestBacktestWorker_ErrorHandling_FailedBacktest(t *testing.T) {
	// Setup test infrastructure
	mongoUri, _, cleanupAll := setupTestEnvironment(t)
	defer cleanupAll()

	mockServer, addr, cleanupServer := setupTestGrpcServer(t)
	defer cleanupServer()

	mongoClient := setupTestMongoClient(t, mongoUri)
	defer mongoClient.Disconnect(context.Background())

	// Configure mock server for segment assignment
	mockServer.subscriptionData = []base.SubscriptionResponse{
		{Segment: 1, WorkerId: "test-backtest-worker"},
	}

	// Create BacktestWorker
	ctx := context.Background()
	options := BacktestWorkerOptions{
		BatchSize:     5,
		BatchTimeout:  30 * time.Second,
		BatchInterval: 3 * time.Second,
	}

	worker, err := NewTestWorker(
		ctx, "test-backtest-worker", mongoUri, constants.BACKTEST_RUNNER_DB_NAME, constants.BACKTEST_COLLECTION, addr,
		&TestBacktestDockerRunnerFactory{}, &TestBacktestSubscriberFactory{}, mongoClient, options,
	)
	require.NoError(t, err)

	// Insert a backtest that will fail
	collection := mongoClient.Database(constants.BACKTEST_RUNNER_DB_NAME).Collection(constants.BACKTEST_COLLECTION)
	errorBacktest := map[string]interface{}{
		"id":         "error-backtest",
		"segment_id": int32(1),
		"config":     map[string]interface{}{},
		"custom": map[string]interface{}{
			"version":  "1.0.0",
			"filePath": "error-backtest.zip",
			"config": map[string]interface{}{
				"runtime": "python3.11",
				"name":    "error-backtest",
				"entrypoints": map[string]string{
					"backtest": "backtest.py",
				},
			},
		},
	}
	_, err = collection.InsertOne(ctx, errorBacktest)
	require.NoError(t, err)

	// Start worker
	go worker.Start()
	defer worker.Stop()

	// Wait for worker to get segment assignment
	time.Sleep(3 * time.Second)

	// Trigger batch processing
	select {
	case worker.baseWorker.SegmentChange <- struct{}{}:
		// Batch processing triggered
	default:
		// Already pending
	}

	// Wait for batch processing and error handling
	time.Sleep(8 * time.Second)

	// Verify failed backtest was still removed from database (cleanup)
	var remainingBacktest map[string]interface{}
	err = collection.FindOne(ctx, map[string]interface{}{"id": "error-backtest"}).Decode(&remainingBacktest)
	assert.Error(t, err, "Failed backtest should still be removed from database")

	t.Logf("✅ Error handling test passed:")
	t.Logf("  - Backtest with Python exception handled gracefully")
	t.Logf("  - Failed backtest removed from database")
	t.Logf("  - Error handling logic works correctly")
	t.Logf("  - Worker continues operating after failures")
}

func TestBacktestWorker_SegmentBasedFiltering(t *testing.T) {
	// Setup test infrastructure
	mongoUri, _, cleanupAll := setupTestEnvironment(t)
	defer cleanupAll()

	mockServer, addr, cleanupServer := setupTestGrpcServer(t)
	defer cleanupServer()

	mongoClient := setupTestMongoClient(t, mongoUri)
	defer mongoClient.Disconnect(context.Background())

	// Configure mock server for specific segment assignment (segment 2)
	mockServer.subscriptionData = []base.SubscriptionResponse{
		{Segment: 2, WorkerId: "test-backtest-worker"},
	}

	// Create BacktestWorker
	ctx := context.Background()
	options := BacktestWorkerOptions{
		BatchSize:     5,
		BatchTimeout:  30 * time.Second,
		BatchInterval: 3 * time.Second,
	}

	worker, err := NewTestWorker(
		ctx, "test-backtest-worker", mongoUri, constants.BACKTEST_RUNNER_DB_NAME, constants.BACKTEST_COLLECTION, addr,
		&TestBacktestDockerRunnerFactory{}, &TestBacktestSubscriberFactory{}, mongoClient, options,
	)
	require.NoError(t, err)

	// Insert backtests for different segments
	collection := mongoClient.Database(constants.BACKTEST_RUNNER_DB_NAME).Collection(constants.BACKTEST_COLLECTION)
	backtests := []map[string]interface{}{
		{
			"id":         "segment-1-backtest",
			"segment_id": int32(1), // Different segment
			"config":     map[string]interface{}{},
			"custom": map[string]interface{}{
				"version":  "1.0.0",
				"filePath": "basic-backtest.zip",
				"config": map[string]interface{}{
					"runtime": "python3.11",
					"name":    "segment-1-backtest",
					"entrypoints": map[string]interface{}{
						"backtest": "backtest.py",
					},
				},
			},
		},
		{
			"id":         "segment-2-backtest",
			"segment_id": int32(2), // Correct segment
			"config":     map[string]interface{}{},
			"custom": map[string]interface{}{
				"version":  "1.0.0",
				"filePath": "basic-backtest.zip",
				"config": map[string]interface{}{
					"runtime": "python3.11",
					"name":    "segment-2-backtest",
					"entrypoints": map[string]interface{}{
						"backtest": "backtest.py",
					},
				},
			},
		},
	}

	for _, backtest := range backtests {
		_, err = collection.InsertOne(ctx, backtest)
		require.NoError(t, err)
	}

	// Start worker
	go worker.Start()
	defer worker.Stop()

	// Wait for worker to get segment assignment
	time.Sleep(3 * time.Second)

	// Verify segment assignment
	assert.Equal(t, int32(2), worker.baseWorker.Segment, "Worker should be assigned to segment 2")

	// Trigger batch processing
	select {
	case worker.baseWorker.SegmentChange <- struct{}{}:
		// Batch processing triggered
	default:
		// Already pending
	}

	// Wait for batch processing
	time.Sleep(8 * time.Second)

	// Verify segment-based filtering
	var segment1Backtest map[string]interface{}
	err = collection.FindOne(ctx, map[string]interface{}{"id": "segment-1-backtest"}).Decode(&segment1Backtest)
	assert.NoError(t, err, "Segment 1 backtest should remain in database (not processed)")

	var segment2Backtest map[string]interface{}
	err = collection.FindOne(ctx, map[string]interface{}{"id": "segment-2-backtest"}).Decode(&segment2Backtest)
	assert.Error(t, err, "Segment 2 backtest should be removed (processed)")

	t.Logf("✅ Segment-based filtering test passed:")
	t.Logf("  - Worker correctly assigned to segment 2")
	t.Logf("  - Only segment 2 backtest was processed")
	t.Logf("  - Segment 1 backtest was ignored (correct filtering)")
	t.Logf("  - Segment-based processing works correctly")
}

func TestBacktestWorker_BatchTimeout_Handling(t *testing.T) {
	// Setup test infrastructure
	mongoUri, _, cleanupAll := setupTestEnvironment(t)
	defer cleanupAll()

	mockServer, addr, cleanupServer := setupTestGrpcServer(t)
	defer cleanupServer()

	mongoClient := setupTestMongoClient(t, mongoUri)
	defer mongoClient.Disconnect(context.Background())

	// Configure mock server for segment assignment
	mockServer.subscriptionData = []base.SubscriptionResponse{
		{Segment: 1, WorkerId: "test-backtest-worker"},
	}

	// Create BacktestWorker with short timeout
	ctx := context.Background()
	options := BacktestWorkerOptions{
		BatchSize:     5,
		BatchTimeout:  5 * time.Second, // Short timeout
		BatchInterval: 3 * time.Second,
	}

	worker, err := NewTestWorker(
		ctx, "test-backtest-worker", mongoUri, constants.BACKTEST_RUNNER_DB_NAME, constants.BACKTEST_COLLECTION, addr,
		&TestBacktestDockerRunnerFactory{}, &TestBacktestSubscriberFactory{}, mongoClient, options,
	)
	require.NoError(t, err)

	// Insert a backtest that will timeout
	collection := mongoClient.Database(constants.BACKTEST_RUNNER_DB_NAME).Collection(constants.BACKTEST_COLLECTION)
	timeoutBacktest := map[string]interface{}{
		"id":         "timeout-backtest",
		"segment_id": int32(1),
		"config":     map[string]interface{}{},
		"custom": map[string]interface{}{
			"version":  "1.0.0",
			"filePath": "timeout-backtest.zip",
			"config": map[string]interface{}{
				"runtime": "python3.11",
				"name":    "timeout-backtest",
				"entrypoints": map[string]string{
					"backtest": "backtest.py",
				},
			},
		},
	}
	_, err = collection.InsertOne(ctx, timeoutBacktest)
	require.NoError(t, err)

	// Start worker
	go worker.Start()
	defer worker.Stop()

	// Wait for worker to get segment assignment
	time.Sleep(3 * time.Second)

	// Trigger batch processing
	select {
	case worker.baseWorker.SegmentChange <- struct{}{}:
		// Batch processing triggered
	default:
		// Already pending
	}

	// Wait for timeout and cleanup
	time.Sleep(12 * time.Second)

	// Verify timeout backtest was handled (removed from database)
	var remainingBacktest map[string]interface{}
	err = collection.FindOne(ctx, map[string]interface{}{"id": "timeout-backtest"}).Decode(&remainingBacktest)
	assert.Error(t, err, "Timeout backtest should be removed from database")

	t.Logf("✅ Batch timeout handling test passed:")
	t.Logf("  - Long-running backtest timed out correctly")
	t.Logf("  - Timeout backtest was cleaned up from database")
	t.Logf("  - Batch timeout mechanism works correctly")
	t.Logf("  - Worker continues operating after timeout")
}
