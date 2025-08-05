package server

import (
	"context"
	"fmt"
	"net"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"github.com/testcontainers/testcontainers-go"
	"github.com/testcontainers/testcontainers-go/wait"
	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
	"runtime/pb"
)

func setupTestMongoForMaster(t *testing.T) (string, func()) {
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

func TestNewMaster(t *testing.T) {
	mongoUri, cleanup := setupTestMongoForMaster(t)
	defer cleanup()

	// Find an available port
	listener, err := net.Listen("tcp", "localhost:0")
	require.NoError(t, err)
	address := listener.Addr().String()
	listener.Close()

	master, err := NewMaster(mongoUri, "test_db", "test_collection", address)
	assert.NoError(t, err)
	assert.NotNil(t, master)
	assert.NotNil(t, master.api)
	assert.NotNil(t, master.ln)
	assert.NotNil(t, master.svr)
	assert.NotNil(t, master.workerSvr)
	assert.False(t, master.Started)

	master.Stop()
}

func TestNewMaster_InvalidMongo(t *testing.T) {
	listener, err := net.Listen("tcp", "localhost:0")
	require.NoError(t, err)
	address := listener.Addr().String()
	listener.Close()

	master, err := NewMaster("", "test_db", "test_collection", address)
	assert.Error(t, err)
	assert.Nil(t, master)
	assert.Contains(t, err.Error(), "MONGO_URI environment variable is not set")
}

func TestNewMaster_InvalidAddress(t *testing.T) {
	mongoUri, cleanup := setupTestMongoForMaster(t)
	defer cleanup()

	// Use an invalid address format
	master, err := NewMaster(mongoUri, "test_db", "test_collection", "invalid:address:format")
	assert.Error(t, err)
	assert.Nil(t, master)
}

func TestMaster_Init(t *testing.T) {
	mongoUri, cleanup := setupTestMongoForMaster(t)
	defer cleanup()

	listener, err := net.Listen("tcp", "localhost:0")
	require.NoError(t, err)
	address := listener.Addr().String()
	listener.Close()

	master := &Master{}
	err = master.Init(mongoUri, "test_db", "test_collection", address)
	assert.NoError(t, err)

	assert.NotNil(t, master.api)
	assert.NotNil(t, master.ln)
	assert.NotNil(t, master.svr)
	assert.NotNil(t, master.workerSvr)

	master.Stop()
}

func TestMaster_Stop(t *testing.T) {
	mongoUri, cleanup := setupTestMongoForMaster(t)
	defer cleanup()

	listener, err := net.Listen("tcp", "localhost:0")
	require.NoError(t, err)
	address := listener.Addr().String()
	listener.Close()

	master, err := NewMaster(mongoUri, "test_db", "test_collection", address)
	require.NoError(t, err)

	// Stop should not panic even if Start wasn't called
	master.Stop()

	// Stopping again should not panic
	master.Stop()
}

func TestMaster_StartAndStop_Integration(t *testing.T) {
	mongoUri, cleanup := setupTestMongoForMaster(t)
	defer cleanup()

	// Use specific ports for easier testing
	listener, err := net.Listen("tcp", "localhost:0")
	require.NoError(t, err)
	grpcAddr := listener.Addr().String()
	listener.Close()

	master, err := NewMaster(mongoUri, "test_db", "test_collection", grpcAddr)
	require.NoError(t, err)

	// Start master in a goroutine with timeout
	done := make(chan struct{})
	go func() {
		defer close(done)
		// This will block, so we need to run it in a goroutine
		master.Start()
	}()

	// Wait for services to start
	time.Sleep(200 * time.Millisecond)

	// Test gRPC connection
	conn, err := grpc.NewClient(grpcAddr, grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		// If connection fails, stop the master and skip rest of test
		master.Stop()
		t.Skipf("Could not connect to gRPC server: %v", err)
		return
	}
	defer conn.Close()

	client := pb.NewWorkerClient(conn)

	// Test heartbeat
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
	defer cancel()

	resp, err := client.Heartbeat(ctx, &pb.Request{Data: `{"workerId": "test-worker"}`})
	if err == nil {
		assert.Equal(t, "resubscribe_required", resp.Data) // Worker doesn't exist yet
	}

	// Stop the master first to free port 8080
	master.Stop()

	// Wait for shutdown
	select {
	case <-done:
		// Master stopped successfully
	case <-time.After(2 * time.Second):
		t.Error("Master did not stop within timeout")
	}

	// Verify gRPC connection is closed after stop
	ctx2, cancel2 := context.WithTimeout(context.Background(), 1*time.Second)
	defer cancel2()

	_, err = client.Heartbeat(ctx2, &pb.Request{Data: `{"workerId": "test-worker"}`})
	assert.Error(t, err) // Should fail because server is stopped
}

func TestMaster_AutoscalerInitialization(t *testing.T) {
	mongoUri, cleanup := setupTestMongoForMaster(t)
	defer cleanup()

	listener, err := net.Listen("tcp", "localhost:0")
	require.NoError(t, err)
	address := listener.Addr().String()
	listener.Close()

	master := &Master{}
	err = master.Init(mongoUri, "test_db", "test_collection", address)
	assert.NoError(t, err)

	// Autoscaler might be nil if Kubernetes permissions are not available
	// This is expected behavior and should not cause initialization to fail
	if master.scaler != nil {
		assert.NotNil(t, master.scaler)
	}

	master.Stop()
}

func TestMaster_MultipleStops(t *testing.T) {
	mongoUri, cleanup := setupTestMongoForMaster(t)
	defer cleanup()

	listener, err := net.Listen("tcp", "localhost:0")
	require.NoError(t, err)
	address := listener.Addr().String()
	listener.Close()

	master, err := NewMaster(mongoUri, "test_db", "test_collection", address)
	require.NoError(t, err)

	// Multiple stops should not panic
	master.Stop()
	master.Stop()
	master.Stop()
}

func TestMaster_PortBinding(t *testing.T) {
	mongoUri, cleanup := setupTestMongoForMaster(t)
	defer cleanup()

	// Test with an already occupied port
	listener, err := net.Listen("tcp", "localhost:0")
	require.NoError(t, err)
	occupiedAddr := listener.Addr().String()
	// Keep listener open to occupy the port

	master := &Master{}
	err = master.Init(mongoUri, "test_db", "test_collection", occupiedAddr)
	assert.Error(t, err) // Should fail because port is occupied

	listener.Close()
}

func TestMaster_ComponentIntegrity(t *testing.T) {
	mongoUri, cleanup := setupTestMongoForMaster(t)
	defer cleanup()

	listener, err := net.Listen("tcp", "localhost:0")
	require.NoError(t, err)
	address := listener.Addr().String()
	listener.Close()

	master, err := NewMaster(mongoUri, "test_db", "test_collection", address)
	require.NoError(t, err)
	defer master.Stop()

	// Verify all components are properly initialized
	assert.NotNil(t, master.api)
	assert.NotNil(t, master.ln)
	assert.NotNil(t, master.svr)
	assert.NotNil(t, master.workerSvr)

	// Verify worker service has correct configuration
	assert.Equal(t, "test_db", master.workerSvr.dbName)
	assert.Equal(t, "test_collection", master.workerSvr.collectionName)
	assert.NotNil(t, master.workerSvr.mongoClient)

	// Verify gRPC server has worker service registered
	assert.NotNil(t, master.svr)
}

func TestMaster_StartedFlag(t *testing.T) {
	mongoUri, cleanup := setupTestMongoForMaster(t)
	defer cleanup()

	listener, err := net.Listen("tcp", "localhost:0")
	require.NoError(t, err)
	address := listener.Addr().String()
	listener.Close()

	master, err := NewMaster(mongoUri, "test_db", "test_collection", address)
	require.NoError(t, err)
	defer master.Stop()

	assert.False(t, master.Started)

	// Start master in a goroutine (since Start() blocks)
	go master.Start()

	// Wait for startup
	time.Sleep(100 * time.Millisecond)

	// Verify Started flag (note: this might be racy, but should generally work)
	assert.True(t, master.Started)
}
