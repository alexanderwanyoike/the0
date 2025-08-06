package server

import (
	"context"
	"encoding/json"
	"fmt"
	"net"
	"runtime/pb"
	"sync"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"github.com/testcontainers/testcontainers-go"
	"github.com/testcontainers/testcontainers-go/wait"
	"google.golang.org/grpc"
)

// Mock gRPC server for testing
type MockWorkerServer struct {
	pb.UnimplementedWorkerServer
	heartbeatResponses []string
	heartbeatCount     int
	heartbeatMutex     sync.Mutex
	subscriptionData   []SubscriptionResponse
	subscriptionIndex  int
	subscriptionMutex  sync.Mutex
}

func (m *MockWorkerServer) Heartbeat(ctx context.Context, req *pb.Request) (*pb.Response, error) {
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

func (m *MockWorkerServer) RebalanceSubscription(req *pb.Request, stream pb.Worker_RebalanceSubscriptionServer) error {
	m.subscriptionMutex.Lock()
	defer m.subscriptionMutex.Unlock()

	// Send subscription responses
	for _, response := range m.subscriptionData {
		data, _ := json.Marshal(response)
		if err := stream.Send(&pb.Response{Data: string(data)}); err != nil {
			return err
		}
		time.Sleep(10 * time.Millisecond) // Small delay between messages
	}

	// Keep stream open
	<-stream.Context().Done()
	return nil
}

func (m *MockWorkerServer) GetHeartbeatCount() int {
	m.heartbeatMutex.Lock()
	defer m.heartbeatMutex.Unlock()
	return m.heartbeatCount
}

func setupMockServer(t *testing.T) (*MockWorkerServer, string, func()) {
	listener, err := net.Listen("tcp", "localhost:0")
	if err != nil {
		t.Fatal(err)
	}

	server := grpc.NewServer()
	mockServer := &MockWorkerServer{}
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

func setupMongoContainer(t *testing.T) (string, func()) {
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

func TestWorker_Heartbeat_Success(t *testing.T) {
	mockServer, addr, cleanupServer := setupMockServer(t)
	defer cleanupServer()

	mongoUri, cleanupMongo := setupMongoContainer(t)
	defer cleanupMongo()

	ctx := context.Background()
	worker, err := NewWorker(ctx, "test-worker", mongoUri, "test_db", "test_collection", addr)
	require.NoError(t, err)
	defer worker.Stop()

	// Start heartbeat in goroutine
	go worker.SendHeartbeat()

	// Wait for a few heartbeats
	time.Sleep(200 * time.Millisecond)

	// Check that heartbeats were sent
	assert.Greater(t, mockServer.GetHeartbeatCount(), 0)
}

func TestWorker_Heartbeat_ResubscribeRequest(t *testing.T) {
	mockServer, addr, cleanupServer := setupMockServer(t)
	defer cleanupServer()

	mongoUri, cleanupMongo := setupMongoContainer(t)
	defer cleanupMongo()

	// Configure server to request resubscription
	mockServer.heartbeatResponses = []string{"resubscribe_required", "ok"}
	mockServer.subscriptionData = []SubscriptionResponse{
		{Segment: 5, WorkerId: "test-worker"},
	}

	ctx := context.Background()
	worker, err := NewWorker(ctx, "test-worker", mongoUri, "test_db", "test_collection", addr)
	require.NoError(t, err)
	defer worker.Stop()

	// Start heartbeat
	go worker.SendHeartbeat()

	// Wait for heartbeat and resubscription
	time.Sleep(300 * time.Millisecond)

	// Verify segment was updated through resubscription
	assert.Equal(t, int32(5), worker.Segment)
}

func TestWorker_Subscription_SegmentAssignment(t *testing.T) {
	mockServer, addr, cleanupServer := setupMockServer(t)
	defer cleanupServer()

	mongoUri, cleanupMongo := setupMongoContainer(t)
	defer cleanupMongo()

	// Configure subscription responses - just send one to avoid race conditions
	mockServer.subscriptionData = []SubscriptionResponse{
		{Segment: 3, WorkerId: "test-worker"},
	}

	ctx := context.Background()
	worker, err := NewWorker(ctx, "test-worker", mongoUri, "test_db", "test_collection", addr)
	require.NoError(t, err)
	defer worker.Stop()

	// Start subscription
	go worker.Subscribe()

	// Wait for subscription responses
	time.Sleep(100 * time.Millisecond)

	// Check segment was updated
	assert.Equal(t, int32(3), worker.Segment)

	// Check segment change was triggered
	select {
	case <-worker.SegmentChange:
		// Expected - segment change was triggered
	case <-time.After(50 * time.Millisecond):
		t.Error("SegmentChange should have been triggered")
	}
}

func TestWorker_Subscription_InvalidJSON(t *testing.T) {
	// Create custom mock server for invalid JSON test
	listener, err := net.Listen("tcp", "localhost:0")
	require.NoError(t, err)
	defer listener.Close()

	server := grpc.NewServer()
	defer server.Stop()

	// Override RebalanceSubscription to send invalid JSON
	pb.RegisterWorkerServer(server, &mockWorkerServerInvalidJSON{})

	go func() {
		server.Serve(listener)
	}()

	addr := listener.Addr().String()

	mongoUri, cleanupMongo := setupMongoContainer(t)
	defer cleanupMongo()

	ctx := context.Background()
	worker, err := NewWorker(ctx, "test-worker", mongoUri, "test_db", "test_collection", addr)
	require.NoError(t, err)
	defer worker.Stop()

	// Start subscription - should handle invalid JSON gracefully
	go worker.Subscribe()

	// Wait and verify worker doesn't crash
	time.Sleep(100 * time.Millisecond)

	// Segment should remain default since JSON was invalid
	assert.Equal(t, int32(-1), worker.Segment)
}

type mockWorkerServerInvalidJSON struct {
	pb.UnimplementedWorkerServer
}

func (m *mockWorkerServerInvalidJSON) RebalanceSubscription(req *pb.Request, stream pb.Worker_RebalanceSubscriptionServer) error {
	// Send invalid JSON
	stream.Send(&pb.Response{Data: "{invalid json"})
	<-stream.Context().Done()
	return nil
}

func TestWorker_RestartSubscription(t *testing.T) {
	_, addr, cleanupServer := setupMockServer(t)
	defer cleanupServer()

	mongoUri, cleanupMongo := setupMongoContainer(t)
	defer cleanupMongo()

	ctx := context.Background()
	worker, err := NewWorker(ctx, "test-worker", mongoUri, "test_db", "test_collection", addr)
	require.NoError(t, err)
	defer worker.Stop()

	// Start initial subscription
	go worker.Subscribe()
	time.Sleep(50 * time.Millisecond)

	// Test restart subscription
	worker.restartSubscription()

	// Should not panic and should create new subscription context
	time.Sleep(50 * time.Millisecond)
	assert.NotNil(t, worker.subscriptionCtx)
}

func TestWorker_ContextCancellation(t *testing.T) {
	_, addr, cleanupServer := setupMockServer(t)
	defer cleanupServer()

	mongoUri, cleanupMongo := setupMongoContainer(t)
	defer cleanupMongo()

	ctx := context.Background()
	worker, err := NewWorker(ctx, "test-worker", mongoUri, "test_db", "test_collection", addr)
	require.NoError(t, err)

	// Start heartbeat and subscription
	go worker.SendHeartbeat()
	go worker.Subscribe()

	// Let them run briefly
	time.Sleep(50 * time.Millisecond)

	// Stop worker
	worker.Stop()

	// Verify context is cancelled
	select {
	case <-worker.Ctx.Done():
		// Expected
	case <-time.After(100 * time.Millisecond):
		t.Error("Worker context should be cancelled after Stop()")
	}
}

func TestWorker_ReconnectToNewMaster_Success(t *testing.T) {
	// Create two mock servers
	_, addr1, cleanup1 := setupMockServer(t)
	defer cleanup1()

	_, addr2, cleanup2 := setupMockServer(t)
	defer cleanup2()

	mongoUri, cleanupMongo := setupMongoContainer(t)
	defer cleanupMongo()

	ctx := context.Background()
	worker, err := NewWorker(ctx, "test-worker", mongoUri, "test_db", "test_collection", addr1)
	require.NoError(t, err)
	defer worker.Stop()

	// Verify initial connection to server1
	originalConn := worker.conn

	// Extract just the IP part from addr2 (remove port)
	host, _, _ := net.SplitHostPort(addr2)

	// Reconnect to server2
	worker.ReconnectToNewMaster(host)

	// Wait for reconnection
	time.Sleep(100 * time.Millisecond)

	// Verify connection changed
	assert.NotEqual(t, originalConn, worker.conn)
}
