package server

import (
	"context"
	"encoding/json"
	"fmt"
	"runtime/internal/util"
	"runtime/pb"
	"sync"
	"time"

	"go.mongodb.org/mongo-driver/mongo"
	mongoOptions "go.mongodb.org/mongo-driver/mongo/options"
	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
)

type WorkerInterface interface {
	Start()
	Stop()
	GetStatus() map[string]interface{}
	ReconnectToNewMaster(masterIP string)
}

// Worker implements a simplified reconciliation-based worker
// that eliminates the complex concurrency patterns and deadlock sources
type Worker struct {
	// Core dependencies - unchanged
	conn        *grpc.ClientConn
	client      pb.WorkerClient
	Segment     int32
	WorkerId    string
	Started     bool
	Ctx         context.Context
	Cancel      context.CancelFunc
	MongoClient *mongo.Client

	// State management
	stateMutex sync.RWMutex

	// Notification channels
	SegmentChange chan struct{}

	// Simple subscription management
	subscriptionCtx    context.Context
	subscriptionCancel context.CancelFunc
	subscriptionMutex  sync.Mutex
}

type HeartbeatRequest struct {
	WorkerId string `json:"workerId"`
}

type SubscriptionResponse struct {
	Segment  int32  `json:"segment"`
	WorkerId string `json:"workerId"`
}

// NewWorker creates a new simplified worker
func NewWorker(
	ctx context.Context,
	workerId string,
	mongoUri string,
	dbName string,
	collectionName string,
	leader string,
) (*Worker, error) {
	if mongoUri == "" {
		return nil, fmt.Errorf("MONGO_URI environment variable is not set")
	}

	mongoClient, err := mongo.Connect(context.TODO(), mongoOptions.Client().ApplyURI(mongoUri))
	if err != nil {
		return nil, fmt.Errorf("failed to connect to MongoDB: %v", err)
	}

	// Verify MongoDB connection
	if err := mongoClient.Ping(context.TODO(), nil); err != nil {
		return nil, fmt.Errorf("failed to ping MongoDB: %v", err)
	}

	workerCtx, cancel := context.WithCancel(ctx)

	worker := &Worker{
		Segment:       int32(-1), // Default segment
		WorkerId:      workerId,
		Ctx:           workerCtx,
		Cancel:        cancel,
		MongoClient:   mongoClient,
		SegmentChange: make(chan struct{}, 1), // Buffered to avoid blocking
	}

	// Initialize subscription context
	worker.subscriptionCtx, worker.subscriptionCancel = context.WithCancel(workerCtx)
	util.LogWorker("New worker created with ID: %s, Segment: %d", worker.WorkerId, worker.Segment)
	if err := worker.initConnection(leader); err != nil {
		util.LogWorker("Failed to initialize connection for worker %s: %v", worker.WorkerId, err)
		return nil, fmt.Errorf("failed to initialize connection: %w", err)
	}
	return worker, nil
}

// Start begins the simplified reconciliation loop
func (worker *Worker) Start() {
	go worker.Subscribe()
	util.LogWorker("Starting heartbeat goroutine...")
	go worker.SendHeartbeat()
	util.LogWorker("Worker started with reconciliation loop for segments: %d", worker.Segment)

	// Block forever to keep the worker running
	<-worker.Ctx.Done()
	util.LogWorker("Worker shutting down")
}

func (worker *Worker) SendHeartbeat() {
	util.LogWorker("SendHeartbeat function started")
	select {
	case <-worker.Ctx.Done():
		util.LogWorker("Worker context already cancelled before heartbeat loop: %v", worker.Ctx.Err())
		return
	default:
		util.LogWorker("Worker context is active, starting heartbeat loop")
	}

	for {
		select {
		case <-worker.Ctx.Done():
			util.LogWorker("Heartbeat cancelled during loop: %v", worker.Ctx.Err())
			return
		default:
			ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
			defer cancel()

			util.LogWorker("Sending heartbeat to server...")

			heartbeatRequest := HeartbeatRequest{WorkerId: worker.WorkerId}
			data, err := json.Marshal(heartbeatRequest)
			if err != nil {
				util.LogWorker("Error marshalling heartbeat request, skipping: %v", err)
				time.Sleep(10 * time.Second)
				continue
			}

			response, err := worker.client.Heartbeat(ctx, &pb.Request{Data: string(data)})
			if err != nil {
				util.LogWorker("Error sending heartbeat: %v", err)
				time.Sleep(10 * time.Second) // Wait longer between heartbeats when failing
				continue                     // Retry instead of panicking
			}

			util.LogWorker("Heartbeat response: %s", response.Data)

			// Check if server is requesting resubscription
			if response.Data == "resubscribe_required" {
				util.LogWorker("Server requested resubscription, restarting subscription")
				worker.restartSubscription()
			}

			time.Sleep(2 * time.Second)
		}
	}
}

// restartSubscription cancels current subscription and starts a new one
func (worker *Worker) restartSubscription() {
	worker.subscriptionMutex.Lock()
	defer worker.subscriptionMutex.Unlock()

	util.LogWorker("Restarting subscription")

	// Cancel existing subscription
	if worker.subscriptionCancel != nil {
		worker.subscriptionCancel()
	}

	// Start new subscription
	go worker.Subscribe()
}

func (worker *Worker) Subscribe() {
	// Cancel any existing subscription first
	if worker.subscriptionCancel != nil {
		util.LogWorker("Canceling existing subscription before starting new one")
		worker.subscriptionCancel()
	}

	// Create new subscription context
	worker.subscriptionCtx, worker.subscriptionCancel = context.WithCancel(context.Background())

	util.LogWorker("Starting subscription for worker %s", worker.WorkerId)
	// Create request with current state for master reconstruction
	request := &pb.Request{Data: fmt.Sprintf(`{"workerId": "%s"}`, worker.WorkerId)}

	// Retry subscription with exponential backoff
	var stream pb.Worker_RebalanceSubscriptionClient
	err := util.RetryWithBackoffLogger(func() error {
		var subscribeErr error
		stream, subscribeErr = worker.client.RebalanceSubscription(worker.subscriptionCtx, request)
		return subscribeErr
	}, 20, "gRPC subscription")

	if err != nil {
		util.LogWorker("Failed to establish subscription after retries, worker will continue without rebalancing: %v", err)
		return // Degrade gracefully instead of panic
	}

	util.LogWorker("Subscription established successfully")

	for {
		select {
		case <-worker.Ctx.Done():
			util.LogWorker("Worker context cancelled, stopping subscription")
			return
		case <-worker.subscriptionCtx.Done():
			util.LogWorker("Subscription context cancelled, stopping subscription")
			return
		default:
			response, err := stream.Recv()
			if err != nil {
				util.LogWorker("Error receiving subscription response: %v", err)
				return
			}

			if response.Data != "" {
				var responseData SubscriptionResponse
				if err := json.Unmarshal([]byte(response.Data), &responseData); err != nil {
					util.LogWorker("Error unmarshalling subscription response: %v", err)
					continue
				}

				util.LogWorker("Received segment assignment: %v", responseData.Segment)

				// Update segments and trigger reconciliation
				worker.Segment = responseData.Segment
				worker.WorkerId = responseData.WorkerId

				// Trigger reconciliation for new segment assignment
				select {
				case worker.SegmentChange <- struct{}{}:
					util.LogWorker("Triggered reconciliation due to segment change")
				default:
					// Reconciliation already pending
				}
			}
		}
	}
}

// Control methods
func (w *Worker) Stop() {
	// Stop the worker gracefully
	if w.Cancel != nil {
		util.LogWorker("Stopping worker %s", w.WorkerId)
		w.Cancel()
	}
}

// ReconnectToNewMaster gracefully reconnects to a new master without stopping containers
func (w *Worker) ReconnectToNewMaster(masterIP string) {
	fmt.Printf("Reconnecting to new master at IP: %s\n", masterIP)

	// Close existing connection
	if w.conn != nil {
		w.conn.Close()
	}

	// Establish new connection to new master
	conn, err := grpc.NewClient(masterIP+":50051", grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		fmt.Printf("Failed to connect to new master at %s: %v\n", masterIP, err)
		return
	}

	// Update worker connection and client
	w.conn = conn
	w.client = pb.NewWorkerClient(conn)

	fmt.Printf("Successfully reconnected to new master at %s\n", masterIP)

	// Restart subscription with current state reporting
	go w.Subscribe()
}

// GetStatus returns current worker status
func (w *Worker) GetStatus() map[string]interface{} {
	w.stateMutex.RLock()
	defer w.stateMutex.RUnlock()

	return map[string]interface{}{
		"worker_id": w.WorkerId,
		"segment":   w.Segment,
	}
}

// initConnection initializes the gRPC connection to the leader
func (worker *Worker) initConnection(leader string) error {
	if leader == "" {
		return fmt.Errorf("leader address is empty")
	}
	util.LogWorker("Attempting to connect to master at: %s", leader)
	// leader already includes port from main.go
	conn, err := grpc.NewClient(leader, grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		util.LogWorker("Failed to connect to master at %s: %v", leader, err)
		return err
	}
	util.LogWorker("Successfully connected to master at: %s", leader)
	worker.conn = conn
	worker.client = pb.NewWorkerClient(conn)
	return nil
}
