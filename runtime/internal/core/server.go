package server

import (
	"context"
	"encoding/json"
	"fmt"
	"runtime/internal/util"
	"runtime/pb"
	"sort"
	"sync"
	"time"

	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/mongo"
	mongoOptions "go.mongodb.org/mongo-driver/mongo/options"
)

type Event struct {
	WorkerId string
	Event    string // e.g. "connected","disconnected"
}

type worker struct {
	segment   int32
	workerId  string
	heartbeat chan struct{}
	ctx       context.Context    // Context used for heartbeat checks
	cancel    context.CancelFunc // Cancel function to stop heartbeat checks
}

type RebalanceNotification struct {
	WorkerId string
	Segment  int32
}

type WorkerServiceGrpcServer struct {
	pb.UnimplementedWorkerServer
	workers        map[string]*worker
	segments       map[int32]string // segment_id -> worker_id (simple 1:1 mapping)
	ctx            context.Context
	cancel         context.CancelFunc
	mongoClient    *mongo.Client
	dbName         string
	collectionName string
	events         chan Event // Channel to listen for worker events
	mutex          sync.Mutex // Mutex to protect access to workers and segments

	subscribers      map[string]chan RebalanceNotification
	subscribersMutex sync.RWMutex
}

/**
 * Heartbeat is a method that is called by the client to tell the server that it is alive.
 * The server will respond with a message of "ok" by default. If the client does not respond
 * within 5 seconds, the server will assume that the client is dead and will remove it from
 * the list of clients and rebalance the load among the remaining clients.
 * @param ctx context.Context
 * @param request *pb.Request
 * @return *pb.Response
 * @return error
 */
func (n *WorkerServiceGrpcServer) Heartbeat(
	ctx context.Context,
	request *pb.Request,
) (*pb.Response, error) {
	var data map[string]any
	if err := json.Unmarshal([]byte(request.Data), &data); err != nil {
		return nil, err
	}

	workerId, ok := data["workerId"].(string)
	if !ok {
		return nil, fmt.Errorf("worker id not found in request data")
	}

	// Check if worker exists
	// We are about to modify the masters state, so we need to lock it!
	n.mutex.Lock()
	worker, exists := n.workers[workerId]
	n.mutex.Unlock() // Unlock after checking existence

	if !exists {
		return &pb.Response{Data: "resubscribe_required"}, nil
	}

	// Update heartbeat
	worker.heartbeat <- struct{}{}
	return &pb.Response{Data: "ok"}, nil
}

/*
 * Subscribe is a method that is called by the worker to subscribe to the server.
 * The server will respond with a list of segments that the client can work on.
 * Any time the list of clients changes the server will rebalance the load among the clients.
 * and send the new list of segments to the clients.
 * @param request *pb.Request
 * @param stream pb.Worker_SubscribeServer
 * @return error
 */
func (n *WorkerServiceGrpcServer) RebalanceSubscription(
	request *pb.Request,
	stream pb.Worker_RebalanceSubscriptionServer,
) error {
	// Parse worker ID from request
	var data map[string]interface{}
	workerId := ""
	if request.Data != "" {
		json.Unmarshal([]byte(request.Data), &data)
		if id, ok := data["workerId"].(string); ok {
			workerId = id
		}
	}

	// Create notification channel for this worker
	notificationChan := make(chan RebalanceNotification, 10)

	// Register subscriber
	n.subscribersMutex.Lock()
	n.subscribers[workerId] = notificationChan
	n.subscribersMutex.Unlock()

	defer func() {
		n.subscribersMutex.Lock()
		delete(n.subscribers, workerId)
		close(notificationChan)
		n.subscribersMutex.Unlock()
	}()

	// Create or get existing worker
	n.events <- Event{
		WorkerId: workerId,
		Event:    "connected",
	}

	// Listen for rebalance events
	for {
		select {
		case <-n.ctx.Done():
			return nil
		case <-stream.Context().Done():
			util.LogMaster("Worker %s disconnected", workerId)
			n.events <- Event{
				WorkerId: workerId,
				Event:    "disconnected",
			}
			return nil
		case notification := <-notificationChan:
			// Send updated segments
			response := map[string]any{
				"workerId": notification.WorkerId,
				"segment":  notification.Segment,
			}
			responseJson, _ := json.Marshal(response)
			if err := stream.Send(&pb.Response{Data: string(responseJson)}); err != nil {
				util.LogMaster("Failed to send rebalance response to worker %s -> disconnecting: %v", workerId, err)
				n.events <- Event{
					WorkerId: workerId,
					Event:    "disconnected",
				}
				return err
			}
		}
	}
}

func (n *WorkerServiceGrpcServer) eventListener(events <-chan Event) {
	for event := range events {
		switch event.Event {
		case "connected":
			n.onConnected(event.WorkerId)
		case "disconnected":
			n.onDisconnected(event.WorkerId)
		}
	}
}

// This function handles the connection of a worker.
func (n *WorkerServiceGrpcServer) onConnected(workerId string) {
	// We are about to modify the masters state, so we need to lock it!
	n.mutex.Lock()
	// Check if worker already exists
	if existingWorker, exists := n.workers[workerId]; exists {
		util.LogMaster("Worker %s already connected, updating heartbeat", workerId)
		existingWorker.cancel() // Cancel any existing heartbeat check
	}

	workerCtx, cancel := context.WithCancel(n.ctx)

	// Create new worker instance
	newWorker := &worker{
		segment:   int32(-1), // Default segment, will be assigned later
		workerId:  workerId,
		heartbeat: make(chan struct{}, 1),
		ctx:       workerCtx, // Use the new context for heartbeat checks
		cancel:    cancel,    // Cancel function to stop heartbeat checks
	}
	n.workers[workerId] = newWorker
	n.mutex.Unlock()

	// Start heartbeat check for the new worker
	go n.checkIfWorkerIsAlive(newWorker)

	// Trigger rebalance to assign segments
	n.Rebalance()
}

// This function handles the disconnection of a worker.
func (n *WorkerServiceGrpcServer) onDisconnected(workerId string) {
	// We are about to modify the masters state, so we need to lock it!
	n.mutex.Lock()

	if worker, exists := n.workers[workerId]; exists {
		worker.cancel() // Cancel the worker's context to stop heartbeat checks
	}
	delete(n.workers, workerId)
	n.removeWorkerFromSegments(workerId)
	n.mutex.Unlock() // Remove the worker from segments
	n.Rebalance()
}

func (n *WorkerServiceGrpcServer) checkIfWorkerIsAlive(client *worker) {
	for {
		select {
		case <-client.ctx.Done(): // Check the clients context to see if it has been cancelled
			return
		case <-client.heartbeat:
			// Heartbeat received, continue monitoring
		case <-time.After(5 * time.Second):
			util.LogMaster("Worker %s is non responsive disconnecting", client.workerId)
			n.events <- Event{
				WorkerId: client.workerId,
				Event:    "disconnected",
			}
			return
		}
	}
}

// removeWorkerFromSegments removes a worker from segment assignments
func (n *WorkerServiceGrpcServer) removeWorkerFromSegments(workerId string) {
	for segmentId, assignedWorker := range n.segments {
		if assignedWorker == workerId {
			delete(n.segments, segmentId)
		}
	}
}

func (n *WorkerServiceGrpcServer) getAvailableSegments() []int32 {
	var availableSegments []int32
	if n.mongoClient != nil {
		partitionsCollection := n.mongoClient.Database(n.dbName).Collection("partitions")
		cursor, err := partitionsCollection.Find(n.ctx, bson.M{"bot_count": bson.M{"$gt": 0}})
		if err != nil {
			return availableSegments
		}
		defer cursor.Close(n.ctx)

		for cursor.Next(n.ctx) {
			var partition struct {
				ID int32 `bson:"_id"`
			}
			if cursor.Decode(&partition) == nil {
				availableSegments = append(availableSegments, partition.ID)
			}
		}
	}
	return availableSegments
}

func (n *WorkerServiceGrpcServer) Rebalance() {
	util.LogMaster("Rebalancing segments among workers")
	// Get available segments from MongoDB
	availableSegments := n.getAvailableSegments()

	// We are about to modify the masters state, so we need to lock it
	// to prevent concurrent modifications
	n.mutex.Lock()

	if len(n.workers) == 0 {
		// We didnt need to cause we have no workers
		n.mutex.Unlock()
		return
	}

	// Find workers without segments
	var workersNeedingSegments []string
	for workerId, worker := range n.workers {
		if worker.segment == -1 {
			workersNeedingSegments = append(workersNeedingSegments, workerId)
		}
	}

	// Find unassigned segments
	var unassignedSegments []int32
	for _, segmentId := range availableSegments {
		if _, assigned := n.segments[segmentId]; !assigned {
			unassignedSegments = append(unassignedSegments, segmentId)
		}
	}

	// Assign unassigned segments to workers without segments
	sort.Strings(workersNeedingSegments) // Deterministic assignment
	assignmentsMade := 0
	for i, workerId := range workersNeedingSegments {
		if i < len(unassignedSegments) {
			segmentId := unassignedSegments[i]
			n.workers[workerId].segment = segmentId
			n.segments[segmentId] = workerId
			assignmentsMade++
			util.LogMaster("Assigned segment %d to worker %s", segmentId, workerId)
		}
	}

	if assignmentsMade > 0 {
		util.LogMaster("Rebalanced %d segments among %d workers", assignmentsMade, len(n.workers))
		// Prepare notifications while holding mutex
		var notifications []RebalanceNotification
		for workerId, worker := range n.workers {
			notifications = append(notifications, RebalanceNotification{
				WorkerId: workerId,
				Segment:  worker.segment,
			})
		}
		n.mutex.Unlock() // Unlock before sending notifications to avoid deadlock
		n.sendNotifications(notifications)
	} else {
		n.mutex.Unlock()
	}
}

func (n *WorkerServiceGrpcServer) sendNotifications(notifications []RebalanceNotification) {
	n.subscribersMutex.RLock()
	defer n.subscribersMutex.RUnlock()

	for _, notification := range notifications {
		if subscriberChan, exists := n.subscribers[notification.WorkerId]; exists {
			select {
			case subscriberChan <- notification:
			default:
				util.LogMaster("Subscriber channel for worker %s is full, skipping notification", notification.WorkerId)
			}
		}
	}
}

func (n *WorkerServiceGrpcServer) Stop() {
	util.LogMaster("Stopping worker service")
	if n.cancel != nil {
		n.cancel()
	}
	util.LogMaster("Worker service stopped")
}

var server *WorkerServiceGrpcServer

// StartRebalanceTimer starts a timer that triggers rebalance at specified intervals
// This is useful for periodic rebalancing in case of dynamic changes in worker availability
// Or a case a worker subscribes when there are no segments available
func (n *WorkerServiceGrpcServer) StartRebalanceTimer(interval time.Duration) {
	ticker := time.NewTicker(interval)
	defer ticker.Stop()
	util.LogMaster("Starting rebalance timer with interval %s", interval)
	for {
		select {
		case <-n.ctx.Done():
			ticker.Stop()
			return
		case <-ticker.C:
			util.LogMaster("Rebalance timer triggered at %s", time.Now().Format(time.RFC3339))
			n.Rebalance()
		}
	}
}

func GetWorkerServiceGrpcServer(
	mongoUri string,
	dbName string,
	collectionName string,
) (*WorkerServiceGrpcServer, error) {
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

	if server == nil {
		context, cancel := context.WithCancel(context.Background())
		server = &WorkerServiceGrpcServer{
			workers:        make(map[string]*worker),
			segments:       make(map[int32]string),
			ctx:            context,
			cancel:         cancel,
			mongoClient:    mongoClient,
			dbName:         dbName,
			collectionName: collectionName,
			events:         make(chan Event, 100), // Buffered channel for events

			subscribers:      make(map[string]chan RebalanceNotification),
			subscribersMutex: sync.RWMutex{},
		}
	}
	// Start event listener in a separate goroutine
	go server.eventListener(server.events)
	// Start rebalance timer in a separate goroutine
	go server.StartRebalanceTimer(10 * time.Second)

	return server, nil
}
