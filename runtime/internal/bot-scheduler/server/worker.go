package server

import (
	"context"
	"fmt"
	"sync"
	"time"

	"runtime/internal/bot-scheduler/model"
	"runtime/internal/bot-scheduler/subscriber"
	"runtime/internal/constants"
	base "runtime/internal/core"
	dockerrunner "runtime/internal/docker-runner"
	core "runtime/internal/model"
	"runtime/internal/util"

	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/mongo"
)

// ScheduledBotWorker implements a reconciliation-based worker for scheduled bots
type ScheduledBotWorker struct {
	// Core dependencies
	baseWorker          *base.Worker
	dockerRunnerFactory DockerRunnerFactory
	subscriberFactory   SubscriberFactory
	dockerRunner        dockerrunner.DockerRunner
	subscriber          subscriber.Subscriber

	// Simplified state management - SINGLE MUTEX
	state      *ScheduledWorkerState
	stateMutex sync.RWMutex

	// Simple control channels
	shutdown chan struct{}
}

// ScheduledWorkerState contains all worker state for scheduled bots
type ScheduledWorkerState struct {
	// Current scheduled bot executions (botID -> execution info)
	RunningBots map[string]*ScheduledBotExecution

	// Simple bot tracking for change detection
	LastSeenBots map[string]*model.BotSchedule

	// Deletion tracking (botID -> deletion time)
	RecentDeletions map[string]time.Time

	// Processing metrics
	LastReconcile    time.Time
	ReconcileCount   int64
	ActiveContainers int
	FailedRestarts   int
}

// ScheduledBotExecution represents a scheduled bot execution
type ScheduledBotExecution struct {
	BotID       string
	ContainerID string
	Status      string // "starting", "running", "stopping", "failed"
	StartTime   time.Time
	BotSchedule model.BotSchedule
	Entrypoint  string
	Restarts    int
}

// ScheduledBotSubscriberFactory creates a Subscriber instance.
type ScheduledBotSubscriberFactory struct{}

func (factory *ScheduledBotSubscriberFactory) CreateSubscriber(
	worker *ScheduledBotWorker,
) (subscriber.Subscriber, error) {
	return subscriber.NewNATSSubscriber(
		worker.baseWorker.Ctx,
		subscriber.DefaultOptions(),
	)
}

// ScheduledBotDockerRunnerFactory creates a DockerRunner instance.
type ScheduledBotDockerRunnerFactory struct{}

func (factory *ScheduledBotDockerRunnerFactory) CreateDockerRunner(
	worker *ScheduledBotWorker,
) (dockerrunner.DockerRunner, error) {
	return dockerrunner.NewDockerRunner(dockerrunner.DockerRunnerOptions{
		Logger: &util.DefaultLogger{},
	})
}

// DockerRunnerFactory creates a DockerRunner instance.
type DockerRunnerFactory interface {
	CreateDockerRunner(
		worker *ScheduledBotWorker,
	) (dockerrunner.DockerRunner, error)
}

// SubscriberFactory creates a Subscriber instance.
type SubscriberFactory interface {
	CreateSubscriber(
		worker *ScheduledBotWorker,
	) (subscriber.Subscriber, error)
}

// NewWorker creates a new scheduled bot worker
func NewWorker(
	ctx context.Context,
	workerId string,
	mongoUri string,
	dbName string,
	collectionName string,
	leader string,
	dockerRunnerFactory DockerRunnerFactory,
	subscriberFactory SubscriberFactory,
	mongoClient *mongo.Client) (*ScheduledBotWorker, error) {

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

	return &ScheduledBotWorker{
		baseWorker:          baseWorker,
		dockerRunnerFactory: dockerRunnerFactory,
		subscriberFactory:   subscriberFactory,
		state: &ScheduledWorkerState{
			RunningBots:     make(map[string]*ScheduledBotExecution),
			LastSeenBots:    make(map[string]*model.BotSchedule),
			RecentDeletions: make(map[string]time.Time),
		},
		shutdown: make(chan struct{}),
	}, nil
}

// Start begins the scheduled bot worker
func (worker *ScheduledBotWorker) Start() {
	// Initialize dependencies
	dockerRunner, err := worker.dockerRunnerFactory.CreateDockerRunner(worker)
	if err != nil {
		util.LogWorker("Failed to create DockerRunner: %v", err)
		return
	}
	worker.dockerRunner = dockerRunner
	go worker.setupEventSubscriber()

	// Start the scheduled bot reconciliation loop
	go worker.runReconciliationLoop()
	worker.baseWorker.Start()
	util.LogWorker("Scheduled bot worker started with reconciliation loop for segments: %d", worker.baseWorker.Segment)
}

func (worker *ScheduledBotWorker) setupEventSubscriber() {
	// Create subscriber with retry logic
	err := util.RetryWithBackoffLogger(func() error {
		var createErr error
		worker.subscriber, createErr = worker.subscriberFactory.CreateSubscriber(worker)
		return createErr
	}, 20, "subscriber creation")

	if err != nil {
		util.LogWorker("CRITICAL: Failed to create subscriber after retries, worker will run in degraded mode: %v", err)
		worker.Stop()
		return
	}

	// Start subscriber in a goroutine to handle cleanup
	go func() {
		startErr := util.RetryWithBackoffLogger(func() error {
			return worker.subscriber.Start(worker.baseWorker.Ctx)
		}, 20, "subscriber start")

		if startErr != nil {
			util.LogWorker("CRITICAL: Failed to start subscriber after retries: %v", startErr)
			return
		}
	}()

	fmt.Println("Event subscriber set up successfully")
}

// runReconciliationLoop is the core reconciliation loop for scheduled bots
func (w *ScheduledBotWorker) runReconciliationLoop() {
	ticker := time.NewTicker(5 * time.Second) // Check every 5 seconds for scheduled executions
	defer ticker.Stop()

	for {
		select {
		case <-ticker.C:
			w.reconcile()
		case <-w.baseWorker.SegmentChange:
			w.reconcile() // Immediate reconciliation trigger
		case <-w.shutdown:
			fmt.Println("Scheduled bot reconciliation loop shutting down")
			w.gracefulShutdown()
			return
		case <-w.baseWorker.Ctx.Done():
			fmt.Println("Scheduled bot reconciliation loop cancelled")
			w.gracefulShutdown()
			return
		}
	}
}

// reconcile performs the core reconciliation logic for scheduled bots
func (w *ScheduledBotWorker) reconcile() {
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Minute)
	defer cancel()

	util.LogWorker("Starting scheduled bot reconciliation cycle")
	util.LogWorker("Worker segment: %d", w.baseWorker.Segment)
	start := time.Now()

	// Step 1: Get scheduled bots that are due for execution
	scheduledBots, err := w.getScheduledBotsForExecution(ctx)
	if err != nil {
		util.LogWorker("Failed to get scheduled bots: %v", err)
		return
	}

	util.LogWorker("Found %d scheduled bots due for execution", len(scheduledBots))
	for _, bot := range scheduledBots {
		util.LogWorker("  Bot Schedule ID: %s, next execution: %d", bot.ID, bot.NextExecutionTime)
	}

	// Step 2: Execute scheduled bots
	w.stateMutex.Lock()
	w.performScheduledBotExecution(ctx, scheduledBots)
	w.state.LastReconcile = time.Now()
	w.state.ReconcileCount++
	w.stateMutex.Unlock()

	util.LogWorker("Scheduled bot reconciliation completed in %v", time.Since(start))
}

// performScheduledBotExecution handles the actual execution of scheduled bots
func (w *ScheduledBotWorker) performScheduledBotExecution(ctx context.Context, scheduledBots []model.BotSchedule) {
	currentTime := time.Now().Unix()

	for _, botSchedule := range scheduledBots {
		// Check if it's time to execute
		if botSchedule.NextExecutionTime <= currentTime && botSchedule.IsEnabled() {
			// Check if already running
			if _, isRunning := w.state.RunningBots[botSchedule.ID]; isRunning {
				util.LogWorker("Bot %s is already running, skipping execution", botSchedule.ID)
				continue
			}

			util.LogWorker("Executing scheduled bot %s", botSchedule.ID)
			// Update next execution time then execute
			w.updateNextExecutionTime(ctx, botSchedule)
			w.executeScheduledBot(botSchedule)
		}
	}
}

// executeScheduledBot executes a single scheduled bot
// NOTE: This function is called while holding stateMutex in performScheduledBotExecution
func (w *ScheduledBotWorker) executeScheduledBot(botSchedule model.BotSchedule) {
	entrypoint := "bot" // Always use "bot" entrypoint for scheduled bots

	util.LogWorker("Starting scheduled bot execution %s with entrypoint %s", botSchedule.ID, entrypoint)

	// Mark as running BEFORE starting goroutine to prevent duplicate concurrent executions
	// This ensures that the reconciliation loop's "already running" check (line 250) works correctly
	// NOTE: Caller already holds stateMutex, so we don't need to lock here
	w.state.RunningBots[botSchedule.ID] = &ScheduledBotExecution{
		BotID:       botSchedule.ID,
		ContainerID: "", // Will be updated after container starts
		Status:      "starting",
		StartTime:   time.Now(),
		BotSchedule: botSchedule,
		Entrypoint:  entrypoint,
	}

	// Start container (non-blocking since AutoRemove=true handles cleanup)
	go func() {
		startCtx := context.Background()

		result, err := w.dockerRunner.StartContainer(startCtx, w.toExecutable(botSchedule))
		if err != nil {
			util.LogWorker("Failed to start scheduled bot %s: %v", botSchedule.ID, err)
			w.stateMutex.Lock()
			w.state.FailedRestarts++
			delete(w.state.RunningBots, botSchedule.ID) // Clean up state on failure
			w.stateMutex.Unlock()
			return
		}

		// Check if the result contains an error status
		if result.Status == "error" {
			util.LogWorker("Scheduled bot %s started with message: %s, error: %s", botSchedule.ID, result.Message, result.Error)
			w.stateMutex.Lock()
			w.state.FailedRestarts++
			delete(w.state.RunningBots, botSchedule.ID) // Clean up state on error
			w.stateMutex.Unlock()
			return
		}

		// Update tracking with container ID
		w.stateMutex.Lock()
		if execution, exists := w.state.RunningBots[botSchedule.ID]; exists {
			execution.ContainerID = result.ContainerID
			execution.Status = "running"
		}
		w.stateMutex.Unlock()

		util.LogWorker("Successfully started scheduled bot %s (container: %s)", botSchedule.ID, result.ContainerID)

		// For short-running scheduled bots, the StartContainer call has already completed
		// meaning the container has finished execution. Clean up the state.
		w.stateMutex.Lock()
		delete(w.state.RunningBots, botSchedule.ID)
		w.stateMutex.Unlock()

		util.LogWorker("Scheduled bot %s execution completed, removed from running state", botSchedule.ID)
	}()
}

func (w *ScheduledBotWorker) toExecutable(botSchedule model.BotSchedule) core.Executable {
	return core.Executable{
		ID:              botSchedule.ID,
		Runtime:         botSchedule.CustomBotVersion.Config.Runtime,
		Entrypoint:      "bot",
		EntrypointFiles: botSchedule.CustomBotVersion.Config.Entrypoints,
		Config:          botSchedule.Config,
		FilePath:        botSchedule.CustomBotVersion.FilePath,
		IsLongRunning:   false,
		PersistResults:  false, // Scheduled bots do not persist results by default
		Segment:         w.baseWorker.Segment,
	}
}

// updateNextExecutionTime calculates and updates the next execution time for a bot schedule
func (w *ScheduledBotWorker) updateNextExecutionTime(ctx context.Context, botSchedule model.BotSchedule) {
	schedule, ok := botSchedule.Config["schedule"].(string)
	if !ok || schedule == "" {
		util.LogWorker("No valid schedule found for bot %s", botSchedule.ID)
		return
	}

	nextTime, err := util.CalculateNextExecutionTime(schedule, time.Now())
	if err != nil {
		util.LogWorker("Failed to calculate next execution time for bot %s: %v", botSchedule.ID, err)
		return
	}

	collection := w.baseWorker.MongoClient.Database(constants.BOT_SCHEDULER_DB_NAME).Collection(constants.BOT_SCHEDULE_COLLECTION)
	filter := bson.M{"id": botSchedule.ID}
	update := bson.M{"$set": bson.M{"next_execution_time": nextTime.Unix()}}

	_, err = collection.UpdateOne(ctx, filter, update)
	if err != nil {
		util.LogWorker("Failed to update next execution time for bot %s: %v", botSchedule.ID, err)
	} else {
		util.LogWorker("Updated next execution time for bot %s to %v", botSchedule.ID, nextTime)
	}
}

// Database operations
func (w *ScheduledBotWorker) getScheduledBotsForExecution(ctx context.Context) ([]model.BotSchedule, error) {
	if w.baseWorker.Segment == -1 {
		return []model.BotSchedule{}, nil
	}

	collection := w.baseWorker.MongoClient.Database(constants.BOT_SCHEDULER_DB_NAME).Collection(constants.BOT_SCHEDULE_COLLECTION)

	// Query for scheduled bots in this segment that are due for execution
	segmentId := w.baseWorker.Segment
	currentTime := time.Now().Unix()
	filter := bson.M{
		"segment_id":          segmentId,
		"next_execution_time": bson.M{"$lte": currentTime},
		"$or": []bson.M{
			{"config.enabled": bson.M{"$ne": false}},     // explicitly not disabled
			{"config.enabled": bson.M{"$exists": false}}, // enabled field missing = enabled by default
		},
	}

	cursor, err := collection.Find(ctx, filter)
	if err != nil {
		return nil, err
	}
	defer cursor.Close(ctx)

	var botSchedules []model.BotSchedule
	if err = cursor.All(ctx, &botSchedules); err != nil {
		return nil, err
	}

	return botSchedules, nil
}

// Control methods
func (w *ScheduledBotWorker) Stop() {
	close(w.shutdown)
	util.LogWorker("Stopping scheduled bot worker...")
}

func (w *ScheduledBotWorker) gracefulShutdown() {
	fmt.Println("Performing graceful shutdown of all scheduled bot containers")

	w.stateMutex.RLock()
	runningBots := make([]*ScheduledBotExecution, 0, len(w.state.RunningBots))
	for _, bot := range w.state.RunningBots {
		runningBots = append(runningBots, bot)
	}
	w.stateMutex.RUnlock()

	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()

	// Stop all containers - AutoRemove will handle cleanup
	for _, bot := range runningBots {
		if err := w.dockerRunner.StopContainer(
			ctx,
			bot.ContainerID,
			w.toExecutable(bot.BotSchedule),
		); err != nil {
			fmt.Printf("Failed to stop container %s during shutdown: %v\n", bot.ContainerID, err)
		}
	}

	if w.dockerRunner != nil {
		w.dockerRunner.Close()
	}
	if w.subscriber != nil {
		w.subscriber.Stop()
	}

	if w.baseWorker != nil {
		w.baseWorker.Stop()
	}
}

// GetStatus returns current worker status
func (w *ScheduledBotWorker) GetStatus() map[string]interface{} {
	w.stateMutex.RLock()
	defer w.stateMutex.RUnlock()

	return map[string]interface{}{
		"worker_id":         w.baseWorker.WorkerId,
		"segment":           w.baseWorker.Segment,
		"active_containers": w.state.ActiveContainers,
		"reconcile_count":   w.state.ReconcileCount,
		"last_reconcile":    w.state.LastReconcile,
		"failed_restarts":   w.state.FailedRestarts,
	}
}
