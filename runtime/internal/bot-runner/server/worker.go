package server

import (
	"context"
	"crypto/sha256"
	"encoding/json"
	"fmt"
	"os"
	"sync"
	"time"

	"runtime/internal/bot-runner/model"
	subscriber "runtime/internal/bot-runner/subscriber"
	"runtime/internal/constants"
	base "runtime/internal/core"
	dockerrunner "runtime/internal/docker-runner"
	core "runtime/internal/model"
	"runtime/internal/util"

	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/mongo"
)

// SubscriberFactory creates a Subscriber instance.
type SubscriberFactory interface {
	CreateSubscriber(
		worker *BotWorker,
	) (subscriber.Subscriber, error)
}

// BotWorker implements a simplified reconciliation-based worker
// that eliminates the complex concurrency patterns and deadlock sources
type BotWorker struct {
	// Core dependencies
	baseWorker          *base.Worker
	dockerRunnerFactory DockerRunnerFactory
	subscriberFactory   SubscriberFactory
	dockerRunner        dockerrunner.DockerRunner
	subscriber          subscriber.Subscriber

	// Simplified state management - SINGLE MUTEX
	state      *WorkerState
	stateMutex sync.RWMutex

	// Simple control channels
	shutdown chan struct{}
}

// WorkerState contains all worker state in a single structure
// Protected by a single mutex to eliminate deadlock sources
type WorkerState struct {
	// Current running containers (botID -> container info)
	RunningBots map[string]*SimpleBotInfo

	// Simple bot tracking for change detection
	LastSeenBots map[string]*model.Bot

	// Deletion tracking (botID -> deletion time)
	RecentDeletions map[string]time.Time

	// Processing metrics
	LastReconcile    time.Time
	ReconcileCount   int64
	ActiveContainers int
	FailedRestarts   int
}

// SimpleBotInfo represents a running bot with minimal state
type SimpleBotInfo struct {
	BotID       string
	ContainerID string
	Status      string // "starting", "running", "stopping", "failed"
	StartTime   time.Time
	Bot         model.Bot
	Entrypoint  string
	Restarts    int
}

type WorkerSubscriberFactory struct{}

func (factory *WorkerSubscriberFactory) CreateSubscriber(
	worker *BotWorker,
) (subscriber.Subscriber, error) {
	return subscriber.NewNATSSubscriber(
		worker.baseWorker.Ctx,
		subscriber.DefaultOptions(),
	)
}

type DockerRunnerFactory interface {
	CreateDockerRunner(
		worker *BotWorker,
	) (dockerrunner.DockerRunner, error)
}

type WorkerDockerRunnerFactory struct{}

func (factory *WorkerDockerRunnerFactory) CreateDockerRunner(
	worker *BotWorker,
) (dockerrunner.DockerRunner, error) {
	return dockerrunner.NewDockerRunner(dockerrunner.DockerRunnerOptions{
		Logger:  &util.DefaultLogger{},
		TempDir: os.Getenv("TEMP_DIR"),
	})
}

func NewWorker(
	ctx context.Context,
	workerId string,
	mongoUri string,
	dbName string,
	collectionName string,
	leader string,
	dockerRunnerFactory DockerRunnerFactory,
	subscriberFactory SubscriberFactory,
	mongoClient *mongo.Client,
) (*BotWorker, error) {

	baseWorker, err := base.NewWorker(
		ctx,
		workerId,
		mongoUri,
		dbName,
		collectionName,
		leader,
	)
	if err != nil {
		util.LogWorker("Failed to create base worker: %v", err)
		return nil, fmt.Errorf("failed to create base worker: %w", err)
	}

	return &BotWorker{
		baseWorker:          baseWorker,
		dockerRunnerFactory: dockerRunnerFactory,
		subscriberFactory:   subscriberFactory,
		state: &WorkerState{
			RunningBots:     make(map[string]*SimpleBotInfo),
			LastSeenBots:    make(map[string]*model.Bot),
			RecentDeletions: make(map[string]time.Time),
		},
		shutdown: make(chan struct{}),
	}, nil
}

// Start begins the simplified reconciliation loop
func (worker *BotWorker) Start() {
	// Initialize dependencies - use type assertion to convert to the expected Worker type
	dockerRunner, err := worker.dockerRunnerFactory.CreateDockerRunner(worker)
	if err != nil {
		util.LogWorker("Failed to create docker runner: %v", err)
		// TODO handle this more gracefully
		return // Exit early if docker runner creation fails
	}
	worker.dockerRunner = dockerRunner

	go worker.setupEventSubscriber()
	go worker.runReconciliationLoop()

	worker.baseWorker.Start() // Start the base worker
	util.LogWorker("Worker started with reconciliation loop for segments: %d", worker.baseWorker.Segment)
}

func (worker *BotWorker) setupEventSubscriber() {
	// Create subscriber with retry logic
	err := util.RetryWithBackoffLogger(func() error {
		var createErr error
		worker.subscriber, createErr = worker.subscriberFactory.CreateSubscriber(worker)
		return createErr
	}, 20, "subscriber creation")

	if err != nil {
		util.LogWorker("CRITICAL: Failed to create subscriber after retries, worker will run in degraded mode: %v", err)
		worker.Stop()
		return // Exit early if subscriber creation fails
	}

	// Start subscriber in a goroutine to handle cleanup
	go func() {
		startErr := util.RetryWithBackoffLogger(func() error {
			return worker.subscriber.Start(worker.baseWorker.Ctx)
		}, 20, "subscriber start")

		if startErr != nil {
			util.LogWorker("CRITICAL: Failed to start subscriber after retries: %v", startErr)
			return // Exit goroutine gracefully
		}
	}()

	fmt.Println("Event subscriber set up successfully")
}

// runReconciliationLoop is the core simplified processing loop
// Single goroutine, no complex concurrency, clear state transitions
func (w *BotWorker) runReconciliationLoop() {
	ticker := time.NewTicker(30 * time.Second) // Simple 30-second reconciliation
	defer ticker.Stop()

	for {
		select {
		case <-ticker.C:
			w.reconcile()
		case <-w.baseWorker.SegmentChange:
			w.reconcile() // Immediate reconciliation trigger
		case <-w.shutdown:
			fmt.Println("Reconciliation loop shutting down")
			w.gracefulShutdown()
			return
		case <-w.baseWorker.Ctx.Done():
			fmt.Println("Reconciliation loop cancelled")
			w.gracefulShutdown()
			return
		}
	}
}

// reconcile performs the core reconciliation logic
func (w *BotWorker) reconcile() {
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Minute)
	defer cancel()

	util.LogWorker("Starting reconciliation cycle")
	util.LogWorker("Worker segment: %d", w.baseWorker.Segment)
	start := time.Now()

	// Step 1: Get desired state from database
	desiredBots, err := w.getDesiredBots(ctx)
	if err != nil {
		util.LogWorker("Failed to get desired bots: %v", err)
		return
	}

	util.LogWorker("Found %d desired bots: ", len(desiredBots))
	for _, bot := range desiredBots {
		util.LogWorker("  Bot ID: %s", bot.ID)
	}

	// Step 2: Get actual running containers from Docker for this worker's segment
	actualContainers, err := w.dockerRunner.ListManagedContainers(ctx, w.baseWorker.Segment)
	if err != nil {
		util.LogWorker("Failed to get actual containers: %v", err)
		// Continue with reconciliation using cached state
		return
	}

	// Print out containers for debugging
	util.LogWorker("Found %d actual containers:", len(actualContainers))
	for _, container := range actualContainers {
		util.LogWorker("  Container: %s->%s", container.ID, container.ContainerID)
	}

	// Step 3: Reconcile state with single lock (no deadlock possible)
	w.stateMutex.Lock()
	w.performReconciliation(ctx, desiredBots, actualContainers)
	w.state.LastReconcile = time.Now()
	w.state.ReconcileCount++
	w.stateMutex.Unlock()

	util.LogWorker("Reconciliation completed in %v", time.Since(start))
}

// performReconciliation handles the actual reconciliation logic
// Called while holding the single state mutex
func (w *BotWorker) performReconciliation(ctx context.Context, desiredBots []model.Bot, actualContainers []*dockerrunner.ContainerInfo) {
	// Step 1: Create maps for easy lookup
	desiredBotsMap := make(map[string]model.Bot)
	for _, bot := range desiredBots {
		if bot.IsEnabled() {
			desiredBotsMap[bot.ID] = bot
		}
	}

	// We need to handle possible duplicates using slices
	actualContainersMap := make(map[string][]*dockerrunner.ContainerInfo)
	for _, container := range actualContainers {
		actualContainersMap[container.ID] = append(actualContainersMap[container.ID], container)
	}

	// Step 2: Iterate through desired bots and take actions
	for botId, bot := range desiredBotsMap {
		runningInstances, isRunning := actualContainersMap[botId]

		// Case 1: Bot is desired but not running -> start it
		if !isRunning || len(runningInstances) == 0 {
			util.LogWorker("Bot %s is desired but not running, starting...", botId)
			w.startBot(ctx, botId, bot)
			continue
		}

		// Case 2: Bot is desired and running -> check for config changes
		// First ensure that its track in our internal state for configuration comparison
		runningInstance := runningInstances[0] // Take the first instance for simplicity
		if len(runningInstances) > 1 {
			fmt.Printf("Found multiple running instances for bot %s, using first one: %s\n", botId, runningInstance.ContainerID)
			fmt.Printf("Stopping additional instances for bot %s...\n", botId)
			for _, instance := range runningInstances[1:] {
				w.stopBot(ctx, botId, instance.ContainerID)
			}
		}

		trackedBotInfo, isTracked := w.state.RunningBots[botId]
		if !isTracked {
			// Not tracked internall, but running in docker. Adopt it and check config
			fmt.Printf("Adopting running bot %s into internal state\n", botId)
			w.state.RunningBots[botId] = &SimpleBotInfo{
				BotID:       botId,
				ContainerID: runningInstance.ContainerID,
				Status:      "running",
				Bot:         bot,
			}
			trackedBotInfo = w.state.RunningBots[botId]
		}
		if w.hasConfigChanged(trackedBotInfo.Bot, bot) {
			// Config has changed -> RESTART the bot
			util.LogWorker("Configuration changed for bot %s - restarting...", botId)
			// Pass the container Id directly to the stop function
			if err := w.stopBot(ctx, botId, runningInstance.ContainerID); err != nil {
				util.LogWorker("Failed to stop container %s during restart for bot %s aborting restart: %v", runningInstance.ContainerID, botId, err)
			} else {
				w.startBot(ctx, botId, bot)
			}

		} else {
			// Config is the same. Update internal state and do nothing
			trackedBotInfo.Bot = bot
			trackedBotInfo.ContainerID = runningInstance.ContainerID
		}
	}

	// Step 3: Iterate through actual containers and stop unwanted ones
	for botId, containers := range actualContainersMap {
		if _, isDesired := desiredBotsMap[botId]; !isDesired {
			// Case 3: Bot is running bot not desired -> stop ALL instances
			util.LogWorker("Bot %s is running but not desired, stopping...", botId)
			for _, container := range containers {
				w.stopBot(ctx, botId, container.ContainerID)
			}
		}
	}

	// Step 4 clean up internal state
	// Remove any bots from internal tracking that are no longer running
	newRunningBotState := make(map[string]*SimpleBotInfo)
	for botId, trackedBot := range w.state.RunningBots {
		// Only keep bots that are still desired and have running instances
		if _, isDesired := desiredBotsMap[botId]; isDesired {
			if _, isRunning := actualContainersMap[botId]; isRunning {
				newRunningBotState[botId] = trackedBot // Keep it in state
			}
		}
	}

	w.state.RunningBots = newRunningBotState
	// Update metrics
	w.state.ActiveContainers = len(w.state.RunningBots)
}

// Simple bot management methods
func (w *BotWorker) startBot(ctx context.Context, botID string, bot model.Bot) {
	entrypointFile := w.determineEntrypointFile(bot)

	util.LogWorker("Starting bot %s with entrypoint %s", botID, entrypointFile)

	// Start container (non-blocking since AutoRemove=true handles cleanup)
	go func() {
		startCtx := context.Background()
		result, err := w.dockerRunner.StartContainer(startCtx, w.toExecutable(bot))
		if err != nil {
			util.LogWorker("Failed to start bot %s: %v", botID, err)
			w.stateMutex.Lock()
			w.state.FailedRestarts++
			w.stateMutex.Unlock()
			return
		}

		// Check if the result contains an error status
		if result.Status == "error" {
			util.LogWorker("Bot %s started with message: %s, error: %s", botID, result.Message, result.Error)
			w.stateMutex.Lock()
			w.state.FailedRestarts++
			w.stateMutex.Unlock()
			return
		}

		// Update tracking with new container info
		w.stateMutex.Lock()
		w.state.RunningBots[botID] = &SimpleBotInfo{
			BotID:       botID,
			ContainerID: result.ContainerID,
			Status:      "running",
			StartTime:   time.Now(),
			Bot:         bot,
			Entrypoint:  entrypointFile,
		}
		w.stateMutex.Unlock()

		util.LogWorker("Successfully started bot %s (container: %s)", botID, result.ContainerID)
	}()
}

func (w *BotWorker) stopBot(ctx context.Context, botID string, containerId string) error {
	util.LogWorker("Stopping bot %s (container: %s)", botID, containerId)

	// Get bot reference before deleting from state
	var bot model.Bot
	if trackedBot, exists := w.state.RunningBots[botID]; exists {
		bot = trackedBot.Bot
	}

	// Mark for deletion
	w.state.RecentDeletions[botID] = time.Now()
	delete(w.state.RunningBots, botID)

	// Execute the stop synchrously and wait for it to complete
	util.LogWorker("Stopping container %s for bot %s", containerId, botID)
	if err := w.dockerRunner.StopContainer(
		ctx,
		containerId,
		w.toExecutable(bot),
	); err != nil {
		util.LogWorker("Failed to stop bot %s: %v", botID, err)
		return err
	}
	return nil
}

func (w *BotWorker) toExecutable(bot model.Bot) core.Executable {
	return core.Executable{
		ID:              bot.ID,
		Runtime:         bot.CustomBotVersion.Config.Runtime,
		Entrypoint:      "bot",
		EntrypointFiles: bot.CustomBotVersion.Config.Entrypoints,
		Config:          bot.Config,
		FilePath:        bot.CustomBotVersion.FilePath,
		IsLongRunning:   true,  // Default to long-running for bots
		PersistResults:  false, // Default to false for bot workers
		Segment:         w.baseWorker.Segment,
	}
}

func (w *BotWorker) determineEntrypointFile(bot model.Bot) string {
	// Simple entrypoint determination logic
	if entrypoint, ok := bot.CustomBotVersion.Config.Entrypoints["bot"]; ok && entrypoint != "" {
		return entrypoint
	}
	return "main.py"
}

func (w *BotWorker) hasConfigChanged(oldBot, newBot model.Bot) bool {
	// Compare the Config maps for changes
	return !w.configMapsEqual(oldBot.Config, newBot.Config)
}

func (w *BotWorker) configMapsEqual(map1, map2 map[string]interface{}) bool {
	// Handle nil cases
	if map1 == nil && map2 == nil {
		return true
	}
	if map1 == nil || map2 == nil {
		return false
	}

	// Simple length check first
	if len(map1) != len(map2) {
		return false
	}

	// Hash-based comparison using crypto/sha256
	hash1 := w.hashMap(map1)
	hash2 := w.hashMap(map2)

	return hash1 == hash2
}

// hashMap creates a consistent hash of a map[string]interface{}
func (w *BotWorker) hashMap(m map[string]interface{}) string {
	// Convert to JSON for consistent ordering
	jsonBytes, err := json.Marshal(m)
	if err != nil {
		return ""
	}

	// Create SHA256 hash
	hasher := sha256.New()
	hasher.Write(jsonBytes)
	return fmt.Sprintf("%x", hasher.Sum(nil))
}

// Database operations
func (w *BotWorker) getDesiredBots(ctx context.Context) ([]model.Bot, error) {
	if w.baseWorker.Segment == -1 {
		return []model.Bot{}, nil
	}

	collection := w.baseWorker.MongoClient.Database(constants.BOT_RUNNER_DB_NAME).Collection(constants.BOT_RUNNER_COLLECTION)

	// Single-segment architecture: query for exactly one segment
	segmentId := w.baseWorker.Segment
	filter := bson.M{
		"segment_id": segmentId,
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

	var bots []model.Bot
	if err = cursor.All(ctx, &bots); err != nil {
		return nil, err
	}

	// Debug logging to see what we actually read from MongoDB
	for i, bot := range bots {
		util.LogWorker("DEBUG: Bot %d - ID: '%s', SegmentId: %d", i, bot.ID, bot.SegmentId)
		util.LogWorker("DEBUG: CustomBotVersion.Version: '%s'", bot.CustomBotVersion.Version)
		util.LogWorker("DEBUG: CustomBotVersion.Config.Runtime: '%s'", bot.CustomBotVersion.Config.Runtime)
		util.LogWorker("DEBUG: CustomBotVersion.Config.Name: '%s'", bot.CustomBotVersion.Config.Name)
	}

	return bots, nil
}

// Control methods
func (w *BotWorker) Stop() {
	select {
	case <-w.shutdown:
		// Already stopped
		return
	default:
		close(w.shutdown)
	}
	util.LogWorker("Stopping Bot Worker...")
	w.baseWorker.Stop()
}

func (w *BotWorker) gracefulShutdown() {
	fmt.Println("Performing graceful shutdown of all containers")

	w.stateMutex.RLock()
	runningBots := make([]*SimpleBotInfo, 0, len(w.state.RunningBots))
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
			w.toExecutable(bot.Bot),
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
}

// GetStatus returns current worker status
func (w *BotWorker) GetStatus() map[string]interface{} {
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
