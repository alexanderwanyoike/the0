package docker

import (
	"context"
	"crypto/sha256"
	"encoding/json"
	"fmt"
	"os"
	"sync"
	"time"

	"runtime/internal/constants"
	"runtime/internal/model"
	"runtime/internal/util"

	botnatssubscriber "runtime/internal/docker/bot-runner/subscriber"

	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/mongo"
	mongoOptions "go.mongodb.org/mongo-driver/mongo/options"
)

// BotServiceConfig contains configuration for the BotService
type BotServiceConfig struct {
	MongoURI          string
	NATSUrl           string // Optional - service degrades gracefully without NATS
	Logger            util.Logger
	DBName            string
	Collection        string
	ReconcileInterval time.Duration // How often to reconcile (default: 30s)
}

// BotService is a simplified single-process bot management service
// It replaces the master-worker architecture with a simple reconciliation loop
type BotService struct {
	config      BotServiceConfig
	runner      DockerRunner
	state       *ServiceState
	logger      util.Logger
	mongoClient *mongo.Client

	// NATS subscriber for persisting bot events to MongoDB
	subscriber *botnatssubscriber.NATSSubscriber

	// Lifecycle
	ctx    context.Context
	cancel context.CancelFunc
	wg     sync.WaitGroup
}

// NewBotService creates a new BotService instance
func NewBotService(config BotServiceConfig) (*BotService, error) {
	if config.MongoURI == "" {
		return nil, fmt.Errorf("MongoURI is required")
	}

	if config.Logger == nil {
		config.Logger = &util.DefaultLogger{}
	}

	if config.DBName == "" {
		config.DBName = constants.BOT_RUNNER_DB_NAME
	}

	if config.Collection == "" {
		config.Collection = constants.BOT_RUNNER_COLLECTION
	}

	if config.ReconcileInterval == 0 {
		config.ReconcileInterval = 30 * time.Second
	}

	ctx, cancel := context.WithCancel(context.Background())

	service := &BotService{
		config: config,
		state:  NewServiceState(),
		logger: config.Logger,
		ctx:    ctx,
		cancel: cancel,
	}

	return service, nil
}

// Run starts the bot service and blocks until shutdown
func (s *BotService) Run(ctx context.Context) error {
	s.logger.Info("Starting BotService...")

	// Initialize MongoDB connection
	if err := s.initMongoDB(ctx); err != nil {
		return fmt.Errorf("failed to initialize MongoDB: %w", err)
	}
	defer func() {
		disconnectCtx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
		defer cancel()
		s.mongoClient.Disconnect(disconnectCtx)
	}()

	// Initialize DockerRunner
	runner, err := NewDockerRunner(DockerRunnerOptions{
		Logger: s.logger,
	})
	if err != nil {
		return fmt.Errorf("failed to create DockerRunner: %w", err)
	}
	s.runner = runner
	defer s.runner.Close()

	// Initialize NATS subscriber (optional - degrades gracefully)
	if s.config.NATSUrl != "" {
		if err := s.initNATSSubscriber(ctx); err != nil {
			s.logger.Info("NATS not available, running without real-time events: %v", err)
		} else {
			s.logger.Info("NATS subscriber started - listening for bot events")
			defer s.subscriber.Stop()
		}
	} else {
		s.logger.Info("NATS URL not configured - running in poll-only mode")
	}

	// Run initial reconciliation
	s.reconcile()

	// Start reconciliation loop
	s.wg.Add(1)
	go s.runReconciliationLoop()

	// Wait for shutdown signal
	select {
	case <-ctx.Done():
		s.logger.Info("Received shutdown signal")
	case <-s.ctx.Done():
		s.logger.Info("Service context cancelled")
	}

	// Graceful shutdown
	s.shutdown()

	return nil
}

// initMongoDB initializes the MongoDB connection
func (s *BotService) initMongoDB(ctx context.Context) error {
	connectCtx, cancel := context.WithTimeout(ctx, 10*time.Second)
	defer cancel()

	client, err := mongo.Connect(connectCtx, mongoOptions.Client().ApplyURI(s.config.MongoURI))
	if err != nil {
		return fmt.Errorf("failed to connect to MongoDB: %w", err)
	}

	if err := client.Ping(connectCtx, nil); err != nil {
		return fmt.Errorf("failed to ping MongoDB: %w", err)
	}

	s.mongoClient = client
	s.logger.Info("Connected to MongoDB")
	return nil
}

// initNATSSubscriber initializes the NATS subscriber for bot events
func (s *BotService) initNATSSubscriber(ctx context.Context) error {
	subscriber, err := botnatssubscriber.NewNATSSubscriber(ctx, botnatssubscriber.SubscriberOptions{
		NATSUrl:            s.config.NATSUrl,
		DBName:             s.config.DBName,
		CollectionName:     s.config.Collection,
		MaxBotPerPartition: 100, // Not used in simplified mode
		MaxRetries:         3,
		Logger:             s.logger,
	})
	if err != nil {
		return fmt.Errorf("failed to create NATS subscriber: %w", err)
	}

	if err := subscriber.Start(ctx); err != nil {
		return fmt.Errorf("failed to start NATS subscriber: %w", err)
	}

	s.subscriber = subscriber
	return nil
}

// runReconciliationLoop runs the periodic reconciliation
func (s *BotService) runReconciliationLoop() {
	defer s.wg.Done()

	ticker := time.NewTicker(s.config.ReconcileInterval)
	defer ticker.Stop()

	for {
		select {
		case <-ticker.C:
			s.reconcile()
		case <-s.ctx.Done():
			s.logger.Info("Reconciliation loop stopped")
			return
		}
	}
}

// reconcile performs the core reconciliation between desired and actual state
func (s *BotService) reconcile() {
	ctx, cancel := context.WithTimeout(s.ctx, 2*time.Minute)
	defer cancel()

	start := time.Now()
	s.logger.Info("Starting reconciliation cycle")

	// Step 1: Get desired state from database (all enabled bots)
	desiredBots, err := s.getDesiredBots(ctx)
	if err != nil {
		s.logger.Error("Failed to get desired bots: %v", err)
		return
	}
	s.logger.Info("Found %d desired bots", len(desiredBots))

	// Step 2: Get ALL containers including crashed/exited ones
	allContainers, err := s.runner.ListAllManagedContainers(ctx)
	if err != nil {
		s.logger.Error("Failed to get all containers: %v", err)
		return
	}

	// Step 3: Filter to only realtime containers and handle crashed ones
	// Bot-runner only manages realtime bots, not scheduled bots
	var runningContainers []*ContainerInfo
	for _, container := range allContainers {
		// Skip containers that aren't realtime bots (e.g., scheduled bot containers)
		if container.Labels["runtime.type"] != "realtime" {
			continue
		}

		if container.Status == "exited" {
			s.logger.Error("Bot %s crashed with exit code %d", container.ID, container.ExitCode)
			// Capture crash logs and cleanup the container
			if _, err := s.runner.HandleCrashedContainer(ctx, container); err != nil {
				s.logger.Error("Failed to handle crashed container %s: %v", container.ID, err)
			}
		} else if container.Status == "running" {
			runningContainers = append(runningContainers, container)
		}
	}
	s.logger.Info("Found %d actual containers", len(runningContainers))

	// Step 4: Reconcile state with running containers only
	s.performReconciliation(ctx, desiredBots, runningContainers)
	s.state.UpdateReconcileMetrics()

	s.logger.Info("Reconciliation completed in %v", time.Since(start))
}

// getDesiredBots retrieves all enabled bots from MongoDB
func (s *BotService) getDesiredBots(ctx context.Context) ([]model.Bot, error) {
	collection := s.mongoClient.Database(s.config.DBName).Collection(s.config.Collection)

	// Query for enabled bots
	// Check both root level "enabled" field and nested "config.enabled" field
	filter := bson.M{
		"$and": []bson.M{
			{
				"$or": []bson.M{
					{"enabled": bson.M{"$ne": false}},     // root level not disabled
					{"enabled": bson.M{"$exists": false}}, // root level field missing = enabled by default
				},
			},
			{
				"$or": []bson.M{
					{"config.enabled": bson.M{"$ne": false}},     // config level not disabled
					{"config.enabled": bson.M{"$exists": false}}, // config level field missing = enabled by default
				},
			},
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

	return bots, nil
}

// performReconciliation handles the actual reconciliation logic
func (s *BotService) performReconciliation(ctx context.Context, desiredBots []model.Bot, actualContainers []*ContainerInfo) {
	// Build lookup maps
	desiredBotsMap := make(map[string]model.Bot)
	for _, bot := range desiredBots {
		if bot.IsEnabled() {
			desiredBotsMap[bot.ID] = bot
		}
	}

	// Handle possible duplicate containers per bot
	actualContainersMap := make(map[string][]*ContainerInfo)
	for _, container := range actualContainers {
		actualContainersMap[container.ID] = append(actualContainersMap[container.ID], container)
	}

	// Get current tracked bots
	trackedBots := s.state.GetAllRunningBots()

	// Process desired bots
	for botID, bot := range desiredBotsMap {
		runningInstances, isRunning := actualContainersMap[botID]

		if !isRunning || len(runningInstances) == 0 {
			// Bot is desired but not running -> start it
			s.logger.Info("Bot %s is desired but not running, starting...", botID)
			s.startBot(ctx, botID, bot)
			continue
		}

		// Bot is running - use first instance, stop extras
		runningInstance := runningInstances[0]
		if len(runningInstances) > 1 {
			s.logger.Info("Found %d instances for bot %s, keeping first, stopping extras", len(runningInstances), botID)
			for _, extra := range runningInstances[1:] {
				s.stopBot(ctx, botID, extra.ContainerID)
			}
		}

		// Check if tracked and if config changed
		trackedBot, isTracked := trackedBots[botID]
		if !isTracked {
			// Adopt running container into state
			s.logger.Info("Adopting running bot %s into state", botID)
			s.state.SetRunningBot(&RunningBot{
				BotID:       botID,
				ContainerID: runningInstance.ContainerID,
				Status:      "running",
				Bot:         bot,
			})
			trackedBot, _ = s.state.GetRunningBot(botID)
		}

		if trackedBot != nil && s.hasConfigChanged(trackedBot.Bot, bot) {
			// Config changed -> restart
			s.logger.Info("Configuration changed for bot %s, restarting...", botID)
			if err := s.stopBot(ctx, botID, runningInstance.ContainerID); err != nil {
				s.logger.Error("Failed to stop bot %s during restart: %v", botID, err)
			} else {
				s.startBot(ctx, botID, bot)
			}
		} else if trackedBot != nil {
			// Update tracked state
			s.state.SetRunningBot(&RunningBot{
				BotID:       botID,
				ContainerID: runningInstance.ContainerID,
				Status:      "running",
				Bot:         bot,
				StartTime:   trackedBot.StartTime,
				Restarts:    trackedBot.Restarts,
			})
		}
	}

	// Stop bots that are running but not desired
	for botID, containers := range actualContainersMap {
		if _, isDesired := desiredBotsMap[botID]; !isDesired {
			s.logger.Info("Bot %s is running but not desired, stopping...", botID)
			for _, container := range containers {
				s.stopBot(ctx, botID, container.ContainerID)
			}
		}
	}

	// Clean up state - remove bots no longer running
	newState := make(map[string]*RunningBot)
	for botID := range trackedBots {
		if _, isDesired := desiredBotsMap[botID]; isDesired {
			if _, isRunning := actualContainersMap[botID]; isRunning {
				if bot, ok := s.state.GetRunningBot(botID); ok {
					newState[botID] = bot
				}
			}
		}
	}
	s.state.UpdateRunningBotsFromMap(newState)
}

// startBot starts a bot container
func (s *BotService) startBot(ctx context.Context, botID string, bot model.Bot) {
	s.logger.Info("Starting bot %s", botID)

	s.wg.Add(1)
	go func() {
		defer s.wg.Done()

		executable := s.toExecutable(bot)
		// Use service context for cancellation support
		result, err := s.runner.StartContainer(s.ctx, executable)
		if err != nil {
			s.logger.Error("Failed to start bot %s: %v", botID, err)
			s.state.IncrementFailedRestarts()
			return
		}

		if result.Status == "error" {
			s.logger.Error("Bot %s started with error: %s", botID, result.Error)
			s.state.IncrementFailedRestarts()
			return
		}

		s.state.SetRunningBot(&RunningBot{
			BotID:       botID,
			ContainerID: result.ContainerID,
			Status:      "running",
			StartTime:   time.Now(),
			Bot:         bot,
		})

		s.logger.Info("Successfully started bot %s (container: %s)", botID, result.ContainerID)
	}()
}

// stopBot stops a bot container
func (s *BotService) stopBot(ctx context.Context, botID string, containerID string) error {
	s.logger.Info("Stopping bot %s (container: %s)", botID, containerID)

	// Get bot reference and mark as "stopping" before attempting stop
	var bot model.Bot
	var originalState *RunningBot
	if trackedBot, ok := s.state.GetRunningBot(botID); ok {
		bot = trackedBot.Bot
		originalState = trackedBot
		// Mark as "stopping" to prevent restart attempts during stop
		s.state.SetRunningBot(&RunningBot{
			BotID:       botID,
			ContainerID: containerID,
			Status:      "stopping",
			StartTime:   trackedBot.StartTime,
			Bot:         trackedBot.Bot,
			Restarts:    trackedBot.Restarts,
		})
	}

	// Attempt to stop the container
	if err := s.runner.StopContainer(ctx, containerID, s.toExecutable(bot)); err != nil {
		s.logger.Error("Failed to stop bot %s: %v", botID, err)
		// Revert status back to running since stop failed
		if originalState != nil {
			s.state.SetRunningBot(&RunningBot{
				BotID:       botID,
				ContainerID: containerID,
				Status:      "running",
				StartTime:   originalState.StartTime,
				Bot:         originalState.Bot,
				Restarts:    originalState.Restarts,
			})
		}
		return err
	}

	// Only remove from state after successful stop
	s.state.RemoveRunningBot(botID)
	return nil
}

// toExecutable converts a Bot to an Executable
func (s *BotService) toExecutable(bot model.Bot) model.Executable {
	return model.Executable{
		ID:              bot.ID,
		Runtime:         bot.CustomBotVersion.Config.Runtime,
		Entrypoint:      "bot",
		EntrypointFiles: bot.CustomBotVersion.Config.Entrypoints,
		Config:          bot.Config,
		FilePath:        bot.CustomBotVersion.FilePath,
		IsLongRunning:   true,
		Segment:         -1,
	}
}

// hasConfigChanged checks if the bot configuration has changed
func (s *BotService) hasConfigChanged(oldBot, newBot model.Bot) bool {
	// Compare config maps using hash
	return s.hashMap(oldBot.Config) != s.hashMap(newBot.Config)
}

// hashMap creates a consistent hash of a map
func (s *BotService) hashMap(m map[string]interface{}) string {
	if m == nil {
		return "nil"
	}
	jsonBytes, err := json.Marshal(m)
	if err != nil {
		// Return unique error sentinel so failed hashes never match each other
		return fmt.Sprintf("error:%p", m)
	}
	hasher := sha256.New()
	hasher.Write(jsonBytes)
	return fmt.Sprintf("%x", hasher.Sum(nil))
}

// shutdown performs graceful shutdown
func (s *BotService) shutdown() {
	s.logger.Info("Performing graceful shutdown...")

	// Cancel context to stop reconciliation loop
	s.cancel()

	// Wait for reconciliation loop and any pending startBot goroutines to stop
	s.wg.Wait()

	// Stop all running containers concurrently
	runningBots := s.state.GetAllRunningBots()
	if len(runningBots) == 0 {
		s.logger.Info("Shutdown complete")
		return
	}

	// Use a WaitGroup to track all stop operations
	var stopWg sync.WaitGroup
	for _, bot := range runningBots {
		stopWg.Add(1)
		go func(b *RunningBot) {
			defer stopWg.Done()
			// Each container gets its own 30s timeout
			ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
			defer cancel()
			if err := s.runner.StopContainer(ctx, b.ContainerID, s.toExecutable(b.Bot)); err != nil {
				s.logger.Error("Failed to stop container %s during shutdown: %v", b.ContainerID, err)
			}
		}(bot)
	}

	// Wait for all containers with overall timeout
	done := make(chan struct{})
	go func() {
		stopWg.Wait()
		close(done)
	}()

	select {
	case <-done:
		s.logger.Info("Shutdown complete")
	case <-time.After(2 * time.Minute):
		s.logger.Info("Shutdown timeout reached, some containers may still be running")
	}
}

// Stop initiates graceful shutdown
func (s *BotService) Stop() {
	s.cancel()
}

// GetStatus returns the current service status
func (s *BotService) GetStatus() map[string]interface{} {
	metrics := s.state.GetMetrics()
	metrics["nats_connected"] = s.subscriber != nil
	return metrics
}

// GetBot retrieves a bot by ID from MongoDB (implements BotResolver interface)
func (s *BotService) GetBot(ctx context.Context, botID string) (*model.Bot, error) {
	collection := s.mongoClient.Database(s.config.DBName).Collection(s.config.Collection)

	var bot model.Bot
	err := collection.FindOne(ctx, bson.M{"_id": botID}).Decode(&bot)
	if err != nil {
		if err == mongo.ErrNoDocuments {
			return nil, fmt.Errorf("bot not found: %s", botID)
		}
		return nil, fmt.Errorf("failed to get bot: %w", err)
	}

	return &bot, nil
}

// GetContainerID returns the container ID for a running bot (implements BotResolver interface)
func (s *BotService) GetContainerID(ctx context.Context, botID string) (string, bool) {
	runningBot, ok := s.state.GetRunningBot(botID)
	if !ok {
		return "", false
	}
	return runningBot.ContainerID, true
}

// DefaultBotServiceConfig returns a BotServiceConfig with environment defaults
func DefaultBotServiceConfig() BotServiceConfig {
	return BotServiceConfig{
		MongoURI:   os.Getenv("MONGO_URI"),
		NATSUrl:    os.Getenv("NATS_URL"),
		Logger:     &util.DefaultLogger{},
		DBName:     constants.BOT_RUNNER_DB_NAME,
		Collection: constants.BOT_RUNNER_COLLECTION,
	}
}
