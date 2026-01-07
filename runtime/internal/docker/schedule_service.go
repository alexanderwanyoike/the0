package docker

import (
	"context"
	"fmt"
	"os"
	"sync"
	"time"

	"runtime/internal/constants"
	"runtime/internal/model"
	"runtime/internal/util"

	schedulenatssubscriber "runtime/internal/docker/bot-scheduler/subscriber"

	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/mongo"
	mongoOptions "go.mongodb.org/mongo-driver/mongo/options"
)

// ScheduleServiceConfig contains configuration for the ScheduleService
type ScheduleServiceConfig struct {
	MongoURI      string
	NATSUrl       string // Required - for API communication
	Logger        util.Logger
	DBName        string
	Collection    string
	CheckInterval time.Duration // How often to check for due schedules (default: 10s)
}

// ScheduleService is a simplified single-process scheduled bot execution service
type ScheduleService struct {
	config      ScheduleServiceConfig
	runner      DockerRunner
	state       *ScheduleState
	logger      util.Logger
	mongoClient *mongo.Client

	// NATS subscriber for API events
	subscriber *schedulenatssubscriber.NATSSubscriber

	// Lifecycle
	ctx    context.Context
	cancel context.CancelFunc
	wg     sync.WaitGroup
}

// ScheduleState tracks running and recently executed schedules
type ScheduleState struct {
	mu sync.RWMutex

	// Currently executing schedules (scheduleID -> start time)
	executing map[string]time.Time

	// WaitGroup for tracking execution goroutines
	executionWg sync.WaitGroup

	// Metrics
	lastCheck        time.Time
	executionCount   int64
	failedCount      int64
	activeExecutions int
}

// NewScheduleState creates a new ScheduleState instance
func NewScheduleState() *ScheduleState {
	return &ScheduleState{
		executing: make(map[string]time.Time),
	}
}

// NewScheduleService creates a new ScheduleService instance
func NewScheduleService(config ScheduleServiceConfig) (*ScheduleService, error) {
	if config.MongoURI == "" {
		return nil, fmt.Errorf("MongoURI is required")
	}

	if config.NATSUrl == "" {
		return nil, fmt.Errorf("NATSUrl is required for schedule service")
	}

	if config.Logger == nil {
		config.Logger = &util.DefaultLogger{}
	}

	if config.DBName == "" {
		config.DBName = constants.BOT_SCHEDULER_DB_NAME
	}

	if config.Collection == "" {
		config.Collection = constants.BOT_SCHEDULE_COLLECTION
	}

	if config.CheckInterval == 0 {
		config.CheckInterval = 10 * time.Second
	}

	ctx, cancel := context.WithCancel(context.Background())

	service := &ScheduleService{
		config: config,
		state:  NewScheduleState(),
		logger: config.Logger,
		ctx:    ctx,
		cancel: cancel,
	}

	return service, nil
}

// Run starts the schedule service and blocks until shutdown
func (s *ScheduleService) Run(ctx context.Context) error {
	s.logger.Info("Starting ScheduleService...")

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

	// Initialize NATS subscriber (required)
	if err := s.initNATSSubscriber(ctx); err != nil {
		return fmt.Errorf("failed to initialize NATS subscriber: %w", err)
	}
	defer s.subscriber.Stop()
	s.logger.Info("NATS subscriber started - listening for schedule events")

	// Start the schedule check loop
	s.wg.Add(1)
	go s.runScheduleLoop()

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
func (s *ScheduleService) initMongoDB(ctx context.Context) error {
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

// initNATSSubscriber initializes the NATS subscriber for schedule events
func (s *ScheduleService) initNATSSubscriber(ctx context.Context) error {
	subscriber, err := schedulenatssubscriber.NewNATSSubscriber(ctx, schedulenatssubscriber.SubscriberOptions{
		NATSUrl:                    s.config.NATSUrl,
		DBName:                     s.config.DBName,
		CollectionName:             s.config.Collection,
		MaxBotSchedulePerPartition: 100, // Not used in simplified mode
		MaxRetries:                 3,
		Logger:                     s.logger,
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

// runScheduleLoop checks for due schedules and executes them
func (s *ScheduleService) runScheduleLoop() {
	defer s.wg.Done()

	ticker := time.NewTicker(s.config.CheckInterval)
	defer ticker.Stop()

	// Run immediately on start
	s.checkAndExecuteSchedules()

	for {
		select {
		case <-ticker.C:
			s.checkAndExecuteSchedules()
		case <-s.ctx.Done():
			s.logger.Info("Schedule loop stopped")
			return
		}
	}
}

// checkAndExecuteSchedules finds due schedules and executes them
func (s *ScheduleService) checkAndExecuteSchedules() {
	ctx, cancel := context.WithTimeout(s.ctx, 5*time.Minute)
	defer cancel()

	now := time.Now()
	s.logger.Info("Checking for due schedules at %v", now.Format(time.RFC3339))

	// Find schedules that are due (next_execution_time <= now)
	dueSchedules, err := s.getDueSchedules(ctx, now)
	if err != nil {
		s.logger.Error("Failed to get due schedules: %v", err)
		return
	}

	if len(dueSchedules) == 0 {
		s.logger.Info("No schedules due for execution")
		return
	}

	s.logger.Info("Found %d schedules due for execution", len(dueSchedules))

	// Execute each due schedule
	for _, schedule := range dueSchedules {
		// Atomically check and mark as executing to prevent TOCTOU race
		if !s.tryStartExecution(schedule.ID) {
			s.logger.Info("Schedule %s is already executing, skipping", schedule.ID)
			continue
		}

		// Track goroutine with WaitGroup for graceful shutdown
		s.state.executionWg.Add(1)
		go func(sched model.BotSchedule) {
			defer s.state.executionWg.Done()
			// Use service context so it can be cancelled on shutdown
			s.executeSchedule(s.ctx, sched)
		}(schedule)
	}

	s.state.mu.Lock()
	s.state.lastCheck = now
	s.state.mu.Unlock()
}

// getDueSchedules retrieves schedules that are due for execution
func (s *ScheduleService) getDueSchedules(ctx context.Context, now time.Time) ([]model.BotSchedule, error) {
	collection := s.mongoClient.Database(s.config.DBName).Collection(s.config.Collection)

	// Query for enabled schedules with next_execution_time <= now
	// Check both root level "enabled" field and nested "config.enabled" field
	filter := bson.M{
		"next_execution_time": bson.M{"$lte": now.Unix()},
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

	var schedules []model.BotSchedule
	if err = cursor.All(ctx, &schedules); err != nil {
		return nil, err
	}

	return schedules, nil
}

// isExecuting checks if a schedule is currently being executed
func (s *ScheduleService) isExecuting(scheduleID string) bool {
	s.state.mu.RLock()
	defer s.state.mu.RUnlock()
	_, exists := s.state.executing[scheduleID]
	return exists
}

// tryStartExecution atomically checks if a schedule is already executing and marks it as executing if not.
// Returns true if the schedule was successfully marked as executing, false if it was already executing.
// This prevents TOCTOU race conditions between checking and marking.
func (s *ScheduleService) tryStartExecution(scheduleID string) bool {
	s.state.mu.Lock()
	defer s.state.mu.Unlock()

	if _, exists := s.state.executing[scheduleID]; exists {
		return false // Already executing
	}

	s.state.executing[scheduleID] = time.Now()
	s.state.activeExecutions++
	return true
}

// executeSchedule runs a scheduled bot and updates the next execution time
// Note: The schedule is already marked as executing by tryStartExecution before this is called
func (s *ScheduleService) executeSchedule(ctx context.Context, schedule model.BotSchedule) {
	s.logger.Info("Executing schedule %s", schedule.ID)

	// Clean up executing state when done
	defer func() {
		s.state.mu.Lock()
		delete(s.state.executing, schedule.ID)
		s.state.activeExecutions--
		s.state.mu.Unlock()
	}()

	// Create executable from schedule
	executable := s.toExecutable(schedule)

	// Execute the container (this blocks until completion since IsLongRunning=false)
	startTime := time.Now()
	result, err := s.runner.StartContainer(ctx, executable)
	duration := time.Since(startTime)

	if err != nil {
		s.logger.Error("Failed to execute schedule %s: %v", schedule.ID, err)
		s.state.mu.Lock()
		s.state.failedCount++
		s.state.mu.Unlock()
		// Still update next execution time to avoid retrying immediately
	} else if result.Status == "error" {
		s.logger.Error("Schedule %s completed with error: %s", schedule.ID, result.Error)
		s.state.mu.Lock()
		s.state.failedCount++
		s.state.mu.Unlock()
	} else {
		s.logger.Info("Schedule %s completed successfully in %v", schedule.ID, duration)
		s.state.mu.Lock()
		s.state.executionCount++
		s.state.mu.Unlock()
	}

	// Update next execution time
	if err := s.updateNextExecutionTime(ctx, schedule); err != nil {
		s.logger.Error("Failed to update next execution time for schedule %s: %v", schedule.ID, err)
	}
}

// toExecutable converts a BotSchedule to an Executable
func (s *ScheduleService) toExecutable(schedule model.BotSchedule) model.Executable {
	return model.Executable{
		ID:              schedule.ID,
		Runtime:         schedule.CustomBotVersion.Config.Runtime,
		Entrypoint:      "bot",
		EntrypointFiles: schedule.CustomBotVersion.Config.Entrypoints,
		Config:          schedule.Config,
		FilePath:        schedule.CustomBotVersion.FilePath,
		IsLongRunning:   false, // Scheduled bots run once and exit
		PersistResults:  true,  // Persist results to MinIO
		Segment:         -1,    // No segment in simplified mode
	}
}

// updateNextExecutionTime calculates and updates the next execution time
func (s *ScheduleService) updateNextExecutionTime(ctx context.Context, schedule model.BotSchedule) error {
	// Get cron expression from config
	cronExpr, ok := schedule.Config["schedule"].(string)
	if !ok || cronExpr == "" {
		return fmt.Errorf("missing schedule cron expression for schedule %s", schedule.ID)
	}

	// Calculate next execution time
	nextTime, err := util.CalculateNextExecutionTime(cronExpr, time.Now())
	if err != nil {
		return fmt.Errorf("failed to calculate next execution time: %w", err)
	}

	// Update in MongoDB
	collection := s.mongoClient.Database(s.config.DBName).Collection(s.config.Collection)
	filter := bson.M{"id": schedule.ID}
	update := bson.M{"$set": bson.M{"next_execution_time": nextTime.Unix()}}

	_, err = collection.UpdateOne(ctx, filter, update)
	if err != nil {
		return fmt.Errorf("failed to update next execution time in MongoDB: %w", err)
	}

	s.logger.Info("Updated next execution time for schedule %s to %v", schedule.ID, nextTime.Format(time.RFC3339))
	return nil
}

// shutdown performs graceful shutdown
func (s *ScheduleService) shutdown() {
	s.logger.Info("Performing graceful shutdown...")

	// Cancel context to stop schedule loop
	s.cancel()

	// Wait for schedule loop to stop
	s.wg.Wait()

	// Wait for any executing schedules to complete (with timeout)
	s.waitForExecutions(30 * time.Second)

	s.logger.Info("Shutdown complete")
}

// waitForExecutions waits for executing schedules to complete
func (s *ScheduleService) waitForExecutions(timeout time.Duration) {
	done := make(chan struct{})
	go func() {
		s.state.executionWg.Wait()
		close(done)
	}()

	select {
	case <-done:
		s.logger.Info("All executions completed")
	case <-time.After(timeout):
		s.state.mu.RLock()
		remaining := s.state.activeExecutions
		s.state.mu.RUnlock()
		if remaining > 0 {
			s.logger.Info("Shutdown timeout: %d executions still running", remaining)
		}
	}
}

// Stop initiates graceful shutdown
func (s *ScheduleService) Stop() {
	s.cancel()
}

// GetStatus returns the current service status
func (s *ScheduleService) GetStatus() map[string]interface{} {
	s.state.mu.RLock()
	defer s.state.mu.RUnlock()

	return map[string]interface{}{
		"last_check":        s.state.lastCheck,
		"execution_count":   s.state.executionCount,
		"failed_count":      s.state.failedCount,
		"active_executions": s.state.activeExecutions,
	}
}

// DefaultScheduleServiceConfig returns a ScheduleServiceConfig with environment defaults
func DefaultScheduleServiceConfig() ScheduleServiceConfig {
	return ScheduleServiceConfig{
		MongoURI:   os.Getenv("MONGO_URI"),
		NATSUrl:    os.Getenv("NATS_URL"),
		Logger:     &util.DefaultLogger{},
		DBName:     constants.BOT_SCHEDULER_DB_NAME,
		Collection: constants.BOT_SCHEDULE_COLLECTION,
	}
}
