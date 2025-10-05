package server

import (
	"context"
	"encoding/json"
	"fmt"
	"strings"
	"sync"
	"time"

	"runtime/internal/backtest-runner/model"
	"runtime/internal/backtest-runner/publisher"
	"runtime/internal/backtest-runner/subscriber"
	"runtime/internal/constants"
	base "runtime/internal/core"
	dockerrunner "runtime/internal/docker-runner"
	core "runtime/internal/model"
	"runtime/internal/util"

	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/mongo"
	mongoOptions "go.mongodb.org/mongo-driver/mongo/options"
)

// SubscriberFactory creates a Subscriber instance.
type SubscriberFactory interface {
	CreateSubscriber(
		worker *BacktestWorker,
	) (subscriber.Subscriber, error)
}

// BacktestWorker implements a simplified batch processing worker
// that runs backtests in configurable batches with timeouts
type BacktestWorker struct {
	// Core dependencies - unchanged
	baseWorker          *base.Worker
	dockerRunnerFactory DockerRunnerFactory
	subscriberFactory   SubscriberFactory
	dockerRunner        dockerrunner.DockerRunner
	subscriber          subscriber.Subscriber
	publisher           publisher.Publisher

	// Batch processing configuration
	batchSize     int
	batchTimeout  time.Duration
	batchInterval time.Duration

	// Simple control channels
	shutdown chan struct{}

	// Worker context for operations that should be tied to worker lifecycle
	workerCtx    context.Context
	workerCancel context.CancelFunc
}

// BatchExecutionResult represents the result of a batch execution
type BatchExecutionResult struct {
	BacktestID      string
	Success         bool
	Error           string
	Duration        time.Duration
	StartTime       time.Time
	EndTime         time.Time
	ExecutionResult *dockerrunner.ExecutionResult
}

type WorkerSubscriberFactory struct{}

func (factory *WorkerSubscriberFactory) CreateSubscriber(
	worker *BacktestWorker,
) (subscriber.Subscriber, error) {
	return subscriber.NewNATSSubscriber(
		worker.baseWorker.Ctx,
		subscriber.DefaultOptions(),
	)
}

type DockerRunnerFactory interface {
	CreateDockerRunner(
		worker *BacktestWorker,
	) (dockerrunner.DockerRunner, error)
}

type WorkerDockerRunnerFactory struct{}

func (factory *WorkerDockerRunnerFactory) CreateDockerRunner(
	worker *BacktestWorker,
) (dockerrunner.DockerRunner, error) {
	return dockerrunner.NewDockerRunner(dockerrunner.DockerRunnerOptions{
		Logger: &util.DefaultLogger{},
	})
}

type BacktestWorkerOptions struct {
	BatchSize     int
	BatchTimeout  time.Duration
	BatchInterval time.Duration
}

// NewWorker creates a new simplified worker
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
	options BacktestWorkerOptions,
) (*BacktestWorker, error) {
	// MinIO logger is handled by the DockerRunner
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

	// Initialize publisher
	pub, err := publisher.NewNATSPublisher(publisher.DefaultPublisherConfig())
	if err != nil {
		util.LogWorker("Warning: Failed to create publisher: %v\n", err)
		// Continue without publisher rather than failing completely
		pub = nil
	}

	// Get batch configuration from environment
	batchSize := options.BatchSize
	batchTimeout := options.BatchTimeout
	batchInterval := options.BatchInterval

	util.LogWorker("Worker configured with batch_size=%d, batch_timeout=%v, batch_interval=%v\n",
		batchSize, batchTimeout, batchInterval)

	// Create worker context that will be canceled when the worker stops
	workerCtx, workerCancel := context.WithCancel(context.Background())

	return &BacktestWorker{
		baseWorker:          baseWorker,
		dockerRunnerFactory: dockerRunnerFactory,
		subscriberFactory:   subscriberFactory,
		publisher:           pub,
		batchSize:           batchSize,
		batchTimeout:        batchTimeout,
		batchInterval:       batchInterval,
		shutdown:            make(chan struct{}),
		workerCtx:           workerCtx,
		workerCancel:        workerCancel,
	}, nil
}

// Start begins the simplified reconciliation loop
func (worker *BacktestWorker) Start() {
	// Initialize dependencies - use type assertion to convert to the expected Worker type
	dockerRunner, err := worker.dockerRunnerFactory.CreateDockerRunner(worker)
	if err != nil {
		util.LogWorker("Failed to create docker runner: %v", err)
		return
	}
	worker.dockerRunner = dockerRunner

	go worker.SetupEventSubscriber()
	go worker.runBatchProcessingLoop()

	worker.baseWorker.Start() // Start the base worker
	util.LogWorker("Worker started with reconciliation loop for segment: %d\n", worker.baseWorker.Segment)
}

func (worker *BacktestWorker) SetupEventSubscriber() {
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

// runBatchProcessingLoop is the core batch processing loop
// Processes backtests in configurable batches with timeouts
func (w *BacktestWorker) runBatchProcessingLoop() {
	ticker := time.NewTicker(w.batchInterval)
	defer ticker.Stop()

	for {
		select {
		case <-ticker.C:
			w.processBatch()
		case <-w.baseWorker.SegmentChange:
			w.processBatch() // Immediate batch processing trigger
		case <-w.shutdown:
			fmt.Println("Batch processing loop shutting down")
			w.gracefulShutdown()
			return
		case <-w.baseWorker.Ctx.Done():
			fmt.Println("Batch processing loop cancelled")
			w.gracefulShutdown()
			return
		}
	}
}

// processBatch processes a batch of backtests concurrently
// Runs N backtests in parallel with timeout, waits for all to complete
func (w *BacktestWorker) processBatch() {
	ctx, cancel := context.WithTimeout(context.Background(), w.batchTimeout)
	defer cancel()

	fmt.Println("Starting batch processing cycle")
	start := time.Now()

	// Step 1: Get batch of backtests from database
	backtests, err := w.getBatchFromDB(ctx)
	if err != nil {
		util.LogWorker("Failed to get batch from database: %v\n", err)
		return
	}

	if len(backtests) == 0 {
		fmt.Println("No backtests to process")
		return
	}

	util.LogWorker("Processing batch of %d backtests\n", len(backtests))

	// Step 2: Process batch concurrently
	results := w.runBatchConcurrently(ctx, backtests)

	// Step 3: Handle results and cleanup
	w.handleBatchResults(ctx, results)

	util.LogWorker("Batch processing completed in %v\n", time.Since(start))
}

// getBatchFromDB retrieves a batch of backtests from the database
func (w *BacktestWorker) getBatchFromDB(ctx context.Context) ([]model.Backtest, error) {
	if w.baseWorker.Segment < 0 {
		util.LogWorker("Skipping batch retrieval, segment is not set")
		return []model.Backtest{}, nil
	}

	collection := w.baseWorker.MongoClient.Database(constants.BACKTEST_RUNNER_DB_NAME).Collection(constants.BACKTEST_COLLECTION)

	filter := bson.M{
		"segment_id": w.baseWorker.Segment,
	}

	// Limit to batch size
	opts := mongoOptions.Find().SetLimit(int64(w.batchSize))

	cursor, err := collection.Find(ctx, filter, opts)
	if err != nil {
		return nil, err
	}
	defer cursor.Close(ctx)

	var backtests []model.Backtest
	if err = cursor.All(ctx, &backtests); err != nil {
		return nil, err
	}

	return backtests, nil
}

// runBatchConcurrently executes a batch of backtests concurrently
func (w *BacktestWorker) runBatchConcurrently(ctx context.Context, backtests []model.Backtest) []BatchExecutionResult {
	var wg sync.WaitGroup
	results := make([]BatchExecutionResult, len(backtests))

	for i, backtest := range backtests {
		wg.Add(1)
		go func(index int, bt model.Backtest) {
			defer wg.Done()
			results[index] = w.runSingleBacktest(ctx, bt)
		}(i, backtest)
	}

	wg.Wait()
	return results
}

// runSingleBacktest executes a single backtest synchronously
func (w *BacktestWorker) runSingleBacktest(ctx context.Context, backtest model.Backtest) BatchExecutionResult {
	start := time.Now()
	result := BatchExecutionResult{
		BacktestID: backtest.ID,
		StartTime:  start,
		Success:    false,
	}

	util.LogWorker("Starting backtest %s\n", backtest.ID)

	// Run backtest synchronously with timeout (pass entrypoint key, not resolved command)
	execResult, err := w.dockerRunner.StartContainer(
		ctx,
		w.toExecutable(backtest),
	)
	result.EndTime = time.Now()
	result.Duration = result.EndTime.Sub(start)

	if err != nil {
		result.Error = err.Error()
		util.LogWorker("Failed to run backtest %s: %v\n", backtest.ID, err)
		return result
	}

	if execResult.Status == "error" {
		result.Error = execResult.Error
		util.LogWorker("Backtest %s failed: %s\n", backtest.ID, execResult.Error)
		return result
	}

	// Store the execution result for later processing
	result.ExecutionResult = execResult
	result.Success = true
	util.LogWorker("Successfully completed backtest %s in %v\n", backtest.ID, result.Duration)
	return result
}

func (w *BacktestWorker) toExecutable(backtest model.Backtest) core.Executable {
	return core.Executable{
		ID:              backtest.ID,
		Runtime:         backtest.CustomBotVersion.Config.Runtime,
		Entrypoint:      "backtest",
		EntrypointFiles: backtest.CustomBotVersion.Config.Entrypoints,
		Config:          backtest.Config,
		FilePath:        backtest.CustomBotVersion.FilePath,
		IsLongRunning:   false,
		PersistResults:  true, // Default to true for backtest workers
		Segment:         w.baseWorker.Segment,
	}
}

// handleBatchResults processes the results from a batch execution
func (w *BacktestWorker) handleBatchResults(ctx context.Context, results []BatchExecutionResult) {
	successCount := 0
	failCount := 0

	for _, result := range results {
		if result.Success {
			// Process the completed backtest (store results to GCS, then delete from DB)
			w.handleCompletedBacktest(ctx, result.BacktestID, result.ExecutionResult)
			successCount++
		} else {
			failCount++
			util.LogWorker("Backtest %s failed: %s\n", result.BacktestID, result.Error)
			// Store failure result to GCS and delete from database
			// Use worker context with timeout for cleanup operations, fallback to background if worker is stopping
			var cleanupCtx context.Context
			var cancel context.CancelFunc
			if w.workerCtx != nil && w.workerCtx.Err() == nil {
				cleanupCtx, cancel = context.WithTimeout(w.workerCtx, 30*time.Second)
			} else {
				cleanupCtx, cancel = context.WithTimeout(context.Background(), 30*time.Second)
			}
			w.storeFailedBacktestResult(cleanupCtx, result.BacktestID, result.Error)
			w.deleteCompletedBacktest(cleanupCtx, result.BacktestID)
			cancel()
			// Publish failure event (can use original context since it's fast)
			w.publishBacktestEvent(ctx, result.BacktestID, false, result.Error)
		}
	}

	util.LogWorker("Batch completed: %d successful, %d failed\n", successCount, failCount)
}

// publishBacktestEvent publishes a backtest completion event
func (w *BacktestWorker) publishBacktestEvent(ctx context.Context, backtestID string, success bool, error string) {
	if w.publisher == nil {
		util.LogWorker("Warning: Publisher not available, cannot publish event for backtest %s\n", backtestID)
		return
	}

	event := publisher.BacktestCompletedEvent{
		BacktestId: backtestID,
		Success:    success,
		Error:      error,
	}

	if err := w.publisher.PublishBacktestCompleted(ctx, event); err != nil {
		util.LogWorker("Failed to publish backtest event for %s: %v\n", backtestID, err)
	} else {
		util.LogWorker("Published backtest completion event for %s (success: %t)\n", backtestID, success)
	}
}

// handleCompletedBacktest processes a completed backtest and stores results to GCS
func (w *BacktestWorker) handleCompletedBacktest(ctx context.Context, backtestID string, execResult *dockerrunner.ExecutionResult) {
	util.LogWorker("Processing completed backtest %s\n", backtestID)

	// Store final logs to GCS
	w.storeFinalLogs(ctx, backtestID, execResult.Output)

	// Check if backtest succeeded or failed
	if execResult.Status == "success" && execResult.ExitCode == 0 {
		// Try to parse result from file first, fallback to logs if needed
		w.parseAndStoreBacktestResult(ctx, backtestID, execResult)
	} else {
		// Store failure result
		errorMsg := fmt.Sprintf("Container exited with code %d", execResult.ExitCode)
		if execResult.Error != "" {
			errorMsg = execResult.Error
		}
		w.storeFailedBacktestResult(ctx, backtestID, errorMsg)
	}

	// Delete the backtest from database so it never gets processed again
	// Use worker context with timeout for cleanup, fallback to background if worker is stopping
	var cleanupCtx context.Context
	var cancel context.CancelFunc
	if w.workerCtx != nil && w.workerCtx.Err() == nil {
		cleanupCtx, cancel = context.WithTimeout(w.workerCtx, 30*time.Second)
	} else {
		cleanupCtx, cancel = context.WithTimeout(context.Background(), 30*time.Second)
	}
	w.deleteCompletedBacktest(cleanupCtx, backtestID)
	cancel()

	// Publish completion event
	success := execResult.Status == "success" && execResult.ExitCode == 0
	errorMsg := ""
	if !success {
		errorMsg = execResult.Error
		if errorMsg == "" {
			errorMsg = fmt.Sprintf("Exit code %d", execResult.ExitCode)
		}
	}
	w.publishBacktestEvent(ctx, backtestID, success, errorMsg)

	util.LogWorker("Completed processing backtest %s\n", backtestID)
}

// storeFinalLogs is now handled by the DockerRunner's MinIO logger
func (w *BacktestWorker) storeFinalLogs(ctx context.Context, backtestID, logs string) {
	// Log storage is now handled by DockerRunner's MinIO logger
	util.LogWorker("Log storage handled by DockerRunner for backtest %s\n", backtestID)
}

// parseAndStoreBacktestResult reads backtest results from file or logs and stores analysis.json
func (w *BacktestWorker) parseAndStoreBacktestResult(ctx context.Context, backtestID string, execResult *dockerrunner.ExecutionResult) {
	var resultJSON string

	// First try to read result from file (new reliable method)
	if len(execResult.ResultFileContents) > 0 {
		resultJSON = string(execResult.ResultFileContents)
		util.LogWorker("Using result file contents for backtest %s\n", backtestID)
	} else {
		// Fallback to parsing logs if file reading failed
		util.LogWorker("No result file found, falling back to log parsing for backtest %s\n", backtestID)
		lines := strings.Split(strings.TrimSpace(execResult.Output), "\n")

		// Look for JSON in the last few lines
		for i := len(lines) - 1; i >= 0 && i >= len(lines)-5; i-- {
			line := strings.TrimSpace(lines[i])
			if strings.HasPrefix(line, "{") && strings.HasSuffix(line, "}") {
				resultJSON = line
				break
			}
		}
	}

	if resultJSON == "" {
		// No JSON found in either method, create basic success result
		util.LogWorker("No result JSON found for backtest %s, using default success result\n", backtestID)
		w.storeSuccessBacktestResult(ctx, backtestID, map[string]interface{}{}, []interface{}{}, []interface{}{})
		return
	}

	// Parse the JSON result
	var result map[string]interface{}
	if err := json.Unmarshal([]byte(resultJSON), &result); err != nil {
		util.LogWorker("Failed to parse backtest result JSON for %s: %v\n", backtestID, err)
		w.storeFailedBacktestResult(ctx, backtestID, fmt.Sprintf("Invalid result JSON: %v", err))
		return
	}

	// Extract components - handle both direct format and nested results format
	var metrics map[string]interface{}
	var plots, tables []interface{}

	// Check if the result has a nested "results" structure
	if results, hasResults := result["results"].(map[string]interface{}); hasResults {
		// Handle nested format: { "status": "success", "results": { "metrics": ..., "plots": ... } }
		metrics, _ = results["metrics"].(map[string]interface{})
		plots, _ = results["plots"].([]interface{})
		tables, _ = results["tables"].([]interface{})
	} else {
		// Handle direct format: { "metrics": ..., "plots": ..., "tables": ... }
		metrics, _ = result["metrics"].(map[string]interface{})
		plots, _ = result["plots"].([]interface{})
		tables, _ = result["tables"].([]interface{})
	}

	// Ensure defaults for nil values
	if metrics == nil {
		metrics = make(map[string]interface{})
	}
	if plots == nil {
		plots = []interface{}{}
	}
	if tables == nil {
		tables = []interface{}{}
	}

	w.storeSuccessBacktestResult(ctx, backtestID, metrics, plots, tables)
}

// storeSuccessBacktestResult stores a successful backtest result to GCS
func (w *BacktestWorker) storeSuccessBacktestResult(ctx context.Context, backtestID string, metrics map[string]interface{}, plots, tables []interface{}) {
	analysisResult := map[string]interface{}{
		"status":  "success",
		"metrics": metrics,
		"plots":   plots,
		"tables":  tables,
	}

	w.storeAnalysisResult(ctx, backtestID, analysisResult)
}

// storeFailedBacktestResult stores a failed backtest result to GCS
func (w *BacktestWorker) storeFailedBacktestResult(ctx context.Context, backtestID, errorMsg string) {
	analysisResult := map[string]interface{}{
		"status": "failed",
		"error":  errorMsg,
	}

	w.storeAnalysisResult(ctx, backtestID, analysisResult)
}

// storeAnalysisResult is now handled by the DockerRunner's MinIO logger
func (w *BacktestWorker) storeAnalysisResult(ctx context.Context, backtestID string, result map[string]interface{}) {
	// Analysis storage is now handled by DockerRunner's MinIO logger
	util.LogWorker("Analysis storage handled by DockerRunner for backtest %s\n", backtestID)
}

// deleteCompletedBacktest removes a completed backtest from the database
func (w *BacktestWorker) deleteCompletedBacktest(ctx context.Context, backtestID string) {
	collection := w.baseWorker.MongoClient.Database(constants.BACKTEST_RUNNER_DB_NAME).Collection(constants.BACKTEST_COLLECTION)

	filter := bson.M{"id": backtestID}

	// First, find the backtest to get its segment ID before deleting
	var backtestToDelete struct {
		SegmentId int32 `bson:"segment_id"`
	}
	err := collection.FindOne(ctx, filter).Decode(&backtestToDelete)
	if err != nil {
		if err == mongo.ErrNoDocuments {
			util.LogWorker("No backtest found with ID %s to delete\n", backtestID)
			return
		}
		util.LogWorker("Failed to find backtest %s before deletion: %v\n", backtestID, err)
		return
	}

	result, err := collection.DeleteOne(ctx, filter)
	if err != nil {
		util.LogWorker("Failed to delete completed backtest %s from database: %v\n", backtestID, err)
		return
	}

	if result.DeletedCount == 0 {
		util.LogWorker("No backtest found with ID %s to delete\n", backtestID)
		return
	}

	util.LogWorker("Successfully deleted completed backtest %s from database\n", backtestID)

	// Decrement the partition count
	partitionsCollection := w.baseWorker.MongoClient.Database(constants.BACKTEST_RUNNER_DB_NAME).Collection("partitions")
	partitionFilter := bson.M{"_id": backtestToDelete.SegmentId}
	update := bson.M{"$inc": bson.M{"bot_count": -1}}
	_, err = partitionsCollection.UpdateOne(ctx, partitionFilter, update)
	if err != nil {
		util.LogWorker("CRITICAL: Failed to update partition count after backtest deletion. partition_id: %d, error: %v\n", backtestToDelete.SegmentId, err)
		return
	}
	util.LogWorker("Updated partition count after backtest deletion, partition_id: %d\n", backtestToDelete.SegmentId)
}

// Control methods
func (w *BacktestWorker) Stop() {
	close(w.shutdown)
	if w.workerCancel != nil {
		w.workerCancel() // Cancel worker context to stop any ongoing cleanup operations
	}
	util.LogWorker("Stopping Backtest Worker...")
	w.baseWorker.Stop()
}

func (w *BacktestWorker) gracefulShutdown() {
	fmt.Println("Performing graceful shutdown")

	if w.dockerRunner != nil {
		w.dockerRunner.Close()
	}
	if w.subscriber != nil {
		w.subscriber.Stop()
	}
	if w.publisher != nil {
		w.publisher.Close()
	}
}

// GetStatus returns current worker status
func (w *BacktestWorker) GetStatus() map[string]interface{} {
	return map[string]interface{}{
		"worker_id":      w.baseWorker.WorkerId,
		"segment":        w.baseWorker.Segment,
		"batch_size":     w.batchSize,
		"batch_timeout":  w.batchTimeout.String(),
		"batch_interval": w.batchInterval.String(),
	}
}
