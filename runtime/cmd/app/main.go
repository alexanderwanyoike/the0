package main

import (
	"context"
	"fmt"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/spf13/cobra"
	"go.mongodb.org/mongo-driver/mongo"
	mongoOptions "go.mongodb.org/mongo-driver/mongo/options"

	botRunner "runtime/internal/bot-runner/server"
	botnatssubscriber "runtime/internal/bot-runner/subscriber"
	botScheduler "runtime/internal/bot-scheduler/server"
	schedulenatssubscriber "runtime/internal/bot-scheduler/subscriber"
	"runtime/internal/constants"
	"runtime/internal/k8s/controller"
	"runtime/internal/k8s/detect"
	"runtime/internal/k8s/health"
	"runtime/internal/util"
)

// Version is the current version of the runtime
const Version = "1.0.0"

var (
	// Global flags
	maxSegment uint
	mode       string
	mongoUri   string
	workerId   string

	// Controller-specific flags
	controllerNamespace         string
	controllerReconcileInterval time.Duration

	// MinIO flags for bot code download
	minioEndpoint  string
	minioAccessKey string
	minioSecretKey string
	minioBucket    string
)

// rootCmd represents the base command when called without any subcommands
var rootCmd = &cobra.Command{
	Use:   "the0-runtime",
	Short: "Unified runtime for bot-runner and bot-scheduler",
	Long: `the0-runtime is a unified executable that provides command-based service selection
for bot execution and scheduled bot management.

Examples:
  the0-runtime bot-runner master
  the0-runtime bot-runner worker
  the0-runtime bot-scheduler master
  the0-runtime bot-scheduler worker`,
}

// Bot Runner Commands
var botRunnerCmd = &cobra.Command{
	Use:   "bot-runner",
	Short: "Run bot execution service",
	Long:  `Manages live bot execution with master-worker architecture`,
}

var botRunnerMasterCmd = &cobra.Command{
	Use:   "master",
	Short: "Run bot-runner in master mode",
	Run: func(cmd *cobra.Command, args []string) {
		runBotRunnerMaster()
	},
}

var botRunnerWorkerCmd = &cobra.Command{
	Use:   "worker",
	Short: "Run bot-runner in worker mode",
	Run: func(cmd *cobra.Command, args []string) {
		runBotRunnerWorker()
	},
}

// Bot Scheduler Commands
var botSchedulerCmd = &cobra.Command{
	Use:   "bot-scheduler",
	Short: "Run scheduled bot management service",
	Long:  `Manages scheduled bot execution with cron-based scheduling`,
}

var botSchedulerMasterCmd = &cobra.Command{
	Use:   "master",
	Short: "Run bot-scheduler in master mode",
	Run: func(cmd *cobra.Command, args []string) {
		runBotSchedulerMaster()
	},
}

var botSchedulerWorkerCmd = &cobra.Command{
	Use:   "worker",
	Short: "Run bot-scheduler in worker mode",
	Run: func(cmd *cobra.Command, args []string) {
		runBotSchedulerWorker()
	},
}

// Controller Command (Kubernetes-native mode)
var controllerCmd = &cobra.Command{
	Use:   "controller",
	Short: "Run Kubernetes-native bot controller",
	Long: `Runs the bot controller in Kubernetes-native mode.
Each bot becomes its own Pod managed by this controller.
The controller reads desired state from MongoDB and reconciles with K8s.

This mode should only be used when running inside a Kubernetes cluster
with RUNTIME_MODE=controller environment variable set.`,
	Run: func(cmd *cobra.Command, args []string) {
		runController()
	},
}

// Version command
var versionCmd = &cobra.Command{
	Use:   "version",
	Short: "Print the version number",
	Run: func(cmd *cobra.Command, args []string) {
		fmt.Printf("the0-runtime version %s\n", Version)
	},
}

func main() {
	// Add persistent flags
	rootCmd.PersistentFlags().UintVar(&maxSegment, "max-segment", 16, "Max segment for master node")
	rootCmd.PersistentFlags().StringVar(&mode, "mode", "cluster", "Mode to run the application in (standalone or cluster)")
	rootCmd.PersistentFlags().StringVar(&mongoUri, "mongo-uri", getEnv("MONGO_URI", "mongodb://localhost:27017"), "MongoDB connection URI")
	rootCmd.PersistentFlags().StringVar(&workerId, "worker-id", getEnv("WORKER_ID", generateWorkerID()), "Unique worker ID")

	// Build command tree
	rootCmd.AddCommand(botRunnerCmd)
	botRunnerCmd.AddCommand(botRunnerMasterCmd, botRunnerWorkerCmd)

	rootCmd.AddCommand(botSchedulerCmd)
	botSchedulerCmd.AddCommand(botSchedulerMasterCmd, botSchedulerWorkerCmd)

	// Controller command with flags
	controllerCmd.Flags().StringVar(&controllerNamespace, "namespace", getEnv("NAMESPACE", "the0"), "Kubernetes namespace for bot pods")
	controllerCmd.Flags().DurationVar(&controllerReconcileInterval, "reconcile-interval", 30*time.Second, "How often to reconcile state")
	controllerCmd.Flags().StringVar(&minioEndpoint, "minio-endpoint", getEnv("MINIO_ENDPOINT", "minio:9000"), "MinIO endpoint for bot code")
	controllerCmd.Flags().StringVar(&minioAccessKey, "minio-access-key", getEnv("MINIO_ACCESS_KEY", ""), "MinIO access key")
	controllerCmd.Flags().StringVar(&minioSecretKey, "minio-secret-key", getEnv("MINIO_SECRET_KEY", ""), "MinIO secret key")
	controllerCmd.Flags().StringVar(&minioBucket, "minio-bucket", getEnv("MINIO_BUCKET", "the0-custom-bots"), "MinIO bucket for bot code")
	rootCmd.AddCommand(controllerCmd)

	rootCmd.AddCommand(versionCmd)

	// Execute the root command
	if err := rootCmd.Execute(); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}

// Bot Runner Implementation
func runBotRunnerMaster() {
	util.LogMaster("Starting bot-runner master...")
	util.LogMaster("Database: %s, Collection: %s", constants.BOT_RUNNER_DB_NAME, constants.BOT_RUNNER_COLLECTION)

	// Setup signal handling
	ch := make(chan os.Signal, 1)
	signal.Notify(ch, os.Interrupt, syscall.SIGTERM)
	go func() {
		<-ch
		util.LogMaster("Received interrupt signal, shutting down bot-runner master...")
		os.Exit(0)
	}()

	// Create master with proper configuration
	address := fmt.Sprintf(":%d", constants.BOT_RUNNER_PORT)
	master := botRunner.NewMaster(
		mongoUri,
		constants.BOT_RUNNER_DB_NAME,
		constants.BOT_RUNNER_COLLECTION,
		address,
	)

	util.LogMaster("Bot-runner master listening on %s", address)
	master.Start()
}

func runBotRunnerWorker() {
	util.LogWorker("Starting bot-runner worker...")
	util.LogWorker("Worker ID: %s", workerId)
	util.LogWorker("Database: %s, Collection: %s", constants.BOT_RUNNER_DB_NAME, constants.BOT_RUNNER_COLLECTION)

	// Get master service address from environment
	masterService := getEnv("MASTER_SERVICE", "bot-runner-master")
	masterAddress := fmt.Sprintf("%s:%d", masterService, constants.BOT_RUNNER_PORT)

	// Create MongoDB client
	mongoClient, err := createMongoClient(mongoUri)
	if err != nil {
		util.LogWorker("Failed to create MongoDB client: %v", err)
		os.Exit(1)
	}
	defer mongoClient.Disconnect(context.Background())

	// Create worker with proper configuration
	ctx := context.Background()
	worker, err := botRunner.NewWorker(
		ctx,
		workerId,
		mongoUri,
		constants.BOT_RUNNER_DB_NAME,
		constants.BOT_RUNNER_COLLECTION,
		masterAddress,
		&botRunner.WorkerDockerRunnerFactory{},
		&botRunner.WorkerSubscriberFactory{},
		mongoClient,
	)
	if err != nil {
		util.LogWorker("Failed to create bot-runner worker: %v", err)
		os.Exit(1)
	}

	// Setup signal handling to gracefully shut down worker
	ch := make(chan os.Signal, 1)
	signal.Notify(ch, os.Interrupt, syscall.SIGTERM)
	go func() {
		<-ch
		util.LogWorker("Received interrupt signal, shutting down bot-runner worker...")
		worker.Stop() // Graceful shutdown
	}()

	worker.Start() // This blocks until shutdown
	util.LogWorker("Bot-runner worker stopped")
}

// Bot Scheduler Implementation
func runBotSchedulerMaster() {
	util.LogMaster("Starting bot-scheduler master...")
	util.LogMaster("Database: %s, Collection: %s", constants.BOT_SCHEDULER_DB_NAME, constants.BOT_SCHEDULE_COLLECTION)

	// Setup signal handling
	ch := make(chan os.Signal, 1)
	signal.Notify(ch, os.Interrupt, syscall.SIGTERM)
	go func() {
		<-ch
		util.LogMaster("Received interrupt signal, shutting down bot-scheduler master...")
		os.Exit(0)
	}()

	// Create master with proper configuration
	address := fmt.Sprintf(":%d", constants.BOT_SCHEDULER_PORT)
	master := botScheduler.NewMaster(
		mongoUri,
		constants.BOT_SCHEDULER_DB_NAME,
		constants.BOT_SCHEDULE_COLLECTION,
		address,
	)

	util.LogMaster("Bot-scheduler master listening on %s", address)
	master.Start()
}

func runBotSchedulerWorker() {
	util.LogWorker("Starting bot-scheduler worker...")
	util.LogWorker("Worker ID: %s", workerId)
	util.LogWorker("Database: %s, Collection: %s", constants.BOT_SCHEDULER_DB_NAME, constants.BOT_SCHEDULE_COLLECTION)

	// Get master service address from environment
	masterService := getEnv("MASTER_SERVICE", "bot-scheduler-master")
	masterAddress := fmt.Sprintf("%s:%d", masterService, constants.BOT_SCHEDULER_PORT)

	// Create MongoDB client
	mongoClient, err := createMongoClient(mongoUri)
	if err != nil {
		util.LogWorker("Failed to create MongoDB client: %v", err)
		os.Exit(1)
	}
	defer mongoClient.Disconnect(context.Background())

	// Create worker with proper configuration
	ctx := context.Background()
	worker, err := botScheduler.NewWorker(
		ctx,
		workerId,
		mongoUri,
		constants.BOT_SCHEDULER_DB_NAME,
		constants.BOT_SCHEDULE_COLLECTION,
		masterAddress,
		&botScheduler.ScheduledBotDockerRunnerFactory{},
		&botScheduler.ScheduledBotSubscriberFactory{},
		mongoClient,
	)
	if err != nil {
		util.LogWorker("Failed to create bot-scheduler worker: %v", err)
		os.Exit(1)
	}

	// Setup signal handling to gracefully shut down worker
	ch := make(chan os.Signal, 1)
	signal.Notify(ch, os.Interrupt, syscall.SIGTERM)
	go func() {
		<-ch
		util.LogWorker("Received interrupt signal, shutting down bot-scheduler worker...")
		worker.Stop() // Graceful shutdown
	}()

	worker.Start() // This blocks until shutdown
	util.LogWorker("Bot-scheduler worker stopped")
}

// Controller Implementation (Kubernetes-native mode)
func runController() {
	util.LogMaster("Starting Kubernetes-native bot controller...")
	util.LogMaster("Runtime mode: %s", detect.DetectRuntimeMode())
	util.LogMaster("Namespace: %s", controllerNamespace)
	util.LogMaster("Reconcile interval: %v", controllerReconcileInterval)

	// Validate MinIO credentials (required for downloading bot code)
	if minioAccessKey == "" || minioSecretKey == "" {
		util.LogMaster("ERROR: MinIO credentials (MINIO_ACCESS_KEY, MINIO_SECRET_KEY) are required")
		os.Exit(1)
	}
	if minioEndpoint == "" {
		util.LogMaster("ERROR: MINIO_ENDPOINT is required")
		os.Exit(1)
	}

	// Start health server for K8s probes
	healthServer := health.NewServer(8080)
	healthServer.Start()

	// Check if we're in the right environment
	if !detect.IsKubernetesEnvironment() {
		util.LogMaster("WARNING: Not running in Kubernetes environment. Controller may not work correctly.")
	}

	// Create MongoDB client
	mongoClient, err := createMongoClient(mongoUri)
	if err != nil {
		util.LogMaster("Failed to create MongoDB client: %v", err)
		os.Exit(1)
	}
	defer mongoClient.Disconnect(context.Background())

	// Start NATS subscribers to sync bots and schedules from API to MongoDB
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	natsUrl := os.Getenv("NATS_URL")
	if natsUrl == "" {
		util.LogMaster("WARNING: NATS_URL not set, bots/schedules won't be synced from API")
	} else {
		// Bot NATS subscriber
		botSubscriber, err := botnatssubscriber.NewNATSSubscriber(ctx, botnatssubscriber.SubscriberOptions{
			NATSUrl:            natsUrl,
			DBName:             constants.BOT_RUNNER_DB_NAME,
			CollectionName:     constants.BOT_RUNNER_COLLECTION,
			MaxBotPerPartition: 100,
			MaxRetries:         3,
			Logger:             &util.DefaultLogger{},
		})
		if err != nil {
			util.LogMaster("Failed to create bot NATS subscriber: %v", err)
			os.Exit(1)
		}
		if err := botSubscriber.Start(ctx); err != nil {
			util.LogMaster("Failed to start bot NATS subscriber: %v", err)
			os.Exit(1)
		}
		defer botSubscriber.Stop()
		util.LogMaster("Bot NATS subscriber started - listening for bot events")

		// Schedule NATS subscriber
		scheduleSubscriber, err := schedulenatssubscriber.NewNATSSubscriber(ctx, schedulenatssubscriber.SubscriberOptions{
			NATSUrl:                    natsUrl,
			DBName:                     constants.BOT_SCHEDULER_DB_NAME,
			CollectionName:             constants.BOT_SCHEDULE_COLLECTION,
			MaxBotSchedulePerPartition: 100,
			MaxRetries:                 3,
			Logger:                     &util.DefaultLogger{},
		})
		if err != nil {
			util.LogMaster("Failed to create schedule NATS subscriber: %v", err)
			os.Exit(1)
		}
		if err := scheduleSubscriber.Start(ctx); err != nil {
			util.LogMaster("Failed to start schedule NATS subscriber: %v", err)
			os.Exit(1)
		}
		defer scheduleSubscriber.Stop()
		util.LogMaster("Schedule NATS subscriber started - listening for schedule events")
	}

	// Create controller manager
	manager, err := controller.NewManager(mongoClient, controller.ManagerConfig{
		Namespace:         controllerNamespace,
		ReconcileInterval: controllerReconcileInterval,
		MinIOEndpoint:     minioEndpoint,
		MinIOAccessKey:    minioAccessKey,
		MinIOSecretKey:    minioSecretKey,
		MinIOBucket:       minioBucket,
		Logger:            &util.DefaultLogger{},
	})
	if err != nil {
		util.LogMaster("Failed to create controller manager: %v", err)
		util.LogMaster("Make sure the controller is running inside a Kubernetes cluster with proper RBAC.")
		os.Exit(1)
	}

	// Setup signal handling
	ch := make(chan os.Signal, 1)
	signal.Notify(ch, os.Interrupt, syscall.SIGTERM)
	go func() {
		<-ch
		util.LogMaster("Received interrupt signal, shutting down controllers...")
		healthServer.SetReady(false)
		manager.Stop()
		cancel()
	}()

	// Start controllers and wait for readiness
	readyCh, err := manager.Start(ctx)
	if err != nil {
		util.LogMaster("Failed to start controller manager: %v", err)
		os.Exit(1)
	}

	// Wait for controllers to be ready, then mark health server as ready
	go func() {
		select {
		case <-readyCh:
			healthServer.SetReady(true)
		case <-ctx.Done():
		}
	}()

	// Wait for context cancellation (signal handler)
	<-ctx.Done()

	// Shutdown health server
	shutdownCtx, shutdownCancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer shutdownCancel()
	healthServer.Stop(shutdownCtx)

	util.LogMaster("Controllers stopped")
}

// Utility functions
func getEnv(key string, defaultValue string) string {
	value, exists := os.LookupEnv(key)
	if !exists {
		return defaultValue
	}
	return value
}

func generateWorkerID() string {
	hostname, _ := os.Hostname()
	if hostname == "" {
		hostname = "unknown"
	}
	return fmt.Sprintf("%s-%d", hostname, os.Getpid())
}

func createMongoClient(mongoUri string) (*mongo.Client, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()

	mongoClient, err := mongo.Connect(ctx, mongoOptions.Client().ApplyURI(mongoUri))
	if err != nil {
		return nil, fmt.Errorf("failed to connect to MongoDB: %w", err)
	}

	// Test the connection
	err = mongoClient.Ping(ctx, nil)
	if err != nil {
		return nil, fmt.Errorf("failed to ping MongoDB: %w", err)
	}

	return mongoClient, nil
}
