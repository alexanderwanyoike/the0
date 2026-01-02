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

	dockerrunner "runtime/internal/docker"
	botnatssubscriber "runtime/internal/docker/bot-runner/subscriber"
	schedulenatssubscriber "runtime/internal/docker/bot-scheduler/subscriber"
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
	mongoUri string
	natsUrl  string

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
  the0-runtime bot-runner
  the0-runtime bot-scheduler
  the0-runtime controller`,
}

// Bot Runner Command - simplified single-process service
var botRunnerCmd = &cobra.Command{
	Use:   "bot-runner",
	Short: "Run bot execution service",
	Long: `Manages live bot execution as a single-process service.
Connects to MongoDB for bot state and optionally to NATS for real-time events.`,
	Run: func(cmd *cobra.Command, args []string) {
		runBotService()
	},
}

// Bot Scheduler Command - simplified single-process service
var botSchedulerCmd = &cobra.Command{
	Use:   "bot-scheduler",
	Short: "Run scheduled bot management service",
	Long:  `Manages scheduled bot execution with cron-based scheduling as a single-process service.`,
	Run: func(cmd *cobra.Command, args []string) {
		runScheduleService()
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
	rootCmd.PersistentFlags().StringVar(&mongoUri, "mongo-uri", getEnv("MONGO_URI", "mongodb://localhost:27017"), "MongoDB connection URI")
	rootCmd.PersistentFlags().StringVar(&natsUrl, "nats-url", getEnv("NATS_URL", ""), "NATS connection URL (optional)")

	// Bot Runner command
	rootCmd.AddCommand(botRunnerCmd)

	// Bot Scheduler command
	rootCmd.AddCommand(botSchedulerCmd)

	// Controller command with flags
	controllerCmd.Flags().StringVar(&controllerNamespace, "namespace", getEnv("NAMESPACE", "the0"), "Kubernetes namespace for bot pods")
	controllerCmd.Flags().DurationVar(&controllerReconcileInterval, "reconcile-interval", 30*time.Second, "How often to reconcile state")
	controllerCmd.Flags().StringVar(&minioEndpoint, "minio-endpoint", getEnv("MINIO_ENDPOINT", "minio:9000"), "MinIO endpoint for bot code")
	controllerCmd.Flags().StringVar(&minioBucket, "minio-bucket", getEnv("MINIO_BUCKET", "the0-custom-bots"), "MinIO bucket for bot code")
	// MinIO credentials read from environment only (not CLI flags) for security
	minioAccessKey = getEnv("MINIO_ACCESS_KEY", "")
	minioSecretKey = getEnv("MINIO_SECRET_KEY", "")
	rootCmd.AddCommand(controllerCmd)

	rootCmd.AddCommand(versionCmd)

	// Execute the root command
	if err := rootCmd.Execute(); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}

// runBotService runs the simplified bot service
func runBotService() {
	util.LogMaster("Starting bot-runner service...")
	util.LogMaster("MongoDB: %s", mongoUri)
	if natsUrl != "" {
		util.LogMaster("NATS: %s", natsUrl)
	} else {
		util.LogMaster("NATS: disabled (poll-only mode)")
	}

	// Create bot service
	service, err := dockerrunner.NewBotService(dockerrunner.BotServiceConfig{
		MongoURI:   mongoUri,
		NATSUrl:    natsUrl,
		Logger:     &util.DefaultLogger{},
		DBName:     constants.BOT_RUNNER_DB_NAME,
		Collection: constants.BOT_RUNNER_COLLECTION,
	})
	if err != nil {
		util.LogMaster("Failed to create bot service: %v", err)
		os.Exit(1)
	}

	// Setup signal handling
	ctx, cancel := context.WithCancel(context.Background())
	ch := make(chan os.Signal, 1)
	signal.Notify(ch, os.Interrupt, syscall.SIGTERM)
	go func() {
		<-ch
		util.LogMaster("Received interrupt signal, shutting down...")
		cancel()
	}()

	// Run the service (blocks until shutdown)
	if err := service.Run(ctx); err != nil {
		util.LogMaster("Bot service error: %v", err)
		os.Exit(1)
	}

	util.LogMaster("Bot service stopped")
}

// runScheduleService runs the simplified schedule service
// TODO: Implement ScheduleService similar to BotService
func runScheduleService() {
	util.LogMaster("Starting bot-scheduler service...")
	util.LogMaster("MongoDB: %s", mongoUri)
	util.LogMaster("NATS: %s", natsUrl)

	// For now, log that this is not yet implemented
	util.LogMaster("ERROR: ScheduleService not yet implemented. Use controller mode for K8s.")
	os.Exit(1)
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
