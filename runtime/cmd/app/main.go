package main

import (
	"context"
	"fmt"
	"os"
	"os/signal"
	"strconv"
	"syscall"
	"time"

	"github.com/spf13/cobra"
	"go.mongodb.org/mongo-driver/mongo"
	mongoOptions "go.mongodb.org/mongo-driver/mongo/options"

	backtestRunner "runtime/internal/backtest-runner/server"
	botRunner "runtime/internal/bot-runner/server"
	botScheduler "runtime/internal/bot-scheduler/server"
	"runtime/internal/constants"
	"runtime/internal/util"
)

var (
	// Global flags
	maxSegment uint
	mode       string
	mongoUri   string
	workerId   string
)

// rootCmd represents the base command when called without any subcommands
var rootCmd = &cobra.Command{
	Use:   "the0-runtime",
	Short: "Unified runtime for bot-runner, backtest-runner, and bot-scheduler",
	Long: `the0-runtime is a unified executable that provides command-based service selection 
for bot execution, backtest processing, and scheduled bot management.

Examples:
  the0-runtime bot-runner master
  the0-runtime bot-runner worker
  the0-runtime backtest-runner master --max-segment=16
  the0-runtime backtest-runner worker
  the0-runtime backtest-runner standalone --node=master --max-segment=16
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

// Backtest Runner Commands
var backtestRunnerCmd = &cobra.Command{
	Use:   "backtest-runner",
	Short: "Run backtest processing service",
	Long:  `Manages backtest execution with leader election and standalone/cluster modes`,
}

var backtestRunnerMasterCmd = &cobra.Command{
	Use:   "master",
	Short: "Run backtest-runner in master mode",
	Run: func(cmd *cobra.Command, args []string) {
		runBacktestRunnerMaster()
	},
}

var backtestRunnerWorkerCmd = &cobra.Command{
	Use:   "worker",
	Short: "Run backtest-runner in worker mode",
	Run: func(cmd *cobra.Command, args []string) {
		runBacktestRunnerWorker()
	},
}

var backtestRunnerStandaloneCmd = &cobra.Command{
	Use:   "standalone",
	Short: "Run backtest-runner in standalone mode",
	Run: func(cmd *cobra.Command, args []string) {
		nodeType, _ := cmd.Flags().GetString("node")
		runBacktestRunnerStandalone(nodeType)
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

func main() {
	// Add persistent flags
	rootCmd.PersistentFlags().UintVar(&maxSegment, "max-segment", 16, "Max segment for master node")
	rootCmd.PersistentFlags().StringVar(&mode, "mode", "cluster", "Mode to run the application in (standalone or cluster)")
	rootCmd.PersistentFlags().StringVar(&mongoUri, "mongo-uri", getEnv("MONGO_URI", "mongodb://localhost:27017"), "MongoDB connection URI")
	rootCmd.PersistentFlags().StringVar(&workerId, "worker-id", getEnv("WORKER_ID", generateWorkerID()), "Unique worker ID")

	// Add service-specific flags
	backtestRunnerStandaloneCmd.Flags().String("node", "master", "Node type to run (master or worker)")

	// Build command tree
	rootCmd.AddCommand(botRunnerCmd)
	botRunnerCmd.AddCommand(botRunnerMasterCmd, botRunnerWorkerCmd)

	rootCmd.AddCommand(backtestRunnerCmd)
	backtestRunnerCmd.AddCommand(backtestRunnerMasterCmd, backtestRunnerWorkerCmd, backtestRunnerStandaloneCmd)

	rootCmd.AddCommand(botSchedulerCmd)
	botSchedulerCmd.AddCommand(botSchedulerMasterCmd, botSchedulerWorkerCmd)

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

// Backtest Runner Implementation
func runBacktestRunnerMaster() {
	util.LogMaster("Starting backtest-runner master...")
	util.LogMaster("Database: %s, Collection: %s", constants.BACKTEST_RUNNER_DB_NAME, constants.BACKTEST_COLLECTION)

	if mode == "standalone" {
		runBacktestRunnerStandalone("master")
		return
	}
	runBacktestRunnerCluster()
}

func runBacktestRunnerWorker() {
	util.LogWorker("Starting backtest-runner worker...")
	util.LogWorker("Worker ID: %s", workerId)
	util.LogWorker("Database: %s, Collection: %s", constants.BACKTEST_RUNNER_DB_NAME, constants.BACKTEST_COLLECTION)

	if mode == "standalone" {
		runBacktestRunnerStandalone("worker")
		return
	}
	runBacktestRunnerCluster()
}

func runBacktestRunnerStandalone(nodeType string) {
	switch nodeType {
	case "master":
		// Create master with proper configuration
		address := fmt.Sprintf(":%d", constants.BACKTEST_RUNNER_PORT)
		master := backtestRunner.NewMaster(
			mongoUri,
			constants.BACKTEST_RUNNER_DB_NAME,
			constants.BACKTEST_COLLECTION,
			address,
		)
		util.LogMaster("Backtest-runner master listening on %s", address)
		master.Start()
	case "worker":
		// Get master service address from environment
		masterHost := getEnv("MASTER_HOST", "localhost")
		masterAddress := fmt.Sprintf("%s:%d", masterHost, constants.BACKTEST_RUNNER_PORT)

		// Create MongoDB client
		mongoClient, err := createMongoClient(mongoUri)
		if err != nil {
			util.LogWorker("Failed to create MongoDB client: %v", err)
			os.Exit(1)
		}
		defer mongoClient.Disconnect(context.Background())

		// Get batch configuration from environment
		batchSize, _ := strconv.Atoi(getEnv("BATCH_SIZE", "5"))
		batchTimeoutSec, _ := strconv.Atoi(getEnv("BATCH_TIMEOUT", "300"))
		batchIntervalSec, _ := strconv.Atoi(getEnv("BATCH_INTERVAL", "10"))

		options := backtestRunner.BacktestWorkerOptions{
			BatchSize:     batchSize,
			BatchTimeout:  time.Duration(batchTimeoutSec) * time.Second,
			BatchInterval: time.Duration(batchIntervalSec) * time.Second,
		}

		// Create worker with proper configuration
		ctx := context.Background()
		worker, err := backtestRunner.NewWorker(
			ctx,
			workerId,
			mongoUri,
			constants.BACKTEST_RUNNER_DB_NAME,
			constants.BACKTEST_COLLECTION,
			masterAddress,
			&backtestRunner.WorkerDockerRunnerFactory{},
			&backtestRunner.WorkerSubscriberFactory{},
			mongoClient,
			options,
		)
		if err != nil {
			util.LogWorker("Failed to create backtest-runner worker: %v", err)
			os.Exit(1)
		}

		// Setup signal handling
		ch := make(chan os.Signal, 1)
		signal.Notify(ch, os.Interrupt, syscall.SIGTERM)
		go func() {
			<-ch
			fmt.Println("Backtest worker shutting down...")
			worker.Stop()
		}()

		worker.Start()
	default:
		panic(fmt.Sprintf("Unknown node type: %s", nodeType))
	}
}

func runBacktestRunnerCluster() {
	// TODO: Implement cluster mode with proper leader election
	// This needs to be updated to work with the new architecture
	
	// For now, we'll run in standalone mode until cluster mode is implemented
	util.LogMaster("Cluster mode not yet implemented with new architecture")
	util.LogMaster("Falling back to standalone mode...")
	runBacktestRunnerStandalone("master")
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