// Package controller provides Kubernetes controllers for managing bot Pods.
package controller

import (
	"context"
	"fmt"
	"sync"
	"time"

	"go.mongodb.org/mongo-driver/mongo"

	"runtime/internal/constants"
	"runtime/internal/util"
)

// ManagerConfig holds configuration for the controller manager.
type ManagerConfig struct {
	// Namespace is the Kubernetes namespace for bot pods.
	Namespace string

	// ReconcileInterval is how often controllers reconcile state.
	ReconcileInterval time.Duration

	// MinIO configuration for downloading bot code
	MinIOEndpoint  string
	MinIOAccessKey string
	MinIOSecretKey string
	MinIOBucket    string
	MinIOUseSSL    bool

	// RuntimeImage for init containers and sidecars in bot pods
	RuntimeImage string

	// RuntimeImagePullPolicy for init/sidecar containers (default: IfNotPresent)
	RuntimeImagePullPolicy string

	// Logger for the manager (optional)
	Logger util.Logger
}

// Manager coordinates multiple K8s controllers and their dependencies.
// It manages the lifecycle of bot controller, schedule controller, and log collector.
type Manager struct {
	config ManagerConfig
	logger util.Logger

	// Controllers
	botController      *BotController
	scheduleController *BotScheduleController
	logCollector       *K8sLogCollector

	// State
	mu      sync.Mutex
	running bool
	stopCh  chan struct{}
}

// NewManager creates a new controller manager.
func NewManager(mongoClient *mongo.Client, config ManagerConfig) (*Manager, error) {
	if config.Namespace == "" {
		config.Namespace = "the0"
	}
	if config.ReconcileInterval == 0 {
		config.ReconcileInterval = 30 * time.Second
	}
	if config.Logger == nil {
		config.Logger = &util.DefaultLogger{}
	}

	logger := config.Logger
	logger.Info("Creating controller manager", "namespace", config.Namespace)

	// Create bot repository
	botRepo := NewMongoBotRepository(
		mongoClient,
		constants.BOT_RUNNER_DB_NAME,
		constants.BOT_RUNNER_COLLECTION,
	)

	// Create bot pod client
	botControllerName := "the0-bot-controller"
	podClient, err := NewPodClient(botControllerName)
	if err != nil {
		return nil, fmt.Errorf("failed to create pod client: %w", err)
	}

	// Create bot controller
	botCtrl := NewBotController(
		BotControllerConfig{
			Namespace:              config.Namespace,
			ReconcileInterval:      config.ReconcileInterval,
			ControllerName:         botControllerName,
			MinIOEndpoint:          config.MinIOEndpoint,
			MinIOAccessKey:         config.MinIOAccessKey,
			MinIOSecretKey:         config.MinIOSecretKey,
			MinIOBucket:            config.MinIOBucket,
			MinIOUseSSL:            config.MinIOUseSSL,
			RuntimeImage:           config.RuntimeImage,
			RuntimeImagePullPolicy: config.RuntimeImagePullPolicy,
		},
		botRepo,
		podClient,
	)

	// Create schedule repository
	scheduleRepo := NewMongoBotScheduleRepository(
		mongoClient,
		constants.BOT_SCHEDULER_DB_NAME,
		constants.BOT_SCHEDULE_COLLECTION,
	)

	// Create CronJob client
	scheduleControllerName := "the0-schedule-controller"
	cronClient, err := NewRealK8sCronJobClient(scheduleControllerName)
	if err != nil {
		return nil, fmt.Errorf("failed to create CronJob client: %w", err)
	}

	// Create schedule controller
	scheduleCtrl := NewBotScheduleController(
		BotScheduleControllerConfig{
			Namespace:              config.Namespace,
			ReconcileInterval:      config.ReconcileInterval,
			ControllerName:         scheduleControllerName,
			MinIOEndpoint:          config.MinIOEndpoint,
			MinIOAccessKey:         config.MinIOAccessKey,
			MinIOSecretKey:         config.MinIOSecretKey,
			MinIOBucket:            config.MinIOBucket,
			MinIOUseSSL:            config.MinIOUseSSL,
			RuntimeImage:           config.RuntimeImage,
			RuntimeImagePullPolicy: config.RuntimeImagePullPolicy,
		},
		scheduleRepo,
		cronClient,
	)

	// Create log collector (optional - logs warning if fails)
	var logCollector *K8sLogCollector
	logCollector, err = NewK8sLogCollector(context.Background(), K8sLogCollectorConfig{
		Namespace:      config.Namespace,
		Interval:       30 * time.Second,
		MinIOEndpoint:  config.MinIOEndpoint,
		MinIOAccessKey: config.MinIOAccessKey,
		MinIOSecretKey: config.MinIOSecretKey,
		MinIOUseSSL:    config.MinIOUseSSL,
	}, logger)
	if err != nil {
		logger.Info("WARNING: Failed to create log collector, logs will not be collected from bot pods", "error", err.Error())
	}

	return &Manager{
		config:             config,
		logger:             logger,
		botController:      botCtrl,
		scheduleController: scheduleCtrl,
		logCollector:       logCollector,
		stopCh:             make(chan struct{}),
	}, nil
}

// Start starts all controllers and waits for them to be ready.
// Returns a channel that receives when all controllers are ready.
func (m *Manager) Start(ctx context.Context) (<-chan struct{}, error) {
	m.mu.Lock()
	if m.running {
		m.mu.Unlock()
		return nil, fmt.Errorf("manager already running")
	}
	m.running = true
	m.stopCh = make(chan struct{})
	m.mu.Unlock()

	m.logger.Info("Starting controller manager")

	// Start log collector if available
	if m.logCollector != nil {
		m.logCollector.Start()
		m.logger.Info("Log collector started - collecting logs every 30s")
	}

	// Channel to signal when controllers are ready
	readyCh := make(chan struct{})

	// Start both controllers in parallel
	var wg sync.WaitGroup
	wg.Add(2)

	controllerReadyCh := make(chan struct{}, 2)

	go func() {
		defer wg.Done()
		// Signal readiness after initialization delay
		// 5 seconds allows time for initial reconciliation to complete
		go func() {
			select {
			case <-time.After(5 * time.Second):
				controllerReadyCh <- struct{}{}
			case <-ctx.Done():
			}
		}()
		if err := m.botController.Start(ctx); err != nil && err != context.Canceled {
			m.logger.Error("Bot controller stopped with error", "error", err.Error())
		}
	}()

	go func() {
		defer wg.Done()
		// Signal readiness after initialization delay
		// 5 seconds allows time for initial reconciliation to complete
		go func() {
			select {
			case <-time.After(5 * time.Second):
				controllerReadyCh <- struct{}{}
			case <-ctx.Done():
			}
		}()
		if err := m.scheduleController.Start(ctx); err != nil && err != context.Canceled {
			m.logger.Error("Schedule controller stopped with error", "error", err.Error())
		}
	}()

	// Wait for both controllers to signal readiness
	go func() {
		count := 0
		for count < 2 {
			select {
			case <-controllerReadyCh:
				count++
			case <-ctx.Done():
				return
			}
		}
		m.logger.Info("All controllers ready")
		close(readyCh)
	}()

	// Wait for controllers to stop in background
	go func() {
		wg.Wait()
		m.logger.Info("All controllers stopped")
	}()

	return readyCh, nil
}

// Stop stops all controllers gracefully.
func (m *Manager) Stop() {
	m.mu.Lock()
	defer m.mu.Unlock()

	if !m.running {
		return
	}

	m.logger.Info("Stopping controller manager")

	m.botController.Stop()
	m.scheduleController.Stop()

	if m.logCollector != nil {
		m.logCollector.Stop()
	}

	m.running = false
}
