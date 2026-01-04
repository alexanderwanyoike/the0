// Package controller provides Kubernetes controllers for managing bot Pods.
package controller

import (
	"context"
	"fmt"
	"sync"
	"time"

	"go.mongodb.org/mongo-driver/mongo"
	corev1 "k8s.io/api/core/v1"
	"k8s.io/client-go/kubernetes"
	"k8s.io/client-go/rest"

	"runtime/internal/constants"
	"runtime/internal/k8s/podgen"
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
// It manages the lifecycle of bot controller, schedule controller, and query server.
// Log collection is handled by the daemon sidecar in each pod.
type Manager struct {
	config ManagerConfig
	logger util.Logger

	// Controllers
	botController      *BotController
	scheduleController *BotScheduleController

	// Query server
	queryServer *K8sQueryServer

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

	// Create K8s clientset for query server
	k8sConfig, err := rest.InClusterConfig()
	if err != nil {
		return nil, fmt.Errorf("failed to get in-cluster config: %w", err)
	}

	clientset, err := kubernetes.NewForConfig(k8sConfig)
	if err != nil {
		return nil, fmt.Errorf("failed to create clientset: %w", err)
	}

	// Create pod generator for query handler
	podGenerator := podgen.NewPodGenerator(podgen.PodGeneratorConfig{
		Namespace:              config.Namespace,
		ControllerName:         "the0-query-handler",
		MinIOEndpoint:          config.MinIOEndpoint,
		MinIOAccessKey:         config.MinIOAccessKey,
		MinIOSecretKey:         config.MinIOSecretKey,
		MinIOBucket:            config.MinIOBucket,
		MinIOUseSSL:            config.MinIOUseSSL,
		RuntimeImage:           config.RuntimeImage,
		RuntimeImagePullPolicy: corev1.PullPolicy(config.RuntimeImagePullPolicy),
	})

	// Create query handler
	queryHandler := NewK8sQueryHandler(K8sQueryHandlerConfig{
		Clientset:    clientset,
		Namespace:    config.Namespace,
		PodGenerator: podGenerator,
		Logger:       logger,
	})

	// Create query server
	queryServer := NewK8sQueryServer(K8sQueryServerConfig{
		Port:      9477,
		Handler:   queryHandler,
		BotRepo:   botRepo,
		Clientset: clientset,
		Namespace: config.Namespace,
		Logger:    logger,
	})

	return &Manager{
		config:             config,
		logger:             logger,
		botController:      botCtrl,
		scheduleController: scheduleCtrl,
		queryServer:        queryServer,
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

	// Start query server
	if m.queryServer != nil {
		if err := m.queryServer.Start(); err != nil {
			m.logger.Error("Failed to start query server", "error", err.Error())
		}
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

	// Stop query server
	if m.queryServer != nil {
		ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
		defer cancel()
		if err := m.queryServer.Stop(ctx); err != nil {
			m.logger.Error("Failed to stop query server", "error", err.Error())
		}
	}

	m.running = false
}
