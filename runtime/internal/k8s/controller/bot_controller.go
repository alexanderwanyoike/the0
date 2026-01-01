// Package controller provides Kubernetes controllers for managing bot Pods
// based on MongoDB state. The controllers implement a reconciliation loop
// that ensures the desired state in MongoDB matches the actual state in K8s.
package controller

import (
	"context"
	"fmt"
	"sync"
	"time"

	corev1 "k8s.io/api/core/v1"
	"k8s.io/apimachinery/pkg/api/errors"
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"
	"k8s.io/client-go/kubernetes"
	"k8s.io/client-go/rest"

	"runtime/internal/bot-runner/model"
	"runtime/internal/k8s/podgen"
	"runtime/internal/util"
)

// BotRepository provides access to bot data from MongoDB.
type BotRepository interface {
	// FindAllEnabled returns all bots that should be running.
	FindAllEnabled(ctx context.Context) ([]model.Bot, error)
}

// K8sClient provides access to Kubernetes Pod operations.
type K8sClient interface {
	// ListBotPods returns all pods managed by this controller.
	ListBotPods(ctx context.Context, namespace string) ([]corev1.Pod, error)
	// CreatePod creates a new pod.
	CreatePod(ctx context.Context, pod *corev1.Pod) error
	// DeletePod deletes a pod by name and namespace.
	DeletePod(ctx context.Context, namespace, name string) error
	// GetPod gets a pod by name and namespace.
	GetPod(ctx context.Context, namespace, name string) (*corev1.Pod, error)
}

// ImageBuilder checks if images exist and builds them if needed.
// Used by BotScheduleController until it's updated to use base images.
type ImageBuilder interface {
	// EnsureImage ensures the bot image exists, building it if needed.
	// Returns the image reference to use for the pod.
	EnsureImage(ctx context.Context, bot model.Bot) (string, error)
}

// BotControllerConfig holds configuration for the bot controller.
type BotControllerConfig struct {
	// Namespace is the Kubernetes namespace for bot pods.
	Namespace string
	// ReconcileInterval is how often to reconcile state.
	ReconcileInterval time.Duration
	// ControllerName identifies this controller for the managed-by label.
	ControllerName string

	// MinIO configuration for downloading bot code
	MinIOEndpoint  string
	MinIOAccessKey string
	MinIOSecretKey string
	MinIOBucket    string
	MinIOUseSSL    bool
}

// BotController reconciles bot state between MongoDB and Kubernetes.
// It ensures that for each enabled bot in MongoDB, there is a corresponding
// Pod running in Kubernetes, and removes Pods for bots that are no longer
// enabled or have been deleted.
type BotController struct {
	config       BotControllerConfig
	botRepo      BotRepository
	k8sClient    K8sClient
	podGenerator *podgen.PodGenerator

	// mu protects running state
	mu      sync.Mutex
	running bool
	stopCh  chan struct{}
}

// NewBotController creates a new BotController.
func NewBotController(
	config BotControllerConfig,
	botRepo BotRepository,
	k8sClient K8sClient,
) *BotController {
	if config.Namespace == "" {
		config.Namespace = "the0"
	}
	if config.ReconcileInterval == 0 {
		config.ReconcileInterval = 30 * time.Second
	}
	if config.ControllerName == "" {
		config.ControllerName = "the0-bot-controller"
	}

	podGenConfig := podgen.PodGeneratorConfig{
		Namespace:      config.Namespace,
		ControllerName: config.ControllerName,
		MinIOEndpoint:  config.MinIOEndpoint,
		MinIOAccessKey: config.MinIOAccessKey,
		MinIOSecretKey: config.MinIOSecretKey,
		MinIOBucket:    config.MinIOBucket,
		MinIOUseSSL:    config.MinIOUseSSL,
	}

	return &BotController{
		config:       config,
		botRepo:      botRepo,
		k8sClient:    k8sClient,
		podGenerator: podgen.NewPodGenerator(podGenConfig),
		stopCh:       make(chan struct{}),
	}
}

// Start begins the reconciliation loop.
func (c *BotController) Start(ctx context.Context) error {
	c.mu.Lock()
	if c.running {
		c.mu.Unlock()
		return fmt.Errorf("controller already running")
	}
	c.running = true
	c.stopCh = make(chan struct{})
	c.mu.Unlock()

	util.LogMaster("[BotController] Starting reconciliation loop (interval: %v)", c.config.ReconcileInterval)

	// Initial reconciliation
	if err := c.Reconcile(ctx); err != nil {
		util.LogMaster("[BotController] Initial reconciliation failed: %v", err)
		// Don't fail startup, just log the error
	}

	ticker := time.NewTicker(c.config.ReconcileInterval)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			util.LogMaster("[BotController] Context cancelled, stopping")
			return ctx.Err()
		case <-c.stopCh:
			util.LogMaster("[BotController] Stop requested")
			return nil
		case <-ticker.C:
			if err := c.Reconcile(ctx); err != nil {
				util.LogMaster("[BotController] Reconciliation failed: %v", err)
				// Continue loop despite errors
			}
		}
	}
}

// Stop stops the reconciliation loop.
func (c *BotController) Stop() {
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.running {
		close(c.stopCh)
		c.running = false
	}
}

// Reconcile performs a single reconciliation cycle.
// It compares the desired state (from MongoDB) with the actual state (from K8s)
// and takes action to align them.
func (c *BotController) Reconcile(ctx context.Context) error {
	util.LogMaster("[BotController] Starting reconciliation")

	// 1. Get desired state from MongoDB (all enabled bots)
	desiredBots, err := c.botRepo.FindAllEnabled(ctx)
	if err != nil {
		return fmt.Errorf("failed to get desired bots from MongoDB: %w", err)
	}
	util.LogMaster("[BotController] Found %d enabled bots in MongoDB", len(desiredBots))

	// 2. Get actual state from K8s (all bot pods)
	actualPods, err := c.k8sClient.ListBotPods(ctx, c.config.Namespace)
	if err != nil {
		return fmt.Errorf("failed to list pods from K8s: %w", err)
	}
	util.LogMaster("[BotController] Found %d bot pods in K8s", len(actualPods))

	// Build maps for efficient lookup
	desiredBotMap := make(map[string]model.Bot)
	for _, bot := range desiredBots {
		desiredBotMap[bot.ID] = bot
	}

	actualPodMap := make(map[string]*corev1.Pod)
	for i := range actualPods {
		pod := &actualPods[i]
		botID := podgen.ExtractBotID(pod)
		if botID != "" {
			actualPodMap[botID] = pod
		}
	}

	// 3. Reconcile: create pods for bots that don't have them
	for _, bot := range desiredBots {
		pod, exists := actualPodMap[bot.ID]

		if !exists {
			// Bot desired but no pod exists -> create pod
			if err := c.ensurePodRunning(ctx, bot); err != nil {
				util.LogMaster("[BotController] Failed to create pod for bot %s: %v", bot.ID, err)
				// Continue with other bots
			}
		} else if podgen.ConfigChanged(pod, bot) {
			// Config changed -> delete pod (will be recreated next cycle)
			util.LogMaster("[BotController] Config changed for bot %s, deleting pod for recreation", bot.ID)
			if err := c.k8sClient.DeletePod(ctx, c.config.Namespace, pod.Name); err != nil {
				util.LogMaster("[BotController] Failed to delete pod for bot %s: %v", bot.ID, err)
			}
		} else if !isPodHealthy(pod) {
			// Pod exists but not healthy -> delete for recreation
			util.LogMaster("[BotController] Pod for bot %s is unhealthy (phase: %s), deleting for recreation", bot.ID, pod.Status.Phase)
			if err := c.k8sClient.DeletePod(ctx, c.config.Namespace, pod.Name); err != nil {
				util.LogMaster("[BotController] Failed to delete unhealthy pod for bot %s: %v", bot.ID, err)
			}
		}
	}

	// 4. Reconcile: delete pods for bots that are no longer desired
	for botID, pod := range actualPodMap {
		if _, exists := desiredBotMap[botID]; !exists {
			// Pod running but bot not desired -> delete
			util.LogMaster("[BotController] Bot %s no longer desired, deleting pod %s", botID, pod.Name)
			if err := c.k8sClient.DeletePod(ctx, c.config.Namespace, pod.Name); err != nil {
				util.LogMaster("[BotController] Failed to delete pod %s: %v", pod.Name, err)
			}
		}
	}

	util.LogMaster("[BotController] Reconciliation complete")
	return nil
}

// ensurePodRunning ensures a pod is running for the given bot.
func (c *BotController) ensurePodRunning(ctx context.Context, bot model.Bot) error {
	util.LogMaster("[BotController] Creating pod for bot %s (custom-bot: %s, version: %s)",
		bot.ID, bot.CustomBotVersion.Config.Name, bot.CustomBotVersion.Version)

	// Generate pod spec (uses base image + init container to download code)
	pod, err := c.podGenerator.GeneratePod(bot)
	if err != nil {
		return fmt.Errorf("failed to generate pod spec for bot %s: %w", bot.ID, err)
	}

	// Create pod
	if err := c.k8sClient.CreatePod(ctx, pod); err != nil {
		return fmt.Errorf("failed to create pod for bot %s: %w", bot.ID, err)
	}

	util.LogMaster("[BotController] Created pod %s for bot %s", pod.Name, bot.ID)
	return nil
}

// isPodHealthy returns true if the pod is in a healthy state.
func isPodHealthy(pod *corev1.Pod) bool {
	switch pod.Status.Phase {
	case corev1.PodRunning:
		return true
	case corev1.PodPending:
		// Pending is okay, pod is being scheduled
		return true
	case corev1.PodSucceeded:
		// For bots with RestartPolicy: Never, this means the bot completed
		// We might want to recreate it depending on use case
		return false
	case corev1.PodFailed:
		return false
	case corev1.PodUnknown:
		return false
	default:
		return true
	}
}

// ---- Implementations ----

// RealK8sClient implements K8sClient using a real Kubernetes clientset.
type RealK8sClient struct {
	clientset      *kubernetes.Clientset
	controllerName string
}

// NewRealK8sClient creates a K8sClient using in-cluster config.
// The controllerName is used for label selectors when listing pods.
func NewRealK8sClient(controllerName string) (*RealK8sClient, error) {
	config, err := rest.InClusterConfig()
	if err != nil {
		return nil, fmt.Errorf("failed to get in-cluster config: %w", err)
	}

	clientset, err := kubernetes.NewForConfig(config)
	if err != nil {
		return nil, fmt.Errorf("failed to create clientset: %w", err)
	}

	if controllerName == "" {
		controllerName = "the0-bot-controller"
	}

	return &RealK8sClient{clientset: clientset, controllerName: controllerName}, nil
}

// NewRealK8sClientFromConfig creates a K8sClient using provided config.
func NewRealK8sClientFromConfig(config *rest.Config, controllerName string) (*RealK8sClient, error) {
	clientset, err := kubernetes.NewForConfig(config)
	if err != nil {
		return nil, fmt.Errorf("failed to create clientset: %w", err)
	}

	if controllerName == "" {
		controllerName = "the0-bot-controller"
	}

	return &RealK8sClient{clientset: clientset, controllerName: controllerName}, nil
}

// ListBotPods returns all pods with the bot label managed by this controller.
func (c *RealK8sClient) ListBotPods(ctx context.Context, namespace string) ([]corev1.Pod, error) {
	listOpts := metav1.ListOptions{
		LabelSelector: fmt.Sprintf("%s=%s", podgen.LabelManagedBy, c.controllerName),
	}

	pods, err := c.clientset.CoreV1().Pods(namespace).List(ctx, listOpts)
	if err != nil {
		return nil, err
	}

	return pods.Items, nil
}

// CreatePod creates a new pod.
func (c *RealK8sClient) CreatePod(ctx context.Context, pod *corev1.Pod) error {
	_, err := c.clientset.CoreV1().Pods(pod.Namespace).Create(ctx, pod, metav1.CreateOptions{})
	return err
}

// DeletePod deletes a pod.
func (c *RealK8sClient) DeletePod(ctx context.Context, namespace, name string) error {
	return c.clientset.CoreV1().Pods(namespace).Delete(ctx, name, metav1.DeleteOptions{})
}

// GetPod gets a pod.
func (c *RealK8sClient) GetPod(ctx context.Context, namespace, name string) (*corev1.Pod, error) {
	pod, err := c.clientset.CoreV1().Pods(namespace).Get(ctx, name, metav1.GetOptions{})
	if errors.IsNotFound(err) {
		return nil, nil
	}
	return pod, err
}

// NoOpImageBuilder is a placeholder image builder that constructs image references
// based on bot metadata. Used by schedule controller until it's updated to use base images.
type NoOpImageBuilder struct {
	Registry       string
	MinIOEndpoint  string
	MinIOAccessKey string
	MinIOSecretKey string
	MinIOBucket    string
}

// NewNoOpImageBuilder creates a NoOpImageBuilder with the given configuration.
func NewNoOpImageBuilder(registry, minioEndpoint, minioAccessKey, minioSecretKey, minioBucket string) *NoOpImageBuilder {
	if registry == "" {
		registry = "localhost:5000"
	}
	return &NoOpImageBuilder{
		Registry:       registry,
		MinIOEndpoint:  minioEndpoint,
		MinIOAccessKey: minioAccessKey,
		MinIOSecretKey: minioSecretKey,
		MinIOBucket:    minioBucket,
	}
}

// EnsureImage constructs an image reference based on bot metadata.
// In the real implementation, this would check the registry and trigger a Kaniko build if needed.
func (b *NoOpImageBuilder) EnsureImage(ctx context.Context, bot model.Bot) (string, error) {
	// In Phase 2, this will:
	// 1. Check if image exists in registry
	// 2. If not, trigger Kaniko job to build it
	// 3. Wait for job completion
	// 4. Return the image reference

	// Extract custom bot ID from config
	customBotID := "unknown"
	if bot.CustomBotVersion.Config.Name != "" {
		customBotID = bot.CustomBotVersion.Config.Name
	}

	// Extract version with fallback
	version := bot.CustomBotVersion.Version
	if version == "" {
		version = "latest"
	}

	// Construct image reference: registry/the0/bots/{customBotId}:{version}
	return fmt.Sprintf("%s/the0/bots/%s:%s", b.Registry, customBotID, version), nil
}
