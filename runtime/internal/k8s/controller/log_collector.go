// Package controller provides the K8s LogCollector component.
//
// LogCollector is a background service that periodically collects logs from
// bot pods and stores them in MinIO. It mirrors the docker-runner's LogCollector
// but uses the Kubernetes API instead of Docker API.
package controller

import (
	"context"
	"fmt"
	"io"
	"sync"
	"time"

	corev1 "k8s.io/api/core/v1"
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"
	"k8s.io/client-go/kubernetes"
	"k8s.io/client-go/rest"

	miniologger "runtime/internal/minio-logger"
	"runtime/internal/k8s/podgen"
	"runtime/internal/util"
)

// K8sLogCollector manages background log collection from bot pods.
type K8sLogCollector struct {
	clientset   *kubernetes.Clientset
	minioLogger miniologger.MinIOLogger
	logger      util.Logger
	namespace   string
	interval    time.Duration

	// Track last log positions per pod to avoid re-uploading
	lastLogTimes   map[string]time.Time
	lastLogTimesMu sync.RWMutex

	// Track completed pods so we only collect their logs once
	completedPods   map[string]bool
	completedPodsMu sync.RWMutex

	ticker *time.Ticker
	stopCh chan struct{}
	doneCh chan struct{}
}

// K8sLogCollectorConfig holds configuration for the log collector.
type K8sLogCollectorConfig struct {
	Namespace      string
	Interval       time.Duration
	MinIOEndpoint  string
	MinIOAccessKey string
	MinIOSecretKey string
	MinIOUseSSL    bool
}

// NewK8sLogCollector creates a new K8s LogCollector.
func NewK8sLogCollector(ctx context.Context, config K8sLogCollectorConfig, logger util.Logger) (*K8sLogCollector, error) {
	// Create K8s client
	k8sConfig, err := rest.InClusterConfig()
	if err != nil {
		return nil, fmt.Errorf("failed to get in-cluster config: %w", err)
	}

	clientset, err := kubernetes.NewForConfig(k8sConfig)
	if err != nil {
		return nil, fmt.Errorf("failed to create K8s clientset: %w", err)
	}

	// Create MinIO logger
	minioLogger, err := miniologger.NewMinIOLogger(ctx, miniologger.MinioLoggerOptions{
		Endpoint:   config.MinIOEndpoint,
		AccessKey:  config.MinIOAccessKey,
		SecretKey:  config.MinIOSecretKey,
		UseSSL:     config.MinIOUseSSL,
		LogsBucket: "bot-logs",
	})
	if err != nil {
		return nil, fmt.Errorf("failed to create MinIO logger: %w", err)
	}

	if config.Interval == 0 {
		config.Interval = 30 * time.Second
	}

	if logger == nil {
		logger = &util.DefaultLogger{}
	}

	return &K8sLogCollector{
		clientset:     clientset,
		minioLogger:   minioLogger,
		logger:        logger,
		namespace:     config.Namespace,
		interval:      config.Interval,
		lastLogTimes:  make(map[string]time.Time),
		completedPods: make(map[string]bool),
		stopCh:        make(chan struct{}),
		doneCh:        make(chan struct{}),
	}, nil
}

// Start begins the background goroutine that periodically collects logs.
func (lc *K8sLogCollector) Start() {
	lc.logger.Info("K8s Log Collector: Starting log collection service", "interval", lc.interval.String())
	lc.ticker = time.NewTicker(lc.interval)

	go func() {
		defer close(lc.doneCh)
		for {
			select {
			case <-lc.ticker.C:
				lc.collectAllLogs()
			case <-lc.stopCh:
				lc.logger.Info("K8s Log Collector: Stopping log collection service")
				lc.ticker.Stop()
				return
			}
		}
	}()
}

// Stop signals the background goroutine to stop and waits for it to finish.
func (lc *K8sLogCollector) Stop() {
	close(lc.stopCh)
	<-lc.doneCh
	if lc.minioLogger != nil {
		lc.minioLogger.Close()
	}
}

// collectAllLogs fetches logs from all managed bot pods.
func (lc *K8sLogCollector) collectAllLogs() {
	ctx, cancel := context.WithTimeout(context.Background(), 60*time.Second)
	defer cancel()

	// Collect from two sources:
	// 1. Realtime bot pods (have bot-id label)
	// 2. Schedule pods (have schedule-id label)
	var allPods []corev1.Pod

	// Get realtime bot pods
	botPods, err := lc.clientset.CoreV1().Pods(lc.namespace).List(ctx, metav1.ListOptions{
		LabelSelector: podgen.LabelBotID,
	})
	if err != nil {
		lc.logger.Error("K8s Log Collector: Failed to list bot pods", "error", err.Error())
	} else {
		allPods = append(allPods, botPods.Items...)
	}

	// Get schedule pods
	schedulePods, err := lc.clientset.CoreV1().Pods(lc.namespace).List(ctx, metav1.ListOptions{
		LabelSelector: LabelScheduleID,
	})
	if err != nil {
		lc.logger.Error("K8s Log Collector: Failed to list schedule pods", "error", err.Error())
	} else {
		// Add schedule pods, avoiding duplicates (in case a pod has both labels)
		seen := make(map[string]bool)
		for _, p := range allPods {
			seen[p.Name] = true
		}
		for _, p := range schedulePods.Items {
			if !seen[p.Name] {
				allPods = append(allPods, p)
			}
		}
	}

	if len(allPods) == 0 {
		return
	}

	pods := &corev1.PodList{Items: allPods}

	lc.logger.Info("K8s Log Collector: Collecting logs from pods", "count", len(pods.Items))

	for _, pod := range pods.Items {
		// Collect logs from running pods AND recently completed pods (for CronJob pods)
		switch pod.Status.Phase {
		case corev1.PodRunning:
			// Always collect from running pods
			go lc.collectAndStoreLogs(pod, false)
		case corev1.PodSucceeded, corev1.PodFailed:
			// Only collect from completed pods if we haven't already
			lc.completedPodsMu.RLock()
			alreadyCollected := lc.completedPods[pod.Name]
			lc.completedPodsMu.RUnlock()
			if !alreadyCollected {
				go lc.collectAndStoreLogs(pod, true)
			}
		}
	}
}

// collectAndStoreLogs fetches logs from a single pod and stores them in MinIO.
// If isCompleted is true, this is a final collection from a completed pod.
func (lc *K8sLogCollector) collectAndStoreLogs(pod corev1.Pod, isCompleted bool) {
	// Get bot ID - try bot-id first, then fall back to schedule-id
	botID := pod.Labels[podgen.LabelBotID]
	if botID == "" {
		botID = pod.Labels[LabelScheduleID]
	}
	if botID == "" {
		return
	}

	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()

	// Build log options - only collect from the "bot" container
	logOpts := &corev1.PodLogOptions{
		Container:  "bot",
		Timestamps: true, // Include timestamps to help with deduplication
	}

	// For running pods, only get logs since last collection using exact time
	// This prevents duplicate logs from overlapping time windows
	if !isCompleted {
		lc.lastLogTimesMu.RLock()
		sinceTime := lc.lastLogTimes[pod.Name]
		lc.lastLogTimesMu.RUnlock()

		if !sinceTime.IsZero() {
			// Use SinceTime for precise log collection without overlap
			metaTime := metav1.NewTime(sinceTime)
			logOpts.SinceTime = &metaTime
		}
	}

	// Get logs from the pod
	req := lc.clientset.CoreV1().Pods(lc.namespace).GetLogs(pod.Name, logOpts)
	logStream, err := req.Stream(ctx)
	if err != nil {
		lc.logger.Error("K8s Log Collector: Failed to get logs", "pod", pod.Name, "error", err.Error())
		return
	}
	defer logStream.Close()

	// Read logs
	logBytes, err := io.ReadAll(logStream)
	if err != nil {
		lc.logger.Error("K8s Log Collector: Failed to read logs", "pod", pod.Name, "error", err.Error())
		return
	}

	logs := string(logBytes)
	if len(logs) == 0 {
		// Even if no logs, update the tracking time to current
		if !isCompleted {
			lc.lastLogTimesMu.Lock()
			lc.lastLogTimes[pod.Name] = time.Now()
			lc.lastLogTimesMu.Unlock()
		} else {
			lc.completedPodsMu.Lock()
			lc.completedPods[pod.Name] = true
			lc.completedPodsMu.Unlock()
		}
		return
	}

	// Upload logs to MinIO
	if err := lc.minioLogger.AppendBotLogs(ctx, botID, logs); err != nil {
		lc.logger.Error("K8s Log Collector: Failed to store logs in MinIO", "bot_id", botID, "error", err.Error())
		return
	}

	// Update tracking with current time AFTER successful upload
	now := time.Now()
	if isCompleted {
		// Mark as collected so we don't re-upload
		lc.completedPodsMu.Lock()
		lc.completedPods[pod.Name] = true
		lc.completedPodsMu.Unlock()
		lc.logger.Info("K8s Log Collector: Stored final logs", "bot_id", botID, "pod", pod.Name, "size", len(logs))
	} else {
		// Update last log time for running pods
		lc.lastLogTimesMu.Lock()
		lc.lastLogTimes[pod.Name] = now
		lc.lastLogTimesMu.Unlock()
		lc.logger.Info("K8s Log Collector: Stored logs", "bot_id", botID, "size", len(logs))
	}
}
