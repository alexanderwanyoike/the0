package controller

import (
	"context"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"strings"
	"sync"
	"time"

	batchv1 "k8s.io/api/batch/v1"
	corev1 "k8s.io/api/core/v1"
	"k8s.io/apimachinery/pkg/api/errors"
	"k8s.io/apimachinery/pkg/api/resource"
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"
	"k8s.io/client-go/kubernetes"
	"k8s.io/client-go/rest"

	botModel "runtime/internal/bot-runner/model"
	scheduleModel "runtime/internal/bot-scheduler/model"
	"runtime/internal/k8s/podgen"
	"runtime/internal/util"
)

const (
	// LabelScheduleID is the label key for the schedule instance ID.
	LabelScheduleID = "the0.dev/schedule-id"

	// LabelScheduleManagedBy indicates the controller managing this CronJob.
	LabelScheduleManagedBy = "the0.dev/managed-by"

	// AnnotationScheduleHash stores a hash of the schedule config for change detection.
	AnnotationScheduleHash = "the0.dev/schedule-hash"
)

// BotScheduleRepository provides access to bot schedule data from MongoDB.
type BotScheduleRepository interface {
	// FindAllEnabled returns all schedules that should be running.
	FindAllEnabled(ctx context.Context) ([]scheduleModel.BotSchedule, error)
}

// K8sCronJobClient provides access to Kubernetes CronJob operations.
type K8sCronJobClient interface {
	// ListCronJobs returns all CronJobs managed by this controller.
	ListCronJobs(ctx context.Context, namespace string) ([]batchv1.CronJob, error)
	// CreateCronJob creates a new CronJob.
	CreateCronJob(ctx context.Context, cronJob *batchv1.CronJob) error
	// UpdateCronJob updates an existing CronJob.
	UpdateCronJob(ctx context.Context, cronJob *batchv1.CronJob) error
	// DeleteCronJob deletes a CronJob by name and namespace.
	DeleteCronJob(ctx context.Context, namespace, name string) error
	// GetCronJob gets a CronJob by name and namespace.
	GetCronJob(ctx context.Context, namespace, name string) (*batchv1.CronJob, error)
}

// BotScheduleControllerConfig holds configuration for the schedule controller.
type BotScheduleControllerConfig struct {
	// Namespace is the Kubernetes namespace for CronJobs.
	Namespace string
	// ReconcileInterval is how often to reconcile state.
	ReconcileInterval time.Duration
	// ControllerName identifies this controller for the managed-by label.
	ControllerName string
}

// BotScheduleController reconciles bot schedule state between MongoDB and Kubernetes.
type BotScheduleController struct {
	config       BotScheduleControllerConfig
	scheduleRepo BotScheduleRepository
	cronClient   K8sCronJobClient
	imageBuilder ImageBuilder

	mu      sync.Mutex
	running bool
	stopCh  chan struct{}
}

// NewBotScheduleController creates a new BotScheduleController.
func NewBotScheduleController(
	config BotScheduleControllerConfig,
	scheduleRepo BotScheduleRepository,
	cronClient K8sCronJobClient,
	imageBuilder ImageBuilder,
) *BotScheduleController {
	if config.Namespace == "" {
		config.Namespace = "the0"
	}
	if config.ReconcileInterval == 0 {
		config.ReconcileInterval = 30 * time.Second
	}
	if config.ControllerName == "" {
		config.ControllerName = "the0-schedule-controller"
	}

	return &BotScheduleController{
		config:       config,
		scheduleRepo: scheduleRepo,
		cronClient:   cronClient,
		imageBuilder: imageBuilder,
		stopCh:       make(chan struct{}),
	}
}

// Start begins the reconciliation loop.
func (c *BotScheduleController) Start(ctx context.Context) error {
	c.mu.Lock()
	if c.running {
		c.mu.Unlock()
		return fmt.Errorf("controller already running")
	}
	c.running = true
	c.stopCh = make(chan struct{})
	c.mu.Unlock()

	util.LogMaster("[ScheduleController] Starting reconciliation loop (interval: %v)", c.config.ReconcileInterval)

	// Initial reconciliation
	if err := c.Reconcile(ctx); err != nil {
		util.LogMaster("[ScheduleController] Initial reconciliation failed: %v", err)
	}

	ticker := time.NewTicker(c.config.ReconcileInterval)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			util.LogMaster("[ScheduleController] Context cancelled, stopping")
			return ctx.Err()
		case <-c.stopCh:
			util.LogMaster("[ScheduleController] Stop requested")
			return nil
		case <-ticker.C:
			if err := c.Reconcile(ctx); err != nil {
				util.LogMaster("[ScheduleController] Reconciliation failed: %v", err)
			}
		}
	}
}

// Stop stops the reconciliation loop.
func (c *BotScheduleController) Stop() {
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.running {
		close(c.stopCh)
		c.running = false
	}
}

// Reconcile performs a single reconciliation cycle.
func (c *BotScheduleController) Reconcile(ctx context.Context) error {
	util.LogMaster("[ScheduleController] Starting reconciliation")

	// 1. Get desired state from MongoDB
	desiredSchedules, err := c.scheduleRepo.FindAllEnabled(ctx)
	if err != nil {
		return fmt.Errorf("failed to get schedules from MongoDB: %w", err)
	}
	util.LogMaster("[ScheduleController] Found %d enabled schedules in MongoDB", len(desiredSchedules))

	// 2. Get actual state from K8s
	actualCronJobs, err := c.cronClient.ListCronJobs(ctx, c.config.Namespace)
	if err != nil {
		return fmt.Errorf("failed to list CronJobs from K8s: %w", err)
	}
	util.LogMaster("[ScheduleController] Found %d CronJobs in K8s", len(actualCronJobs))

	// Build maps for lookup
	desiredMap := make(map[string]scheduleModel.BotSchedule)
	for _, schedule := range desiredSchedules {
		desiredMap[schedule.ID] = schedule
	}

	actualMap := make(map[string]*batchv1.CronJob)
	for i := range actualCronJobs {
		cj := &actualCronJobs[i]
		scheduleID := extractScheduleID(cj)
		if scheduleID != "" {
			actualMap[scheduleID] = cj
		}
	}

	// 3. Create/update CronJobs for desired schedules
	for _, schedule := range desiredSchedules {
		cronJob, exists := actualMap[schedule.ID]

		if !exists {
			// Schedule desired but no CronJob -> create
			if err := c.ensureCronJobRunning(ctx, schedule); err != nil {
				util.LogMaster("[ScheduleController] Failed to create CronJob for schedule %s: %v", schedule.ID, err)
			}
		} else if c.scheduleChanged(cronJob, schedule) {
			// Config changed -> update CronJob
			util.LogMaster("[ScheduleController] Config changed for schedule %s, updating CronJob", schedule.ID)
			if err := c.updateCronJob(ctx, schedule, cronJob); err != nil {
				util.LogMaster("[ScheduleController] Failed to update CronJob for schedule %s: %v", schedule.ID, err)
			}
		}
	}

	// 4. Delete CronJobs for removed schedules
	for scheduleID, cronJob := range actualMap {
		if _, exists := desiredMap[scheduleID]; !exists {
			util.LogMaster("[ScheduleController] Schedule %s no longer desired, deleting CronJob %s", scheduleID, cronJob.Name)
			if err := c.cronClient.DeleteCronJob(ctx, c.config.Namespace, cronJob.Name); err != nil {
				util.LogMaster("[ScheduleController] Failed to delete CronJob %s: %v", cronJob.Name, err)
			}
		}
	}

	util.LogMaster("[ScheduleController] Reconciliation complete")
	return nil
}

// ensureCronJobRunning creates a CronJob for the schedule.
func (c *BotScheduleController) ensureCronJobRunning(ctx context.Context, schedule scheduleModel.BotSchedule) error {
	util.LogMaster("[ScheduleController] Creating CronJob for schedule %s", schedule.ID)

	// Get cron expression from config
	cronExpr, ok := schedule.Config["schedule"].(string)
	if !ok || cronExpr == "" {
		return fmt.Errorf("schedule %s has no cron expression", schedule.ID)
	}

	// Convert to K8s cron format if needed
	k8sCronExpr := convertToK8sCronFormat(cronExpr)

	// Ensure image exists
	imageRef, err := c.imageBuilder.EnsureImage(ctx, c.scheduleToBot(schedule))
	if err != nil {
		return fmt.Errorf("failed to ensure image: %w", err)
	}

	// Create CronJob spec
	cronJob := c.createCronJobSpec(schedule, k8sCronExpr, imageRef)

	if err := c.cronClient.CreateCronJob(ctx, cronJob); err != nil {
		return fmt.Errorf("failed to create CronJob: %w", err)
	}

	util.LogMaster("[ScheduleController] Created CronJob %s for schedule %s", cronJob.Name, schedule.ID)
	return nil
}

// updateCronJob updates an existing CronJob.
func (c *BotScheduleController) updateCronJob(ctx context.Context, schedule scheduleModel.BotSchedule, existing *batchv1.CronJob) error {
	cronExpr, ok := schedule.Config["schedule"].(string)
	if !ok || cronExpr == "" {
		return fmt.Errorf("schedule %s has no cron expression", schedule.ID)
	}

	k8sCronExpr := convertToK8sCronFormat(cronExpr)

	imageRef, err := c.imageBuilder.EnsureImage(ctx, c.scheduleToBot(schedule))
	if err != nil {
		return fmt.Errorf("failed to ensure image: %w", err)
	}

	// Update the CronJob
	updated := c.createCronJobSpec(schedule, k8sCronExpr, imageRef)
	updated.ResourceVersion = existing.ResourceVersion

	if err := c.cronClient.UpdateCronJob(ctx, updated); err != nil {
		return fmt.Errorf("failed to update CronJob: %w", err)
	}

	util.LogMaster("[ScheduleController] Updated CronJob %s for schedule %s", updated.Name, schedule.ID)
	return nil
}

// createCronJobSpec creates a CronJob spec for the schedule.
func (c *BotScheduleController) createCronJobSpec(schedule scheduleModel.BotSchedule, cronExpr, imageRef string) *batchv1.CronJob {
	configJSON, _ := json.Marshal(schedule.Config)
	configHash := computeScheduleHash(schedule)

	// Resource limits
	memoryLimit := "512Mi"
	cpuLimit := "500m"
	memoryRequest := "128Mi"
	cpuRequest := "100m"

	if m, ok := schedule.Config["memory_limit"].(string); ok && m != "" {
		memoryLimit = m
	}
	if c, ok := schedule.Config["cpu_limit"].(string); ok && c != "" {
		cpuLimit = c
	}

	successfulJobsHistory := int32(3)
	failedJobsHistory := int32(1)
	backoffLimit := int32(2)

	return &batchv1.CronJob{
		ObjectMeta: metav1.ObjectMeta{
			Name:      generateCronJobName(schedule.ID),
			Namespace: c.config.Namespace,
			Labels: map[string]string{
				LabelScheduleID:            schedule.ID,
				podgen.LabelCustomBotID:    schedule.CustomBotVersion.Config.Name,
				podgen.LabelRuntime:        schedule.CustomBotVersion.Config.Runtime,
				LabelScheduleManagedBy:     c.config.ControllerName,
			},
			Annotations: map[string]string{
				AnnotationScheduleHash: configHash,
			},
		},
		Spec: batchv1.CronJobSpec{
			Schedule:                   cronExpr,
			SuccessfulJobsHistoryLimit: &successfulJobsHistory,
			FailedJobsHistoryLimit:     &failedJobsHistory,
			ConcurrencyPolicy:          batchv1.ForbidConcurrent,
			JobTemplate: batchv1.JobTemplateSpec{
				Spec: batchv1.JobSpec{
					BackoffLimit: &backoffLimit,
					Template: corev1.PodTemplateSpec{
						ObjectMeta: metav1.ObjectMeta{
							Labels: map[string]string{
								LabelScheduleID:         schedule.ID,
								podgen.LabelCustomBotID: schedule.CustomBotVersion.Config.Name,
							},
						},
						Spec: corev1.PodSpec{
							RestartPolicy: corev1.RestartPolicyNever,
							Containers: []corev1.Container{
								{
									Name:       "bot",
									Image:      imageRef,
									Command:    []string{"/bin/bash", "/bot/entrypoint.sh"},
									WorkingDir: "/bot",
									Env: []corev1.EnvVar{
										{Name: "BOT_ID", Value: schedule.ID},
										{Name: "BOT_CONFIG", Value: string(configJSON)},
										{Name: "CODE_MOUNT_DIR", Value: "/bot"},
									},
									Resources: corev1.ResourceRequirements{
										Limits: corev1.ResourceList{
											corev1.ResourceMemory: mustParseQuantity(memoryLimit),
											corev1.ResourceCPU:    mustParseQuantity(cpuLimit),
										},
										Requests: corev1.ResourceList{
											corev1.ResourceMemory: mustParseQuantity(memoryRequest),
											corev1.ResourceCPU:    mustParseQuantity(cpuRequest),
										},
									},
								},
							},
						},
					},
				},
			},
		},
	}
}

// scheduleChanged returns true if the schedule config has changed.
func (c *BotScheduleController) scheduleChanged(cronJob *batchv1.CronJob, schedule scheduleModel.BotSchedule) bool {
	existingHash := ""
	if cronJob.Annotations != nil {
		existingHash = cronJob.Annotations[AnnotationScheduleHash]
	}
	currentHash := computeScheduleHash(schedule)
	return existingHash != currentHash
}

// scheduleToBot converts a BotSchedule to a Bot model for the image builder.
func (c *BotScheduleController) scheduleToBot(schedule scheduleModel.BotSchedule) botModel.Bot {
	enabled := true
	if schedule.Enabled != nil {
		enabled = *schedule.Enabled
	}
	return botModel.Bot{
		ID:     schedule.ID,
		Config: schedule.Config,
		CustomBotVersion: botModel.CustomBotVersion{
			Version:  schedule.CustomBotVersion.Version,
			FilePath: schedule.CustomBotVersion.FilePath,
			Config: botModel.APIBotConfig{
				Name:        schedule.CustomBotVersion.Config.Name,
				Runtime:     schedule.CustomBotVersion.Config.Runtime,
				Entrypoints: schedule.CustomBotVersion.Config.Entrypoints,
			},
		},
		Enabled: &enabled,
	}
}

// extractScheduleID extracts the schedule ID from a CronJob's labels.
func extractScheduleID(cronJob *batchv1.CronJob) string {
	if cronJob.Labels == nil {
		return ""
	}
	return cronJob.Labels[LabelScheduleID]
}

// generateCronJobName generates the CronJob name for a schedule ID.
func generateCronJobName(scheduleID string) string {
	name := fmt.Sprintf("schedule-%s", scheduleID)
	// K8s name limit
	if len(name) > 52 { // Leave room for job suffix
		name = name[:52]
	}
	return strings.TrimRight(name, "-")
}

// convertToK8sCronFormat converts cron expressions to K8s format.
// K8s CronJobs only support 5-field cron (minute hour day month weekday).
// If the expression has 6 fields (with seconds), we strip the seconds field.
func convertToK8sCronFormat(cronExpr string) string {
	fields := strings.Fields(cronExpr)
	if len(fields) == 6 {
		// Strip the seconds field (first field)
		return strings.Join(fields[1:], " ")
	}
	return cronExpr
}

// computeScheduleHash creates a hash of the schedule config.
func computeScheduleHash(schedule scheduleModel.BotSchedule) string {
	data := struct {
		Config           map[string]interface{}
		CustomBotVersion string
		Enabled          *bool
	}{
		Config:           schedule.Config,
		CustomBotVersion: schedule.CustomBotVersion.Version,
		Enabled:          schedule.Enabled,
	}

	jsonBytes, err := json.Marshal(data)
	if err != nil {
		return ""
	}

	hash := sha256.Sum256(jsonBytes)
	return hex.EncodeToString(hash[:])[:16]
}

// mustParseQuantity parses a resource quantity string.
func mustParseQuantity(s string) resource.Quantity {
	q, err := resource.ParseQuantity(s)
	if err != nil {
		return resource.MustParse("0")
	}
	return q
}

// ---- Real K8s CronJob Client Implementation ----

// RealK8sCronJobClient implements K8sCronJobClient.
type RealK8sCronJobClient struct {
	clientset *kubernetes.Clientset
}

// NewRealK8sCronJobClient creates a K8sCronJobClient using in-cluster config.
func NewRealK8sCronJobClient() (*RealK8sCronJobClient, error) {
	config, err := rest.InClusterConfig()
	if err != nil {
		return nil, fmt.Errorf("failed to get in-cluster config: %w", err)
	}

	clientset, err := kubernetes.NewForConfig(config)
	if err != nil {
		return nil, fmt.Errorf("failed to create clientset: %w", err)
	}

	return &RealK8sCronJobClient{clientset: clientset}, nil
}

// ListCronJobs returns all CronJobs with the schedule label.
func (c *RealK8sCronJobClient) ListCronJobs(ctx context.Context, namespace string) ([]batchv1.CronJob, error) {
	listOpts := metav1.ListOptions{
		LabelSelector: LabelScheduleManagedBy,
	}

	cronJobs, err := c.clientset.BatchV1().CronJobs(namespace).List(ctx, listOpts)
	if err != nil {
		return nil, err
	}

	return cronJobs.Items, nil
}

// CreateCronJob creates a new CronJob.
func (c *RealK8sCronJobClient) CreateCronJob(ctx context.Context, cronJob *batchv1.CronJob) error {
	_, err := c.clientset.BatchV1().CronJobs(cronJob.Namespace).Create(ctx, cronJob, metav1.CreateOptions{})
	return err
}

// UpdateCronJob updates an existing CronJob.
func (c *RealK8sCronJobClient) UpdateCronJob(ctx context.Context, cronJob *batchv1.CronJob) error {
	_, err := c.clientset.BatchV1().CronJobs(cronJob.Namespace).Update(ctx, cronJob, metav1.UpdateOptions{})
	return err
}

// DeleteCronJob deletes a CronJob.
func (c *RealK8sCronJobClient) DeleteCronJob(ctx context.Context, namespace, name string) error {
	propagation := metav1.DeletePropagationForeground
	return c.clientset.BatchV1().CronJobs(namespace).Delete(ctx, name, metav1.DeleteOptions{
		PropagationPolicy: &propagation,
	})
}

// GetCronJob gets a CronJob.
func (c *RealK8sCronJobClient) GetCronJob(ctx context.Context, namespace, name string) (*batchv1.CronJob, error) {
	cronJob, err := c.clientset.BatchV1().CronJobs(namespace).Get(ctx, name, metav1.GetOptions{})
	if errors.IsNotFound(err) {
		return nil, nil
	}
	return cronJob, err
}

// ---- Mock K8s CronJob Client for Testing ----

// MockK8sCronJobClient is a mock implementation for testing.
type MockK8sCronJobClient struct {
	CronJobs     map[string]*batchv1.CronJob
	ListError    error
	CreateError  error
	UpdateError  error
	DeleteError  error
	ListCalled   int
	CreateCalled int
	UpdateCalled int
	DeleteCalled int
}

// NewMockK8sCronJobClient creates a new MockK8sCronJobClient.
func NewMockK8sCronJobClient() *MockK8sCronJobClient {
	return &MockK8sCronJobClient{
		CronJobs: make(map[string]*batchv1.CronJob),
	}
}

// ListCronJobs returns mock CronJobs.
func (m *MockK8sCronJobClient) ListCronJobs(ctx context.Context, namespace string) ([]batchv1.CronJob, error) {
	m.ListCalled++
	if m.ListError != nil {
		return nil, m.ListError
	}
	result := make([]batchv1.CronJob, 0, len(m.CronJobs))
	for _, cj := range m.CronJobs {
		result = append(result, *cj)
	}
	return result, nil
}

// CreateCronJob creates a mock CronJob.
func (m *MockK8sCronJobClient) CreateCronJob(ctx context.Context, cronJob *batchv1.CronJob) error {
	m.CreateCalled++
	if m.CreateError != nil {
		return m.CreateError
	}
	key := fmt.Sprintf("%s/%s", cronJob.Namespace, cronJob.Name)
	m.CronJobs[key] = cronJob
	return nil
}

// UpdateCronJob updates a mock CronJob.
func (m *MockK8sCronJobClient) UpdateCronJob(ctx context.Context, cronJob *batchv1.CronJob) error {
	m.UpdateCalled++
	if m.UpdateError != nil {
		return m.UpdateError
	}
	key := fmt.Sprintf("%s/%s", cronJob.Namespace, cronJob.Name)
	m.CronJobs[key] = cronJob
	return nil
}

// DeleteCronJob deletes a mock CronJob.
func (m *MockK8sCronJobClient) DeleteCronJob(ctx context.Context, namespace, name string) error {
	m.DeleteCalled++
	if m.DeleteError != nil {
		return m.DeleteError
	}
	key := fmt.Sprintf("%s/%s", namespace, name)
	delete(m.CronJobs, key)
	return nil
}

// GetCronJob gets a mock CronJob.
func (m *MockK8sCronJobClient) GetCronJob(ctx context.Context, namespace, name string) (*batchv1.CronJob, error) {
	key := fmt.Sprintf("%s/%s", namespace, name)
	return m.CronJobs[key], nil
}
