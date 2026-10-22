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
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"
	"k8s.io/client-go/kubernetes"
	"k8s.io/client-go/rest"

	"runtime/internal/desired"
	"runtime/internal/k8s/podgen"
	"runtime/internal/model"
	"runtime/internal/util"
)

const (
	// LabelScheduleID is the label key for the schedule instance ID.
	LabelScheduleID = "the0.app/schedule-id"

	// LabelScheduleManagedBy indicates the controller managing this CronJob.
	LabelScheduleManagedBy = "the0.app/managed-by"

	// AnnotationScheduleHash stores a hash of the schedule config for change detection.
	AnnotationScheduleHash = "the0.app/schedule-hash"
)

// scheduledRunBackoffLimit is 0 because a failed scheduled run may already have
// acted (placed an order, say), so retrying it could act twice. The next run comes
// from the schedule, as in Docker mode, which never retries.
const scheduledRunBackoffLimit int32 = 0

// DesiredSchedules are the schedules that should run on their cron, as the
// desired module decides for both modes.
type DesiredSchedules interface {
	Schedules(ctx context.Context) ([]model.BotSchedule, error)
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
	// MinIO configuration for downloading bot code
	MinIOEndpoint  string
	MinIOAccessKey string
	MinIOSecretKey string
	MinIOBucket    string
	MinIOUseSSL    bool
	// StorageCredentials, when named, is the Secret bot pods read the MinIO
	// credentials from.
	StorageCredentials podgen.StorageCredentialsSecret
	// NATSURL for live log publishing from bot pods (empty disables)
	NATSURL string
	// RuntimeImage for init containers and sidecars
	RuntimeImage string
	// RuntimeImagePullPolicy for init/sidecar containers
	RuntimeImagePullPolicy string
}

// BotScheduleController reconciles bot schedule state between MongoDB and Kubernetes.
type BotScheduleController struct {
	config       BotScheduleControllerConfig
	schedules    DesiredSchedules
	cronClient   K8sCronJobClient
	podGenerator *podgen.PodGenerator

	mu      sync.Mutex
	running bool
	stopCh  chan struct{}
}

// NewBotScheduleController creates a new BotScheduleController.
func NewBotScheduleController(
	config BotScheduleControllerConfig,
	schedules DesiredSchedules,
	cronClient K8sCronJobClient,
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

	// Create pod generator for base image + init container approach
	pg := podgen.NewPodGenerator(podgen.PodGeneratorConfig{
		Namespace:              config.Namespace,
		ControllerName:         config.ControllerName,
		MinIOEndpoint:          config.MinIOEndpoint,
		MinIOAccessKey:         config.MinIOAccessKey,
		MinIOSecretKey:         config.MinIOSecretKey,
		MinIOBucket:            config.MinIOBucket,
		MinIOUseSSL:            config.MinIOUseSSL,
		StorageCredentials:     config.StorageCredentials,
		NATSURL:                config.NATSURL,
		RuntimeImage:           config.RuntimeImage,
		RuntimeImagePullPolicy: corev1.PullPolicy(config.RuntimeImagePullPolicy),
	})

	return &BotScheduleController{
		config:       config,
		schedules:    schedules,
		cronClient:   cronClient,
		podGenerator: pg,
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
	desiredSchedules, err := c.schedules.Schedules(ctx)
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
	desiredMap := make(map[string]model.BotSchedule)
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
func (c *BotScheduleController) ensureCronJobRunning(ctx context.Context, schedule model.BotSchedule) error {
	util.LogMaster("[ScheduleController] Creating CronJob for schedule %s", schedule.ID)

	// Get cron expression from config
	cronExpr, ok := schedule.Config["schedule"].(string)
	if !ok || cronExpr == "" {
		return fmt.Errorf("schedule %s has no cron expression", schedule.ID)
	}

	if _, err := desired.NextExecutionTime(cronExpr, time.Now()); err != nil {
		return fmt.Errorf("schedule %s: %w", schedule.ID, err)
	}

	// Create CronJob spec using podGenerator
	cronJob, err := c.createCronJobSpec(schedule, cronExpr)
	if err != nil {
		return fmt.Errorf("failed to create CronJob spec: %w", err)
	}

	if err := c.cronClient.CreateCronJob(ctx, cronJob); err != nil {
		return fmt.Errorf("failed to create CronJob: %w", err)
	}

	util.LogMaster("[ScheduleController] Created CronJob %s for schedule %s", cronJob.Name, schedule.ID)
	return nil
}

// updateCronJob updates an existing CronJob.
func (c *BotScheduleController) updateCronJob(ctx context.Context, schedule model.BotSchedule, existing *batchv1.CronJob) error {
	cronExpr, ok := schedule.Config["schedule"].(string)
	if !ok || cronExpr == "" {
		return fmt.Errorf("schedule %s has no cron expression", schedule.ID)
	}

	if _, err := desired.NextExecutionTime(cronExpr, time.Now()); err != nil {
		return fmt.Errorf("schedule %s: %w", schedule.ID, err)
	}

	// Update the CronJob
	updated, err := c.createCronJobSpec(schedule, cronExpr)
	if err != nil {
		return fmt.Errorf("failed to create CronJob spec: %w", err)
	}
	updated.ResourceVersion = existing.ResourceVersion

	if err := c.cronClient.UpdateCronJob(ctx, updated); err != nil {
		return fmt.Errorf("failed to update CronJob: %w", err)
	}

	util.LogMaster("[ScheduleController] Updated CronJob %s for schedule %s", updated.Name, schedule.ID)
	return nil
}

// createCronJobSpec creates a CronJob spec for the schedule using base images + init containers.
func (c *BotScheduleController) createCronJobSpec(schedule model.BotSchedule, cronExpr string) (*batchv1.CronJob, error) {
	bot := desired.ScheduleToBot(schedule)

	// Generate scheduled pod spec with daemon sidecar
	podSpec, err := c.podGenerator.GenerateScheduledPodSpec(bot)
	if err != nil {
		return nil, fmt.Errorf("failed to generate pod spec: %w", err)
	}

	successfulJobsHistory := int32(3)
	failedJobsHistory := int32(1)
	backoffLimit := scheduledRunBackoffLimit

	cronJob := &batchv1.CronJob{
		ObjectMeta: metav1.ObjectMeta{
			Name:      generateCronJobName(schedule.ID),
			Namespace: c.config.Namespace,
			Labels: map[string]string{
				LabelScheduleID:         schedule.ID,
				podgen.LabelCustomBotID: schedule.CustomBotVersion.Config.Name,
				podgen.LabelRuntime:     schedule.CustomBotVersion.Config.Runtime,
				LabelScheduleManagedBy:  c.config.ControllerName,
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
								podgen.LabelManagedBy:   c.config.ControllerName,
								podgen.LabelBotID:       schedule.ID, // Use schedule ID as bot ID for log collection
							},
						},
						Spec: *podSpec, // Use the full pod spec from podGenerator (includes init containers + sidecar)
					},
				},
			},
		},
	}
	cronJob.Annotations = map[string]string{
		AnnotationScheduleHash: computeScheduleHash(schedule, cronJob),
	}
	return cronJob, nil
}

// scheduleChanged returns true if the schedule config has changed.
func (c *BotScheduleController) scheduleChanged(cronJob *batchv1.CronJob, schedule model.BotSchedule) bool {
	cronExpr, _ := schedule.Config["schedule"].(string)
	wanted, err := c.createCronJobSpec(schedule, cronExpr)
	if err != nil {
		// updateCronJob reports why the schedule cannot be applied.
		return true
	}
	return cronJob.Annotations[AnnotationScheduleHash] != wanted.Annotations[AnnotationScheduleHash] ||
		backoffLimitDrifted(cronJob)
}

// backoffLimitDrifted reads the limit off the live CronJob because the hash only
// covers inputs: a CronJob created with another limit would otherwise keep it
// until an unrelated input, such as the runtime image tag, happened to change.
// A nil limit means the Kubernetes default of 6.
func backoffLimitDrifted(cronJob *batchv1.CronJob) bool {
	limit := cronJob.Spec.JobTemplate.Spec.BackoffLimit
	return limit == nil || *limit != scheduledRunBackoffLimit
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

// computeScheduleHash hashes the schedule together with the CronJob generated
// for it. Hashing the generated CronJob, not a list of the controller inputs
// behind it, means any change to what the controller writes, such as the
// runtime image, NATS URL or storage credentials, updates existing CronJobs.
func computeScheduleHash(schedule model.BotSchedule, cronJob *batchv1.CronJob) string {
	data := struct {
		Config           map[string]interface{}
		CustomBotVersion string
		Labels           map[string]string
		Spec             batchv1.CronJobSpec
	}{
		Config:           schedule.Config,
		CustomBotVersion: schedule.CustomBotVersion.Version,
		Labels:           cronJob.Labels,
		Spec:             cronJob.Spec,
	}

	jsonBytes, err := json.Marshal(data)
	if err != nil {
		// Return a deterministic fallback hash based on schedule ID to avoid false matches
		if len(schedule.ID) > 8 {
			return fmt.Sprintf("err-%s", schedule.ID[:8])
		}
		return fmt.Sprintf("err-%s", schedule.ID)
	}

	hash := sha256.Sum256(jsonBytes)
	return hex.EncodeToString(hash[:])[:16]
}

// ---- Real K8s CronJob Client Implementation ----

// RealK8sCronJobClient implements K8sCronJobClient.
type RealK8sCronJobClient struct {
	clientset      *kubernetes.Clientset
	controllerName string
}

// NewRealK8sCronJobClient creates a K8sCronJobClient using in-cluster config.
// The controllerName is used for label selectors when listing CronJobs.
func NewRealK8sCronJobClient(controllerName string) (*RealK8sCronJobClient, error) {
	config, err := rest.InClusterConfig()
	if err != nil {
		return nil, fmt.Errorf("failed to get in-cluster config: %w", err)
	}

	clientset, err := kubernetes.NewForConfig(config)
	if err != nil {
		return nil, fmt.Errorf("failed to create clientset: %w", err)
	}

	if controllerName == "" {
		controllerName = "the0-schedule-controller"
	}

	return &RealK8sCronJobClient{clientset: clientset, controllerName: controllerName}, nil
}

// ListCronJobs returns all CronJobs with the schedule label managed by this controller.
func (c *RealK8sCronJobClient) ListCronJobs(ctx context.Context, namespace string) ([]batchv1.CronJob, error) {
	listOpts := metav1.ListOptions{
		LabelSelector: fmt.Sprintf("%s=%s", LabelScheduleManagedBy, c.controllerName),
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
