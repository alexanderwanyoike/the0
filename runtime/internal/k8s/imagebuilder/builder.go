package imagebuilder

import (
	"context"
	"fmt"
	"strings"
	"time"

	batchv1 "k8s.io/api/batch/v1"
	corev1 "k8s.io/api/core/v1"
	"k8s.io/apimachinery/pkg/api/errors"
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"
	"k8s.io/client-go/kubernetes"
	"k8s.io/client-go/rest"

	"runtime/internal/bot-runner/model"
	"runtime/internal/util"
)

const (
	// KanikoImage is the default Kaniko executor image.
	KanikoImage = "gcr.io/kaniko-project/executor:latest"

	// BuildJobPrefix is the prefix for Kaniko build job names.
	BuildJobPrefix = "build-bot-"

	// DefaultBuildTimeout is the default timeout for image builds.
	DefaultBuildTimeout = 10 * time.Minute

	// LabelBuildJob identifies Kaniko build jobs.
	LabelBuildJob = "the0.dev/build-job"

	// LabelCustomBotID labels the custom bot ID for the build.
	LabelCustomBotID = "the0.dev/custom-bot-id"

	// LabelCustomBotVersion labels the version being built.
	LabelCustomBotVersion = "the0.dev/custom-bot-version"
)

// K8sJobClient provides access to Kubernetes Job operations.
type K8sJobClient interface {
	// CreateJob creates a new Job.
	CreateJob(ctx context.Context, job *batchv1.Job) error
	// GetJob gets a Job by name and namespace.
	GetJob(ctx context.Context, namespace, name string) (*batchv1.Job, error)
	// DeleteJob deletes a Job by name and namespace.
	DeleteJob(ctx context.Context, namespace, name string) error
	// WaitForJobCompletion waits for a job to complete or fail.
	WaitForJobCompletion(ctx context.Context, namespace, name string, timeout time.Duration) error
}

// MinIOConfig holds MinIO configuration for downloading bot code.
type MinIOConfig struct {
	Endpoint        string
	AccessKeyID     string
	SecretAccessKey string
	Bucket          string
	UseSSL          bool
}

// KanikoBuilderConfig holds configuration for the Kaniko image builder.
type KanikoBuilderConfig struct {
	// Namespace is the Kubernetes namespace for build jobs.
	Namespace string
	// Registry is the container registry URL (e.g., "localhost:5000").
	Registry string
	// MinIO holds MinIO configuration for code access.
	MinIO MinIOConfig
	// BuildTimeout is the maximum time to wait for a build.
	BuildTimeout time.Duration
	// KanikoImage is the Kaniko executor image to use.
	KanikoImage string
	// InsecureRegistry allows HTTP registry communication (for local development).
	// In production, this should be false to enforce TLS.
	InsecureRegistry bool
}

// KanikoImageBuilder builds container images using Kaniko Jobs.
// It implements the controller.ImageBuilder interface.
type KanikoImageBuilder struct {
	config            KanikoBuilderConfig
	registryClient    RegistryClient
	jobClient         K8sJobClient
	dockerfileGen     *DockerfileGenerator
}

// NewKanikoImageBuilder creates a new KanikoImageBuilder.
func NewKanikoImageBuilder(
	config KanikoBuilderConfig,
	registryClient RegistryClient,
	jobClient K8sJobClient,
) *KanikoImageBuilder {
	if config.Namespace == "" {
		config.Namespace = "the0"
	}
	if config.BuildTimeout == 0 {
		config.BuildTimeout = DefaultBuildTimeout
	}
	if config.KanikoImage == "" {
		config.KanikoImage = KanikoImage
	}

	return &KanikoImageBuilder{
		config:         config,
		registryClient: registryClient,
		jobClient:      jobClient,
		dockerfileGen:  NewDockerfileGenerator(),
	}
}

// EnsureImage ensures the bot image exists, building it if needed.
// Returns the image reference to use for the pod.
func (b *KanikoImageBuilder) EnsureImage(ctx context.Context, bot model.Bot) (string, error) {
	customBotID := bot.CustomBotVersion.Config.Name
	if customBotID == "" {
		return "", fmt.Errorf("bot has no custom bot name")
	}
	version := bot.CustomBotVersion.Version
	if version == "" {
		return "", fmt.Errorf("bot has no version")
	}

	// Generate image reference
	imageRef := GenerateImageRef(b.config.Registry, customBotID, version)

	// Check if image already exists
	exists, err := b.registryClient.ImageExists(ctx, imageRef)
	if err != nil {
		util.LogMaster("[ImageBuilder] Failed to check registry for %s: %v", imageRef, err)
		// Continue to build attempt - registry might be temporarily unavailable
	}

	if exists {
		util.LogMaster("[ImageBuilder] Image %s already exists, skipping build", imageRef)
		return imageRef, nil
	}

	// Image doesn't exist, trigger build
	util.LogMaster("[ImageBuilder] Image %s not found, triggering build", imageRef)

	if err := b.buildImage(ctx, bot, imageRef); err != nil {
		return "", fmt.Errorf("failed to build image %s: %w", imageRef, err)
	}

	return imageRef, nil
}

// buildImage creates and runs a Kaniko job to build the image.
func (b *KanikoImageBuilder) buildImage(ctx context.Context, bot model.Bot, imageRef string) error {
	customBotID := bot.CustomBotVersion.Config.Name
	version := bot.CustomBotVersion.Version
	runtime := bot.CustomBotVersion.Config.Runtime
	entrypoint := b.getEntrypoint(bot)
	filePath := bot.CustomBotVersion.FilePath

	// Generate job name
	jobName := b.generateJobName(customBotID, version)

	// Check if job already exists (from a previous attempt)
	existingJob, err := b.jobClient.GetJob(ctx, b.config.Namespace, jobName)
	if err == nil && existingJob != nil {
		util.LogMaster("[ImageBuilder] Build job %s already exists, waiting for completion", jobName)
		return b.jobClient.WaitForJobCompletion(ctx, b.config.Namespace, jobName, b.config.BuildTimeout)
	}

	// Generate Dockerfile content
	dockerfile, err := b.dockerfileGen.GenerateDockerfile(runtime, entrypoint)
	if err != nil {
		return fmt.Errorf("failed to generate Dockerfile: %w", err)
	}

	// Generate entrypoint script
	entrypointScript, err := b.dockerfileGen.GenerateEntrypointScript(runtime, entrypoint)
	if err != nil {
		return fmt.Errorf("failed to generate entrypoint script: %w", err)
	}

	// Create Kaniko job
	job := b.createKanikoJob(jobName, customBotID, version, imageRef, filePath, dockerfile, entrypointScript)

	util.LogMaster("[ImageBuilder] Creating build job %s for %s", jobName, imageRef)
	if err := b.jobClient.CreateJob(ctx, job); err != nil {
		return fmt.Errorf("failed to create build job: %w", err)
	}

	// Wait for job completion
	util.LogMaster("[ImageBuilder] Waiting for build job %s to complete (timeout: %v)", jobName, b.config.BuildTimeout)
	if err := b.jobClient.WaitForJobCompletion(ctx, b.config.Namespace, jobName, b.config.BuildTimeout); err != nil {
		return fmt.Errorf("build job failed: %w", err)
	}

	util.LogMaster("[ImageBuilder] Build job %s completed successfully", jobName)
	return nil
}

// createKanikoJob creates a Kubernetes Job spec for Kaniko.
func (b *KanikoImageBuilder) createKanikoJob(
	jobName, customBotID, version, imageRef, filePath, dockerfile, entrypointScript string,
) *batchv1.Job {
	backoffLimit := int32(2)
	ttlSeconds := int32(3600) // 1 hour TTL after completion

	// MinIO context URL (S3-compatible)
	// Kaniko expects: s3://bucket/path
	contextURL := fmt.Sprintf("s3://%s/%s", b.config.MinIO.Bucket, filePath)

	return &batchv1.Job{
		ObjectMeta: metav1.ObjectMeta{
			Name:      jobName,
			Namespace: b.config.Namespace,
			Labels: map[string]string{
				LabelBuildJob:         "true",
				LabelCustomBotID:      customBotID,
				LabelCustomBotVersion: version,
			},
		},
		Spec: batchv1.JobSpec{
			BackoffLimit:            &backoffLimit,
			TTLSecondsAfterFinished: &ttlSeconds,
			Template: corev1.PodTemplateSpec{
				ObjectMeta: metav1.ObjectMeta{
					Labels: map[string]string{
						LabelBuildJob:         "true",
						LabelCustomBotID:      customBotID,
						LabelCustomBotVersion: version,
					},
				},
				Spec: corev1.PodSpec{
					RestartPolicy: corev1.RestartPolicyNever,
					InitContainers: []corev1.Container{
						{
							Name:  "setup",
							Image: "busybox:latest",
							Command: []string{"/bin/sh", "-c"},
							Args: []string{
								fmt.Sprintf(`
mkdir -p /workspace
echo '%s' > /workspace/Dockerfile
echo '%s' > /workspace/entrypoint.sh
chmod +x /workspace/entrypoint.sh
`, escapeForShell(dockerfile), escapeForShell(entrypointScript)),
							},
							VolumeMounts: []corev1.VolumeMount{
								{Name: "workspace", MountPath: "/workspace"},
							},
						},
					},
					Containers: []corev1.Container{
						{
							Name:  "kaniko",
							Image: b.config.KanikoImage,
							Args: func() []string {
								args := []string{
									"--dockerfile=/workspace/Dockerfile",
									fmt.Sprintf("--context=%s", contextURL),
									fmt.Sprintf("--destination=%s", imageRef),
									"--cache=true",
									"--cache-ttl=24h",
								}
								// Only add --insecure for local/development registries
								if b.config.InsecureRegistry {
									args = append(args, "--insecure")
								}
								return args
							}(),
							Env: []corev1.EnvVar{
								{Name: "AWS_ACCESS_KEY_ID", Value: b.config.MinIO.AccessKeyID},
								{Name: "AWS_SECRET_ACCESS_KEY", Value: b.config.MinIO.SecretAccessKey},
								{Name: "AWS_REGION", Value: "us-east-1"}, // MinIO doesn't care about region
								{Name: "S3_ENDPOINT", Value: b.config.MinIO.Endpoint},
								{Name: "S3_FORCE_PATH_STYLE", Value: "true"},
							},
							VolumeMounts: []corev1.VolumeMount{
								{Name: "workspace", MountPath: "/workspace"},
							},
						},
					},
					Volumes: []corev1.Volume{
						{
							Name: "workspace",
							VolumeSource: corev1.VolumeSource{
								EmptyDir: &corev1.EmptyDirVolumeSource{},
							},
						},
					},
				},
			},
		},
	}
}

// generateJobName generates a unique job name for the build.
func (b *KanikoImageBuilder) generateJobName(customBotID, version string) string {
	// Sanitize for K8s naming - must match DNS subdomain rules
	sanitized := strings.ToLower(customBotID)
	sanitized = strings.ReplaceAll(sanitized, " ", "-")
	sanitized = strings.ReplaceAll(sanitized, ".", "-")

	// Also sanitize version
	sanitizedVersion := strings.ReplaceAll(version, ".", "-")

	// K8s name limit is 63 chars
	name := fmt.Sprintf("%s%s-%s", BuildJobPrefix, sanitized, sanitizedVersion)
	if len(name) > 63 {
		name = name[:63]
	}
	// Ensure doesn't end with dash
	name = strings.TrimRight(name, "-")

	return name
}

// getEntrypoint extracts the bot entrypoint from the config.
func (b *KanikoImageBuilder) getEntrypoint(bot model.Bot) string {
	if bot.CustomBotVersion.Config.Entrypoints != nil {
		if entry, ok := bot.CustomBotVersion.Config.Entrypoints["bot"]; ok {
			return entry
		}
	}
	// Default entrypoints based on runtime
	switch bot.CustomBotVersion.Config.Runtime {
	case "python3.11":
		return "main.py"
	case "nodejs20":
		return "index.js"
	default:
		return "main"
	}
}

// escapeForShell escapes a string for use in shell single quotes.
func escapeForShell(s string) string {
	// Replace single quotes with '\''
	return strings.ReplaceAll(s, "'", "'\\''")
}

// ---- Real K8s Job Client Implementation ----

// RealK8sJobClient implements K8sJobClient using a real Kubernetes clientset.
type RealK8sJobClient struct {
	clientset *kubernetes.Clientset
}

// NewRealK8sJobClient creates a K8sJobClient using in-cluster config.
func NewRealK8sJobClient() (*RealK8sJobClient, error) {
	config, err := rest.InClusterConfig()
	if err != nil {
		return nil, fmt.Errorf("failed to get in-cluster config: %w", err)
	}

	clientset, err := kubernetes.NewForConfig(config)
	if err != nil {
		return nil, fmt.Errorf("failed to create clientset: %w", err)
	}

	return &RealK8sJobClient{clientset: clientset}, nil
}

// NewRealK8sJobClientFromConfig creates a K8sJobClient using provided config.
func NewRealK8sJobClientFromConfig(config *rest.Config) (*RealK8sJobClient, error) {
	clientset, err := kubernetes.NewForConfig(config)
	if err != nil {
		return nil, fmt.Errorf("failed to create clientset: %w", err)
	}

	return &RealK8sJobClient{clientset: clientset}, nil
}

// CreateJob creates a new Job.
func (c *RealK8sJobClient) CreateJob(ctx context.Context, job *batchv1.Job) error {
	_, err := c.clientset.BatchV1().Jobs(job.Namespace).Create(ctx, job, metav1.CreateOptions{})
	return err
}

// GetJob gets a Job by name and namespace.
func (c *RealK8sJobClient) GetJob(ctx context.Context, namespace, name string) (*batchv1.Job, error) {
	job, err := c.clientset.BatchV1().Jobs(namespace).Get(ctx, name, metav1.GetOptions{})
	if errors.IsNotFound(err) {
		return nil, nil
	}
	return job, err
}

// DeleteJob deletes a Job by name and namespace.
func (c *RealK8sJobClient) DeleteJob(ctx context.Context, namespace, name string) error {
	propagation := metav1.DeletePropagationForeground
	return c.clientset.BatchV1().Jobs(namespace).Delete(ctx, name, metav1.DeleteOptions{
		PropagationPolicy: &propagation,
	})
}

// WaitForJobCompletion waits for a job to complete or fail.
func (c *RealK8sJobClient) WaitForJobCompletion(ctx context.Context, namespace, name string, timeout time.Duration) error {
	deadline := time.Now().Add(timeout)
	ticker := time.NewTicker(5 * time.Second)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			return ctx.Err()
		case <-ticker.C:
			if time.Now().After(deadline) {
				return fmt.Errorf("timeout waiting for job %s to complete", name)
			}

			job, err := c.GetJob(ctx, namespace, name)
			if err != nil {
				return fmt.Errorf("failed to get job status: %w", err)
			}
			if job == nil {
				return fmt.Errorf("job %s not found", name)
			}

			// Check for completion
			if job.Status.Succeeded > 0 {
				return nil
			}
			if job.Status.Failed > 0 {
				return fmt.Errorf("job %s failed", name)
			}
		}
	}
}

// ---- Mock K8s Job Client for Testing ----

// MockK8sJobClient is a mock implementation of K8sJobClient for testing.
type MockK8sJobClient struct {
	Jobs             map[string]*batchv1.Job
	CreateError      error
	GetError         error
	DeleteError      error
	WaitError        error
	CreateCalled     int
	GetCalled        int
	DeleteCalled     int
	WaitCalled       int
	SimulateSuccess  bool
}

// NewMockK8sJobClient creates a new MockK8sJobClient.
func NewMockK8sJobClient() *MockK8sJobClient {
	return &MockK8sJobClient{
		Jobs:            make(map[string]*batchv1.Job),
		SimulateSuccess: true,
	}
}

// CreateJob creates a new Job in the mock.
func (m *MockK8sJobClient) CreateJob(ctx context.Context, job *batchv1.Job) error {
	m.CreateCalled++
	if m.CreateError != nil {
		return m.CreateError
	}
	key := fmt.Sprintf("%s/%s", job.Namespace, job.Name)
	m.Jobs[key] = job
	return nil
}

// GetJob gets a Job by name and namespace.
func (m *MockK8sJobClient) GetJob(ctx context.Context, namespace, name string) (*batchv1.Job, error) {
	m.GetCalled++
	if m.GetError != nil {
		return nil, m.GetError
	}
	key := fmt.Sprintf("%s/%s", namespace, name)
	return m.Jobs[key], nil
}

// DeleteJob deletes a Job by name and namespace.
func (m *MockK8sJobClient) DeleteJob(ctx context.Context, namespace, name string) error {
	m.DeleteCalled++
	if m.DeleteError != nil {
		return m.DeleteError
	}
	key := fmt.Sprintf("%s/%s", namespace, name)
	delete(m.Jobs, key)
	return nil
}

// WaitForJobCompletion waits for a job to complete (mock).
func (m *MockK8sJobClient) WaitForJobCompletion(ctx context.Context, namespace, name string, timeout time.Duration) error {
	m.WaitCalled++
	if m.WaitError != nil {
		return m.WaitError
	}
	if m.SimulateSuccess {
		return nil
	}
	return fmt.Errorf("job %s failed", name)
}
