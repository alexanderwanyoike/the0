// Package podgen provides Pod specification generation for Kubernetes-native bot execution.
package podgen

import (
	"encoding/json"
	"fmt"

	corev1 "k8s.io/api/core/v1"
	"k8s.io/apimachinery/pkg/api/resource"
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"
	"k8s.io/apimachinery/pkg/util/intstr"
)

// PodBuilder provides a fluent interface for building Kubernetes Pod specs.
// Use NewPodBuilder() to create a builder, chain With* methods, then call Build().
type PodBuilder struct {
	name        string
	namespace   string
	labels      map[string]string
	annotations map[string]string

	// Main bot container config
	botImage           string
	botImagePullPolicy corev1.PullPolicy
	botCommand         []string
	botEnv             []corev1.EnvVar
	botResources       corev1.ResourceRequirements

	// Sidecar containers (optional)
	syncSidecar  *corev1.Container
	querySidecar *corev1.Container

	// Pod-level config
	restartPolicy corev1.RestartPolicy
	volumes       []corev1.Volume
	volumeMounts  []corev1.VolumeMount

	// Validation errors collected during building
	errors []error
}

// NewPodBuilder creates a new PodBuilder with sensible defaults.
func NewPodBuilder(name, namespace string) *PodBuilder {
	return &PodBuilder{
		name:          name,
		namespace:     namespace,
		labels:        make(map[string]string),
		annotations:   make(map[string]string),
		restartPolicy: corev1.RestartPolicyAlways,
		botCommand:    []string{"/app/runtime", "execute", "--skip-sync", "--skip-query-server"},
		botImagePullPolicy: corev1.PullIfNotPresent,
		volumes: []corev1.Volume{
			{Name: "bot-code", VolumeSource: corev1.VolumeSource{EmptyDir: &corev1.EmptyDirVolumeSource{}}},
			{Name: "bot-state", VolumeSource: corev1.VolumeSource{EmptyDir: &corev1.EmptyDirVolumeSource{}}},
			{Name: "the0", VolumeSource: corev1.VolumeSource{EmptyDir: &corev1.EmptyDirVolumeSource{}}},
		},
		volumeMounts: []corev1.VolumeMount{
			{Name: "bot-code", MountPath: "/bot"},
			{Name: "bot-state", MountPath: "/state"},
			{Name: "the0", MountPath: "/var/the0"},
		},
		botResources: corev1.ResourceRequirements{
			Limits: corev1.ResourceList{
				corev1.ResourceMemory: resource.MustParse(DefaultMemoryLimit),
				corev1.ResourceCPU:    resource.MustParse(DefaultCPULimit),
			},
			Requests: corev1.ResourceList{
				corev1.ResourceMemory: resource.MustParse(DefaultMemoryRequest),
				corev1.ResourceCPU:    resource.MustParse(DefaultCPURequest),
			},
		},
	}
}

// WithImage sets the container image for the bot.
func (b *PodBuilder) WithImage(image string) *PodBuilder {
	b.botImage = image
	return b
}

// WithImagePullPolicy sets the image pull policy.
func (b *PodBuilder) WithImagePullPolicy(policy corev1.PullPolicy) *PodBuilder {
	b.botImagePullPolicy = policy
	return b
}

// WithLabels adds labels to the pod.
func (b *PodBuilder) WithLabels(labels map[string]string) *PodBuilder {
	for k, v := range labels {
		b.labels[k] = v
	}
	return b
}

// WithAnnotations adds annotations to the pod.
func (b *PodBuilder) WithAnnotations(annotations map[string]string) *PodBuilder {
	for k, v := range annotations {
		b.annotations[k] = v
	}
	return b
}

// WithRestartPolicy sets the pod restart policy.
func (b *PodBuilder) WithRestartPolicy(policy corev1.RestartPolicy) *PodBuilder {
	b.restartPolicy = policy
	return b
}

// WithInlineSync configures the bot to run sync inline (not as a sidecar).
// Use this for scheduled bots where sync runs as a process after bot execution.
func (b *PodBuilder) WithInlineSync() *PodBuilder {
	// Remove --skip-sync from command so sync runs inline
	b.botCommand = []string{"/app/runtime", "execute", "--skip-query-server"}
	return b
}

// WithEnv adds environment variables to the bot container.
func (b *PodBuilder) WithEnv(envVars ...corev1.EnvVar) *PodBuilder {
	b.botEnv = append(b.botEnv, envVars...)
	return b
}

// WithEnvMap adds environment variables from a map.
func (b *PodBuilder) WithEnvMap(envMap map[string]string) *PodBuilder {
	for k, v := range envMap {
		b.botEnv = append(b.botEnv, corev1.EnvVar{Name: k, Value: v})
	}
	return b
}

// WithBotConfig sets the bot configuration environment variables.
func (b *PodBuilder) WithBotConfig(botID, codeFile, runtime, entrypoint string, config map[string]interface{}) *PodBuilder {
	// Validate required fields
	if botID == "" {
		b.errors = append(b.errors, fmt.Errorf("botID is required"))
	}
	if runtime == "" {
		b.errors = append(b.errors, fmt.Errorf("runtime is required"))
	}
	if entrypoint == "" {
		b.errors = append(b.errors, fmt.Errorf("entrypoint is required"))
	}
	if codeFile == "" {
		b.errors = append(b.errors, fmt.Errorf("codeFile is required"))
	}

	configJSON, err := json.Marshal(config)
	if err != nil {
		b.errors = append(b.errors, fmt.Errorf("failed to marshal config: %w", err))
		configJSON = []byte("{}")
	}

	b.botEnv = append(b.botEnv,
		corev1.EnvVar{Name: "BOT_ID", Value: botID},
		corev1.EnvVar{Name: "CODE_FILE", Value: codeFile},
		corev1.EnvVar{Name: "RUNTIME", Value: runtime},
		corev1.EnvVar{Name: "ENTRYPOINT", Value: entrypoint},
		corev1.EnvVar{Name: "BOT_CONFIG", Value: string(configJSON)},
		corev1.EnvVar{Name: "CODE_PATH", Value: "/bot"},
		corev1.EnvVar{Name: "STATE_PATH", Value: "/state"},
		corev1.EnvVar{Name: "LOGS_PATH", Value: "/var/the0/logs"},
		// STATE_DIR points to the SDK-managed state directory inside /state
		corev1.EnvVar{Name: "STATE_DIR", Value: "/state/.the0-state"},
	)
	return b
}

// WithBotType sets the bot type (realtime or scheduled).
// For scheduled bots, also sets IS_SCHEDULED=true to enable done file mechanism.
func (b *PodBuilder) WithBotType(botType string) *PodBuilder {
	b.botEnv = append(b.botEnv, corev1.EnvVar{Name: "BOT_TYPE", Value: botType})
	if botType == "scheduled" {
		b.botEnv = append(b.botEnv, corev1.EnvVar{Name: "IS_SCHEDULED", Value: "true"})
	}
	return b
}

// WithQueryEntrypoint sets the query entrypoint for realtime bots.
func (b *PodBuilder) WithQueryEntrypoint(queryEntrypoint string) *PodBuilder {
	if queryEntrypoint != "" {
		b.botEnv = append(b.botEnv, corev1.EnvVar{Name: "QUERY_ENTRYPOINT", Value: queryEntrypoint})
	}
	return b
}

// WithMinIOConfig adds MinIO credentials as environment variables.
func (b *PodBuilder) WithMinIOConfig(endpoint, accessKey, secretKey string, useSSL bool) *PodBuilder {
	b.botEnv = append(b.botEnv,
		corev1.EnvVar{Name: "MINIO_ENDPOINT", Value: endpoint},
		corev1.EnvVar{Name: "MINIO_ACCESS_KEY", Value: accessKey},
		corev1.EnvVar{Name: "MINIO_SECRET_KEY", Value: secretKey},
		corev1.EnvVar{Name: "MINIO_USE_SSL", Value: fmt.Sprintf("%t", useSSL)},
	)
	return b
}

// WithResources sets resource limits and requests for the bot container.
func (b *PodBuilder) WithResources(memoryLimit, cpuLimit, memoryRequest, cpuRequest string) *PodBuilder {
	b.botResources = corev1.ResourceRequirements{
		Limits: corev1.ResourceList{
			corev1.ResourceMemory: parseResourceOrDefault(memoryLimit, DefaultMemoryLimit),
			corev1.ResourceCPU:    parseResourceOrDefault(cpuLimit, DefaultCPULimit),
		},
		Requests: corev1.ResourceList{
			corev1.ResourceMemory: parseResourceOrDefault(memoryRequest, DefaultMemoryRequest),
			corev1.ResourceCPU:    parseResourceOrDefault(cpuRequest, DefaultCPURequest),
		},
	}
	return b
}

// SyncSidecarReadinessPort is the port used for sync sidecar readiness probes.
const SyncSidecarReadinessPort = 8079

// WithSyncSidecar adds the sync sidecar as a native K8s sidecar container (K8s 1.28+).
// Native sidecars are init containers with restartPolicy: Always that:
// - Start before the main container
// - Must pass startup probe before main container starts
// - Keep running alongside main container
func (b *PodBuilder) WithSyncSidecar(image string, botID string, watchDone bool) *PodBuilder {
	args := []string{
		"--bot-id", botID,
		"--state-path", "/state",
		"--logs-path", "/var/the0/logs",
		"--readiness-port", fmt.Sprintf("%d", SyncSidecarReadinessPort),
	}
	if watchDone {
		args = append(args, "--watch-done", "/var/the0/done")
	}

	// Native sidecar uses restartPolicy on the container (K8s 1.28+)
	restartPolicyAlways := corev1.ContainerRestartPolicyAlways

	b.syncSidecar = &corev1.Container{
		Name:            "sync",
		Image:           image,
		ImagePullPolicy: b.botImagePullPolicy,
		Command:         []string{"/app/runtime", "daemon", "sync"},
		Args:            args,
		Env:             b.minioEnvVars(),
		VolumeMounts: []corev1.VolumeMount{
			{Name: "bot-state", MountPath: "/state"},
			{Name: "the0", MountPath: "/var/the0"},
		},
		// Native sidecar: restartPolicy on container makes it run alongside main container
		RestartPolicy: &restartPolicyAlways,
		// Startup probe ensures main container waits for sync to be ready
		StartupProbe: &corev1.Probe{
			ProbeHandler: corev1.ProbeHandler{
				HTTPGet: &corev1.HTTPGetAction{
					Path: "/readyz",
					Port: intstr.FromInt(SyncSidecarReadinessPort),
				},
			},
			InitialDelaySeconds: 0,
			PeriodSeconds:       1,
			FailureThreshold:    30, // 30 seconds max to become ready
		},
		Resources: corev1.ResourceRequirements{
			Requests: corev1.ResourceList{
				corev1.ResourceMemory: resource.MustParse("32Mi"),
				corev1.ResourceCPU:    resource.MustParse("10m"),
			},
			Limits: corev1.ResourceList{
				corev1.ResourceMemory: resource.MustParse("64Mi"),
				corev1.ResourceCPU:    resource.MustParse("100m"),
			},
		},
	}
	return b
}

// WithQuerySidecar adds the query server sidecar container.
func (b *PodBuilder) WithQuerySidecar(image string) *PodBuilder {
	b.querySidecar = &corev1.Container{
		Name:            "query-server",
		Image:           image,
		ImagePullPolicy: b.botImagePullPolicy,
		Command:         []string{"/app/runtime", "execute", "--query-server-only"},
		Env:             b.botEnv, // Share env with bot container
		VolumeMounts: []corev1.VolumeMount{
			{Name: "bot-code", MountPath: "/bot"},
			{Name: "bot-state", MountPath: "/state"},
		},
		Ports: []corev1.ContainerPort{
			{Name: "query", ContainerPort: 9476, Protocol: corev1.ProtocolTCP},
		},
		Resources: corev1.ResourceRequirements{
			Requests: corev1.ResourceList{
				corev1.ResourceMemory: resource.MustParse("32Mi"),
				corev1.ResourceCPU:    resource.MustParse("10m"),
			},
			Limits: corev1.ResourceList{
				corev1.ResourceMemory: resource.MustParse("128Mi"),
				corev1.ResourceCPU:    resource.MustParse("200m"),
			},
		},
	}
	return b
}

// ForQuery configures the builder for ephemeral query execution.
// Sets query environment variables and removes sidecars.
func (b *PodBuilder) ForQuery(queryPath string, queryParams map[string]interface{}) *PodBuilder {
	b.restartPolicy = corev1.RestartPolicyNever
	b.syncSidecar = nil
	b.querySidecar = nil

	b.botEnv = append(b.botEnv, corev1.EnvVar{Name: "QUERY_PATH", Value: queryPath})

	if queryParams != nil && len(queryParams) > 0 {
		paramsJSON, err := json.Marshal(queryParams)
		if err != nil {
			b.errors = append(b.errors, fmt.Errorf("failed to marshal query params: %w", err))
		} else {
			b.botEnv = append(b.botEnv, corev1.EnvVar{Name: "QUERY_PARAMS", Value: string(paramsJSON)})
		}
	}

	return b
}

// minioEnvVars extracts MinIO env vars from botEnv for sidecars.
func (b *PodBuilder) minioEnvVars() []corev1.EnvVar {
	var minioEnv []corev1.EnvVar
	for _, env := range b.botEnv {
		switch env.Name {
		case "MINIO_ENDPOINT", "MINIO_ACCESS_KEY", "MINIO_SECRET_KEY", "MINIO_USE_SSL":
			minioEnv = append(minioEnv, env)
		}
	}
	return minioEnv
}

// Build creates the final Pod spec.
// Returns an error if validation fails.
func (b *PodBuilder) Build() (*corev1.Pod, error) {
	// Check for accumulated errors
	if len(b.errors) > 0 {
		return nil, fmt.Errorf("pod builder validation failed: %v", b.errors[0])
	}

	// Validate required fields
	if b.botImage == "" {
		return nil, fmt.Errorf("image is required")
	}

	// Build containers list
	containers := []corev1.Container{
		{
			Name:            "bot",
			Image:           b.botImage,
			ImagePullPolicy: b.botImagePullPolicy,
			Command:         b.botCommand,
			WorkingDir:      "/bot",
			Env:             b.botEnv,
			VolumeMounts:    b.volumeMounts,
			Resources:       b.botResources,
		},
	}

	// Native sidecars (K8s 1.28+) go in initContainers with restartPolicy: Always
	// This ensures they start before the main container and keep running
	var initContainers []corev1.Container
	if b.syncSidecar != nil {
		initContainers = append(initContainers, *b.syncSidecar)
	}

	// Query sidecar runs as a regular container (not native sidecar)
	if b.querySidecar != nil {
		containers = append(containers, *b.querySidecar)
	}

	return &corev1.Pod{
		ObjectMeta: metav1.ObjectMeta{
			Name:        b.name,
			Namespace:   b.namespace,
			Labels:      b.labels,
			Annotations: b.annotations,
		},
		Spec: corev1.PodSpec{
			RestartPolicy:  b.restartPolicy,
			InitContainers: initContainers,
			Containers:     containers,
			Volumes:        b.volumes,
		},
	}, nil
}

// BuildSpec returns just the PodSpec (for CronJobs).
func (b *PodBuilder) BuildSpec() (*corev1.PodSpec, error) {
	pod, err := b.Build()
	if err != nil {
		return nil, err
	}
	return &pod.Spec, nil
}
