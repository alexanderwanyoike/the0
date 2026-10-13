// Package podgen provides Pod specification generation for Kubernetes-native bot execution.
package podgen

import (
	"fmt"

	corev1 "k8s.io/api/core/v1"
	"k8s.io/apimachinery/pkg/api/resource"
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"
	"k8s.io/apimachinery/pkg/util/intstr"

	"runtime/internal/launch"
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
	botEnv             []corev1.EnvVar
	botResources       corev1.ResourceRequirements
	launch             launch.Settings

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
		name:               name,
		namespace:          namespace,
		labels:             make(map[string]string),
		annotations:        make(map[string]string),
		restartPolicy:      corev1.RestartPolicyAlways,
		botImagePullPolicy: corev1.PullIfNotPresent,
		volumes: []corev1.Volume{
			{Name: "bot-code", VolumeSource: corev1.VolumeSource{EmptyDir: &corev1.EmptyDirVolumeSource{}}},
			{Name: "bot-state", VolumeSource: corev1.VolumeSource{EmptyDir: &corev1.EmptyDirVolumeSource{}}},
			{Name: "the0", VolumeSource: corev1.VolumeSource{EmptyDir: &corev1.EmptyDirVolumeSource{}}},
			{Name: "tmp", VolumeSource: corev1.VolumeSource{EmptyDir: &corev1.EmptyDirVolumeSource{}}},
			{Name: "query", VolumeSource: corev1.VolumeSource{EmptyDir: &corev1.EmptyDirVolumeSource{}}},
		},
		volumeMounts: []corev1.VolumeMount{
			{Name: "bot-code", MountPath: "/bot"},
			{Name: "bot-state", MountPath: "/state"},
			{Name: "the0", MountPath: "/var/the0"},
			{Name: "tmp", MountPath: "/tmp"},
			{Name: "query", MountPath: "/query"},
		},
		// Python block-buffers stdout (8KB) when it's a pipe, so metric()
		// print() output would sit unflushed instead of reaching the log
		// pipeline. Set for all runtimes; harmless outside Python.
		botEnv: []corev1.EnvVar{{Name: "PYTHONUNBUFFERED", Value: "1"}},
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
		errors: []error{},
	}
}

func hardenedContainerSecurityContext() *corev1.SecurityContext {
	runAsUser := int64(1000)
	runAsGroup := int64(1000)
	readOnlyRootFilesystem := true
	allowPrivilegeEscalation := false

	return &corev1.SecurityContext{
		RunAsNonRoot:             boolPtr(true),
		RunAsUser:                &runAsUser,
		RunAsGroup:               &runAsGroup,
		ReadOnlyRootFilesystem:   &readOnlyRootFilesystem,
		AllowPrivilegeEscalation: &allowPrivilegeEscalation,
		Capabilities: &corev1.Capabilities{
			Drop: []corev1.Capability{"ALL"},
		},
	}
}

func hardenedPodSecurityContext() *corev1.PodSecurityContext {
	runAsUser := int64(1000)
	runAsGroup := int64(1000)
	fsGroup := int64(1000)

	return &corev1.PodSecurityContext{
		RunAsNonRoot: boolPtr(true),
		RunAsUser:    &runAsUser,
		RunAsGroup:   &runAsGroup,
		FSGroup:      &fsGroup,
		SeccompProfile: &corev1.SeccompProfile{
			Type: corev1.SeccompProfileTypeRuntimeDefault,
		},
	}
}

func boolPtr(value bool) *bool {
	return &value
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

// WithLaunch makes the bot container start the launch. The sync sidecar
// gets the part of it that sync reads, and the query sidecar serves the
// launch's queries.
func (b *PodBuilder) WithLaunch(settings launch.Settings) *PodBuilder {
	for _, required := range []struct{ name, value string }{
		{"botID", settings.BotID},
		{"runtime", settings.Runtime},
		{"entrypoint", settings.Entrypoint},
		{"codeFile", settings.CodeFile},
	} {
		if required.value == "" {
			b.errors = append(b.errors, fmt.Errorf("%s is required", required.name))
		}
	}
	b.launch = settings
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
func (b *PodBuilder) WithSyncSidecar(image string, botID string) *PodBuilder {
	args := []string{
		"--bot-id", botID,
		"--state-path", "/state",
		"--logs-path", "/var/the0/logs",
		"--readiness-port", fmt.Sprintf("%d", SyncSidecarReadinessPort),
	}

	// Native sidecar uses restartPolicy on the container (K8s 1.28+)
	restartPolicyAlways := corev1.ContainerRestartPolicyAlways

	b.syncSidecar = &corev1.Container{
		Name:            "sync",
		Image:           image,
		ImagePullPolicy: b.botImagePullPolicy,
		Command:         []string{launch.Binary, "daemon", "sync"},
		Args:            args,
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
				corev1.ResourceMemory: resource.MustParse("64Mi"),
				corev1.ResourceCPU:    resource.MustParse("10m"),
			},
			Limits: corev1.ResourceList{
				corev1.ResourceMemory: resource.MustParse("128Mi"),
				corev1.ResourceCPU:    resource.MustParse("100m"),
			},
		},
		SecurityContext: hardenedContainerSecurityContext(),
	}
	return b
}

// WithQuerySidecar adds the query server sidecar container.
func (b *PodBuilder) WithQuerySidecar(image string) *PodBuilder {
	b.querySidecar = &corev1.Container{
		Name:            "query-server",
		Image:           image,
		ImagePullPolicy: b.botImagePullPolicy,
		VolumeMounts:    b.volumeMounts,
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
		SecurityContext: hardenedContainerSecurityContext(),
	}
	return b
}

// containerEnv is the bot container's own environment followed by env.
func (b *PodBuilder) containerEnv(env []launch.Var) []corev1.EnvVar {
	return append(append([]corev1.EnvVar{}, b.botEnv...), envVars(env)...)
}

func envVars(env []launch.Var) []corev1.EnvVar {
	vars := make([]corev1.EnvVar, 0, len(env))
	for _, variable := range env {
		vars = append(vars, corev1.EnvVar{Name: variable.Name, Value: variable.Value})
	}
	return vars
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
			Command:         b.launch.Command(),
			WorkingDir:      "/bot",
			Env:             b.containerEnv(b.launch.Env()),
			VolumeMounts:    b.volumeMounts,
			Resources:       b.botResources,
			SecurityContext: hardenedContainerSecurityContext(),
		},
	}

	// Native sidecars (K8s 1.28+) go in initContainers with restartPolicy: Always
	// This ensures they start before the main container and keep running
	var initContainers []corev1.Container
	if b.syncSidecar != nil {
		syncSidecar := *b.syncSidecar
		syncSidecar.Env = envVars(b.launch.SyncEnv())
		initContainers = append(initContainers, syncSidecar)
	}

	// Query sidecar runs as a regular container (not native sidecar)
	if b.querySidecar != nil {
		server := b.launch
		server.Mode = launch.Server
		server.SkipSync = false
		server.SkipQueryServer = false
		querySidecar := *b.querySidecar
		querySidecar.Command = server.Command()
		querySidecar.Env = b.containerEnv(server.Env())
		containers = append(containers, querySidecar)
	}

	return &corev1.Pod{
		ObjectMeta: metav1.ObjectMeta{
			Name:        b.name,
			Namespace:   b.namespace,
			Labels:      b.labels,
			Annotations: b.annotations,
		},
		Spec: corev1.PodSpec{
			SecurityContext: hardenedPodSecurityContext(),
			RestartPolicy:   b.restartPolicy,
			InitContainers:  initContainers,
			Containers:      containers,
			Volumes:         b.volumes,
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
