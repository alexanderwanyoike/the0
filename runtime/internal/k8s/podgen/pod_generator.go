// Package podgen provides Pod specification generation for Kubernetes-native
// bot execution. It converts Bot models from MongoDB into Kubernetes Pod specs.
package podgen

import (
	"crypto/sha256"
	"encoding/base64"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"strings"

	corev1 "k8s.io/api/core/v1"
	"k8s.io/apimachinery/pkg/api/resource"
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"

	"runtime/internal/model"
	"runtime/internal/entrypoints"
	runtimepkg "runtime/internal/runtime"
)

const (
	// LabelBotID is the label key for the bot instance ID.
	LabelBotID = "the0.dev/bot-id"

	// LabelCustomBotID is the label key for the custom bot type ID.
	LabelCustomBotID = "the0.dev/custom-bot-id"

	// LabelCustomBotVersion is the label key for the custom bot version.
	LabelCustomBotVersion = "the0.dev/custom-bot-version"

	// LabelRuntime is the label key for the bot runtime (python3.11, nodejs20, etc).
	LabelRuntime = "the0.dev/runtime"

	// LabelManagedBy indicates the controller managing this pod.
	LabelManagedBy = "the0.dev/managed-by"

	// AnnotationConfigHash stores a hash of the bot config for change detection.
	AnnotationConfigHash = "the0.dev/config-hash"
)

// Resource defaults are defined in runtime/internal/runtime/resources.go
// Re-exported here for backward compatibility
const (
	DefaultMemoryLimit   = runtimepkg.DefaultMemoryLimit
	DefaultCPULimit      = runtimepkg.DefaultCPULimit
	DefaultMemoryRequest = runtimepkg.DefaultMemoryRequest
	DefaultCPURequest    = runtimepkg.DefaultCPURequest
)

// PodGeneratorConfig holds configuration for pod generation.
type PodGeneratorConfig struct {
	// Namespace is the Kubernetes namespace for bot pods.
	Namespace string

	// ControllerName identifies this controller for the managed-by label.
	ControllerName string

	// MinIO configuration for downloading bot code
	MinIOEndpoint   string
	MinIOAccessKey  string
	MinIOSecretKey  string
	MinIOBucket     string
	MinIOUseSSL     bool
}

// PodGenerator generates Kubernetes Pod specs from Bot models.
type PodGenerator struct {
	config PodGeneratorConfig
}

// NewPodGenerator creates a new PodGenerator with the given configuration.
func NewPodGenerator(config PodGeneratorConfig) *PodGenerator {
	if config.ControllerName == "" {
		config.ControllerName = "the0-bot-controller"
	}
	if config.Namespace == "" {
		config.Namespace = "the0"
	}
	return &PodGenerator{config: config}
}

// GeneratePod creates a Kubernetes Pod spec for the given bot.
// Uses base images with an init container to download code from MinIO.
func (g *PodGenerator) GeneratePod(bot model.Bot) (*corev1.Pod, error) {
	// Marshal config to JSON for BOT_CONFIG env var
	configJSON, err := json.Marshal(bot.Config)
	if err != nil {
		return nil, fmt.Errorf("failed to marshal bot config: %w", err)
	}

	// Compute config hash for change detection
	configHash := computeConfigHash(bot)

	// Get resource limits from config or use defaults
	memoryLimit, cpuLimit := g.getResourceLimits(bot)
	memoryRequest, cpuRequest := g.getResourceRequests(bot)

	// Get custom bot ID from file path or generate one
	customBotID := extractCustomBotID(bot)

	// Safely extract version and runtime with defaults
	version := bot.CustomBotVersion.Version
	runtime := bot.CustomBotVersion.Config.Runtime
	filePath := bot.CustomBotVersion.FilePath
	entrypoint := g.getEntrypoint(bot)

	// Validate filePath and bucket to prevent command injection
	if err := validateMinIOPath(filePath); err != nil {
		return nil, fmt.Errorf("invalid file path: %w", err)
	}
	if err := validateMinIOPath(g.config.MinIOBucket); err != nil {
		return nil, fmt.Errorf("invalid bucket name: %w", err)
	}

	// Get base image for the runtime (using shared runtime package)
	baseImage, err := runtimepkg.GetDockerImage(runtime)
	if err != nil {
		return nil, fmt.Errorf("failed to get Docker image: %w", err)
	}

	// Build MinIO endpoint URL
	minioURL := g.getMinIOURL()

	// Generate entrypoint script using shared entrypoints package
	entrypointScript, err := entrypoints.GenerateK8sEntrypoint(runtime, entrypoints.GeneratorOptions{
		EntryPointType: "bot",
		Entrypoint:     entrypoint,
		WorkDir:        "/bot",
	})
	if err != nil {
		return nil, fmt.Errorf("failed to generate entrypoint script: %w", err)
	}

	pod := &corev1.Pod{
		ObjectMeta: metav1.ObjectMeta{
			Name:      fmt.Sprintf("bot-%s", bot.ID),
			Namespace: g.config.Namespace,
			Labels: map[string]string{
				LabelBotID:            bot.ID,
				LabelCustomBotID:      customBotID,
				LabelCustomBotVersion: version,
				LabelRuntime:          runtime,
				LabelManagedBy:        g.config.ControllerName,
			},
			Annotations: map[string]string{
				AnnotationConfigHash: configHash,
			},
		},
		Spec: corev1.PodSpec{
			RestartPolicy: corev1.RestartPolicyNever,
			InitContainers: []corev1.Container{
				{
					Name:    "download-code",
					Image:   "minio/mc:RELEASE.2024-11-17T19-35-25Z",
					Command: []string{"/bin/sh", "-c"},
					Args: []string{
						fmt.Sprintf(`
set -e
mc alias set minio %s $MINIO_ACCESS_KEY $MINIO_SECRET_KEY
mc cp minio/%s/%s /bot/code.zip
ls -la /bot/
`, minioURL, g.config.MinIOBucket, filePath),
					},
					Env: []corev1.EnvVar{
						{Name: "MINIO_ACCESS_KEY", Value: g.config.MinIOAccessKey},
						{Name: "MINIO_SECRET_KEY", Value: g.config.MinIOSecretKey},
					},
					VolumeMounts: []corev1.VolumeMount{
						{Name: "bot-code", MountPath: "/bot"},
					},
				},
				{
					Name:    "extract-code",
					Image:   "busybox:1.36.1",
					Command: []string{"/bin/sh", "-c"},
					Args: []string{
						fmt.Sprintf(`
set -e
cd /bot
unzip -o code.zip
rm code.zip
echo '%s' | base64 -d > /bot/entrypoint.sh
chmod +x /bot/entrypoint.sh
ls -la /bot/
`, base64.StdEncoding.EncodeToString([]byte(entrypointScript))),
					},
					VolumeMounts: []corev1.VolumeMount{
						{Name: "bot-code", MountPath: "/bot"},
					},
				},
			},
			Containers: []corev1.Container{
				{
					Name:       "bot",
					Image:      baseImage,
					Command:    []string{"/bin/bash", "/bot/entrypoint.sh"},
					WorkingDir: "/bot",
					Env: []corev1.EnvVar{
						{Name: "BOT_ID", Value: bot.ID},
						{Name: "BOT_CONFIG", Value: string(configJSON)},
						{Name: "CODE_MOUNT_DIR", Value: "/bot"},
					},
					VolumeMounts: []corev1.VolumeMount{
						{Name: "bot-code", MountPath: "/bot"},
					},
					Resources: corev1.ResourceRequirements{
						Limits: corev1.ResourceList{
							corev1.ResourceMemory: parseResourceOrDefault(memoryLimit, DefaultMemoryLimit),
							corev1.ResourceCPU:    parseResourceOrDefault(cpuLimit, DefaultCPULimit),
						},
						Requests: corev1.ResourceList{
							corev1.ResourceMemory: parseResourceOrDefault(memoryRequest, DefaultMemoryRequest),
							corev1.ResourceCPU:    parseResourceOrDefault(cpuRequest, DefaultCPURequest),
						},
					},
				},
			},
			Volumes: []corev1.Volume{
				{
					Name: "bot-code",
					VolumeSource: corev1.VolumeSource{
						EmptyDir: &corev1.EmptyDirVolumeSource{},
					},
				},
			},
		},
	}

	return pod, nil
}

// GeneratePodName generates the pod name for a given bot ID.
func GeneratePodName(botID string) string {
	return fmt.Sprintf("bot-%s", botID)
}

// ExtractBotID extracts the bot ID from a pod's labels.
func ExtractBotID(pod *corev1.Pod) string {
	if pod.Labels == nil {
		return ""
	}
	return pod.Labels[LabelBotID]
}

// ExtractConfigHash extracts the config hash from a pod's annotations.
func ExtractConfigHash(pod *corev1.Pod) string {
	if pod.Annotations == nil {
		return ""
	}
	return pod.Annotations[AnnotationConfigHash]
}

// ConfigChanged returns true if the bot config has changed since the pod was created.
func ConfigChanged(pod *corev1.Pod, bot model.Bot) bool {
	existingHash := ExtractConfigHash(pod)
	currentHash := computeConfigHash(bot)
	return existingHash != currentHash
}

// getResourceLimits extracts resource limits from bot config or returns defaults.
func (g *PodGenerator) getResourceLimits(bot model.Bot) (memory, cpu string) {
	memory = DefaultMemoryLimit
	cpu = DefaultCPULimit

	if bot.Config == nil {
		return
	}

	if m, ok := bot.Config["memory_limit"].(string); ok && m != "" {
		memory = m
	}
	if c, ok := bot.Config["cpu_limit"].(string); ok && c != "" {
		cpu = c
	}

	return
}

// getResourceRequests extracts resource requests from bot config or returns defaults.
func (g *PodGenerator) getResourceRequests(bot model.Bot) (memory, cpu string) {
	memory = DefaultMemoryRequest
	cpu = DefaultCPURequest

	if bot.Config == nil {
		return
	}

	if m, ok := bot.Config["memory_request"].(string); ok && m != "" {
		memory = m
	}
	if c, ok := bot.Config["cpu_request"].(string); ok && c != "" {
		cpu = c
	}

	return
}

// parseResourceOrDefault parses a resource quantity string, returning a default if invalid.
// This prevents panics from invalid user input.
func parseResourceOrDefault(value, defaultValue string) resource.Quantity {
	q, err := resource.ParseQuantity(value)
	if err != nil {
		return resource.MustParse(defaultValue)
	}
	return q
}

// computeConfigHash creates a hash of the bot config for change detection.
// Uses SHA256 hash of the JSON-serialized config data.
func computeConfigHash(bot model.Bot) string {
	data := struct {
		Config           map[string]interface{}
		CustomBotVersion string
		Enabled          *bool
	}{
		Config:           bot.Config,
		CustomBotVersion: bot.CustomBotVersion.Version,
		Enabled:          bot.Enabled,
	}

	jsonBytes, err := json.Marshal(data)
	if err != nil {
		// Return a deterministic fallback hash based on bot ID to avoid false positives
		return fmt.Sprintf("err-%s", bot.ID[:min(8, len(bot.ID))])
	}

	// Use SHA256 for proper content-based hash
	hash := sha256.Sum256(jsonBytes)
	// Take first 16 chars of hex-encoded hash (64 bits - good enough for change detection)
	return hex.EncodeToString(hash[:])[:16]
}

// extractCustomBotID extracts a custom bot ID from the bot's file path or config.
func extractCustomBotID(bot model.Bot) string {
	// Try to extract from custom bot config name
	if bot.CustomBotVersion.Config.Name != "" {
		return sanitizeLabelValue(bot.CustomBotVersion.Config.Name)
	}

	// Fallback to using bot ID prefix
	if len(bot.ID) > 8 {
		return bot.ID[:8]
	}
	return bot.ID
}

// sanitizeLabelValue ensures a string is valid as a Kubernetes label value.
// Label values must be 63 characters or less, begin and end with alphanumeric,
// and contain only alphanumerics, dashes, underscores, and dots.
func sanitizeLabelValue(s string) string {
	result := make([]byte, 0, len(s))
	for _, c := range s {
		if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') {
			result = append(result, byte(c))
		} else if c == '-' || c == '_' || c == '.' {
			result = append(result, byte(c))
		}
	}

	// Truncate to 63 characters
	if len(result) > 63 {
		result = result[:63]
	}

	// Trim leading non-alphanumeric characters
	for len(result) > 0 && !isAlphanumeric(result[0]) {
		result = result[1:]
	}

	// Trim trailing non-alphanumeric characters
	for len(result) > 0 && !isAlphanumeric(result[len(result)-1]) {
		result = result[:len(result)-1]
	}

	// Ensure not empty
	if len(result) == 0 {
		return "bot"
	}

	return string(result)
}

// isAlphanumeric returns true if the byte is a letter or digit.
func isAlphanumeric(b byte) bool {
	return (b >= 'a' && b <= 'z') || (b >= 'A' && b <= 'Z') || (b >= '0' && b <= '9')
}

// getMinIOURL builds the MinIO endpoint URL with protocol.
func (g *PodGenerator) getMinIOURL() string {
	endpoint := g.config.MinIOEndpoint
	if strings.HasPrefix(endpoint, "http://") || strings.HasPrefix(endpoint, "https://") {
		return endpoint
	}
	if g.config.MinIOUseSSL {
		return "https://" + endpoint
	}
	return "http://" + endpoint
}

// getEntrypoint extracts the bot entrypoint from the config.
func (g *PodGenerator) getEntrypoint(bot model.Bot) string {
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

// validateMinIOPath ensures a MinIO path doesn't contain shell metacharacters.
// Returns an error if the path contains potentially dangerous characters.
func validateMinIOPath(path string) error {
	// Allow only safe characters in MinIO paths
	for _, c := range path {
		if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') {
			continue
		}
		switch c {
		case '/', '-', '_', '.':
			continue
		default:
			return fmt.Errorf("invalid character '%c' in MinIO path", c)
		}
	}
	return nil
}
