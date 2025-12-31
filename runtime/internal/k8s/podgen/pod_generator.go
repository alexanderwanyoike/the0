// Package podgen provides Pod specification generation for Kubernetes-native
// bot execution. It converts Bot models from MongoDB into Kubernetes Pod specs.
package podgen

import (
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"fmt"

	corev1 "k8s.io/api/core/v1"
	"k8s.io/apimachinery/pkg/api/resource"
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"

	"runtime/internal/bot-runner/model"
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

	// DefaultMemoryLimit is the default memory limit for bot pods.
	DefaultMemoryLimit = "512Mi"

	// DefaultCPULimit is the default CPU limit for bot pods.
	DefaultCPULimit = "500m"

	// DefaultMemoryRequest is the default memory request for bot pods.
	DefaultMemoryRequest = "128Mi"

	// DefaultCPURequest is the default CPU request for bot pods.
	DefaultCPURequest = "100m"
)

// PodGeneratorConfig holds configuration for pod generation.
type PodGeneratorConfig struct {
	// Namespace is the Kubernetes namespace for bot pods.
	Namespace string

	// ControllerName identifies this controller for the managed-by label.
	ControllerName string
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
// The imageRef should be the full container image reference (e.g., "registry:5000/the0/bots/my-bot:1.0.0").
func (g *PodGenerator) GeneratePod(bot model.Bot, imageRef string) (*corev1.Pod, error) {
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

	pod := &corev1.Pod{
		ObjectMeta: metav1.ObjectMeta{
			Name:      fmt.Sprintf("bot-%s", bot.ID),
			Namespace: g.config.Namespace,
			Labels: map[string]string{
				LabelBotID:            bot.ID,
				LabelCustomBotID:      customBotID,
				LabelCustomBotVersion: bot.CustomBotVersion.Version,
				LabelRuntime:          bot.CustomBotVersion.Config.Runtime,
				LabelManagedBy:        g.config.ControllerName,
			},
			Annotations: map[string]string{
				AnnotationConfigHash: configHash,
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
						{
							Name:  "BOT_ID",
							Value: bot.ID,
						},
						{
							Name:  "BOT_CONFIG",
							Value: string(configJSON),
						},
						{
							Name:  "CODE_MOUNT_DIR",
							Value: "/bot",
						},
					},
					Resources: corev1.ResourceRequirements{
						Limits: corev1.ResourceList{
							corev1.ResourceMemory: resource.MustParse(memoryLimit),
							corev1.ResourceCPU:    resource.MustParse(cpuLimit),
						},
						Requests: corev1.ResourceList{
							corev1.ResourceMemory: resource.MustParse(memoryRequest),
							corev1.ResourceCPU:    resource.MustParse(cpuRequest),
						},
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
		return ""
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
	for i, c := range s {
		if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') {
			result = append(result, byte(c))
		} else if c == '-' || c == '_' || c == '.' {
			// Only add if not at start or end
			if i > 0 && i < len(s)-1 {
				result = append(result, byte(c))
			}
		}
	}

	// Truncate to 63 characters
	if len(result) > 63 {
		result = result[:63]
	}

	// Ensure not empty
	if len(result) == 0 {
		return "bot"
	}

	return string(result)
}
