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

	"runtime/internal/model"
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
	MinIOEndpoint    string
	MinIOAccessKey   string
	MinIOSecretKey   string
	MinIOBucket      string
	MinIOStateBucket string // Bucket for persistent bot state (default: "bot-state")
	MinIOUseSSL      bool

	// RuntimeImage is the image for init and sidecar containers (e.g., "the0/runtime:latest")
	RuntimeImage string

	// RuntimeImagePullPolicy for init and sidecar containers (default: IfNotPresent)
	RuntimeImagePullPolicy corev1.PullPolicy
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
	if config.MinIOStateBucket == "" {
		config.MinIOStateBucket = "bot-state"
	}
	if config.RuntimeImage == "" {
		config.RuntimeImage = "runtime:latest"
	}
	if config.RuntimeImagePullPolicy == "" {
		config.RuntimeImagePullPolicy = corev1.PullIfNotPresent
	}
	return &PodGenerator{config: config}
}

// GeneratePod creates a Kubernetes Pod spec for a realtime bot.
// Realtime bots run continuously with sync sidecar and optional query server sidecar.
func (g *PodGenerator) GeneratePod(bot model.Bot) (*corev1.Pod, error) {
	// Extract and validate required configuration
	botConfig, err := g.extractBotConfig(bot)
	if err != nil {
		return nil, err
	}

	// Get the runtime image
	image, err := runtimepkg.GetDockerImage(botConfig.runtime)
	if err != nil {
		return nil, fmt.Errorf("failed to get Docker image: %w", err)
	}

	// Build the pod using the fluent builder
	builder := NewPodBuilder(fmt.Sprintf("bot-%s", bot.ID), g.config.Namespace).
		WithImage(image).
		WithImagePullPolicy(g.config.RuntimeImagePullPolicy).
		WithLabels(map[string]string{
			LabelBotID:            bot.ID,
			LabelCustomBotID:      botConfig.customBotID,
			LabelCustomBotVersion: botConfig.version,
			LabelRuntime:          botConfig.runtime,
			LabelManagedBy:        g.config.ControllerName,
		}).
		WithAnnotations(map[string]string{
			AnnotationConfigHash: computeConfigHash(bot),
		}).
		WithBotConfig(bot.ID, botConfig.filePath, botConfig.runtime, botConfig.entrypoint, bot.Config).
		WithBotType("realtime").
		WithMinIOConfig(g.config.MinIOEndpoint, g.config.MinIOAccessKey, g.config.MinIOSecretKey, g.config.MinIOUseSSL).
		WithResources(botConfig.memoryLimit, botConfig.cpuLimit, botConfig.memoryRequest, botConfig.cpuRequest).
		WithSyncSidecar(g.config.RuntimeImage, bot.ID, false)

	// Add query entrypoint if configured
	if botConfig.queryEntrypoint != "" {
		builder.WithQueryEntrypoint(botConfig.queryEntrypoint).
			WithQuerySidecar(image)
	}

	return builder.Build()
}

// GenerateScheduledPodSpec creates a Pod spec for scheduled bots (CronJobs).
// Scheduled bots run once per trigger with sync sidecar that watches for completion.
// No query server sidecar - queries are executed ephemerally.
func (g *PodGenerator) GenerateScheduledPodSpec(bot model.Bot) (*corev1.PodSpec, error) {
	// Extract and validate required configuration
	botConfig, err := g.extractBotConfig(bot)
	if err != nil {
		return nil, err
	}

	// Get the runtime image
	image, err := runtimepkg.GetDockerImage(botConfig.runtime)
	if err != nil {
		return nil, fmt.Errorf("failed to get Docker image: %w", err)
	}

	// Build the pod for scheduled execution
	builder := NewPodBuilder(fmt.Sprintf("bot-%s", bot.ID), g.config.Namespace).
		WithImage(image).
		WithImagePullPolicy(g.config.RuntimeImagePullPolicy).
		WithRestartPolicy(corev1.RestartPolicyNever). // Scheduled bots don't restart
		WithLabels(map[string]string{
			LabelBotID:            bot.ID,
			LabelCustomBotID:      botConfig.customBotID,
			LabelCustomBotVersion: botConfig.version,
			LabelRuntime:          botConfig.runtime,
			LabelManagedBy:        g.config.ControllerName,
		}).
		WithAnnotations(map[string]string{
			AnnotationConfigHash: computeConfigHash(bot),
		}).
		WithBotConfig(bot.ID, botConfig.filePath, botConfig.runtime, botConfig.entrypoint, bot.Config).
		WithBotType("scheduled").
		WithMinIOConfig(g.config.MinIOEndpoint, g.config.MinIOAccessKey, g.config.MinIOSecretKey, g.config.MinIOUseSSL).
		WithResources(botConfig.memoryLimit, botConfig.cpuLimit, botConfig.memoryRequest, botConfig.cpuRequest).
		WithSyncSidecar(g.config.RuntimeImage, bot.ID, true) // watchDone=true for scheduled

	// Note: No query sidecar for scheduled bots - queries are ephemeral

	return builder.BuildSpec()
}

// GenerateQueryPod creates a Pod spec for executing a query against a bot.
// Query pods run once without sidecars.
func (g *PodGenerator) GenerateQueryPod(bot model.Bot, queryPath string, queryParams map[string]interface{}) (*corev1.Pod, error) {
	// Extract and validate required configuration
	botConfig, err := g.extractBotConfig(bot)
	if err != nil {
		return nil, err
	}

	// Get the runtime image
	image, err := runtimepkg.GetDockerImage(botConfig.runtime)
	if err != nil {
		return nil, fmt.Errorf("failed to get Docker image: %w", err)
	}

	// Use query entrypoint if available, otherwise fall back to bot entrypoint
	entrypoint := botConfig.entrypoint
	if botConfig.queryEntrypoint != "" {
		entrypoint = botConfig.queryEntrypoint
	}

	// Build the pod for query execution
	builder := NewPodBuilder(fmt.Sprintf("query-%s", bot.ID), g.config.Namespace).
		WithImage(image).
		WithImagePullPolicy(g.config.RuntimeImagePullPolicy).
		WithLabels(map[string]string{
			LabelBotID:            bot.ID,
			LabelCustomBotID:      botConfig.customBotID,
			LabelCustomBotVersion: botConfig.version,
			LabelRuntime:          botConfig.runtime,
			LabelManagedBy:        g.config.ControllerName,
		}).
		WithBotConfig(bot.ID, botConfig.filePath, botConfig.runtime, entrypoint, bot.Config).
		WithMinIOConfig(g.config.MinIOEndpoint, g.config.MinIOAccessKey, g.config.MinIOSecretKey, g.config.MinIOUseSSL).
		WithResources(botConfig.memoryLimit, botConfig.cpuLimit, botConfig.memoryRequest, botConfig.cpuRequest).
		ForQuery(queryPath, queryParams)

	return builder.Build()
}

// botConfig holds extracted and validated bot configuration.
type botConfig struct {
	version         string
	runtime         string
	filePath        string
	entrypoint      string
	queryEntrypoint string
	customBotID     string
	memoryLimit     string
	cpuLimit        string
	memoryRequest   string
	cpuRequest      string
}

// extractBotConfig extracts and validates configuration from a Bot model.
func (g *PodGenerator) extractBotConfig(bot model.Bot) (*botConfig, error) {
	cfg := &botConfig{
		version:  bot.CustomBotVersion.Version,
		runtime:  bot.CustomBotVersion.Config.Runtime,
		filePath: bot.CustomBotVersion.FilePath,
	}

	// Extract entrypoint
	if bot.CustomBotVersion.Config.Entrypoints != nil {
		cfg.entrypoint = bot.CustomBotVersion.Config.Entrypoints["bot"]
		cfg.queryEntrypoint = bot.CustomBotVersion.Config.Entrypoints["query"]
	}

	// Validate required fields
	if cfg.runtime == "" {
		return nil, fmt.Errorf("bot %s: runtime is required", bot.ID)
	}
	if cfg.entrypoint == "" {
		return nil, fmt.Errorf("bot %s: entrypoint is required (missing entrypoints.bot in config)", bot.ID)
	}
	if cfg.filePath == "" {
		return nil, fmt.Errorf("bot %s: file path is required", bot.ID)
	}

	// Validate paths for command injection
	if err := validateMinIOPath(cfg.filePath); err != nil {
		return nil, fmt.Errorf("bot %s: invalid file path: %w", bot.ID, err)
	}
	if err := validateMinIOPath(bot.ID); err != nil {
		return nil, fmt.Errorf("bot %s: invalid bot ID: %w", bot.ID, err)
	}

	// Extract custom bot ID
	cfg.customBotID = extractCustomBotID(bot)

	// Extract resource limits with defaults
	cfg.memoryLimit, cfg.cpuLimit = g.getResourceLimits(bot)
	cfg.memoryRequest, cfg.cpuRequest = g.getResourceRequests(bot)

	return cfg, nil
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
func parseResourceOrDefault(value, defaultValue string) resource.Quantity {
	q, err := resource.ParseQuantity(value)
	if err != nil {
		return resource.MustParse(defaultValue)
	}
	return q
}

// computeConfigHash creates a hash of the bot config for change detection.
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
		return fmt.Sprintf("err-%s", bot.ID[:min(8, len(bot.ID))])
	}

	hash := sha256.Sum256(jsonBytes)
	return hex.EncodeToString(hash[:])[:16]
}

// extractCustomBotID extracts a custom bot ID from the bot's config or ID.
func extractCustomBotID(bot model.Bot) string {
	if bot.CustomBotVersion.Config.Name != "" {
		return sanitizeLabelValue(bot.CustomBotVersion.Config.Name)
	}
	if len(bot.ID) > 8 {
		return bot.ID[:8]
	}
	return bot.ID
}

// sanitizeLabelValue ensures a string is valid as a Kubernetes label value.
func sanitizeLabelValue(s string) string {
	result := make([]byte, 0, len(s))
	for _, c := range s {
		if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') {
			result = append(result, byte(c))
		} else if c == '-' || c == '_' || c == '.' {
			result = append(result, byte(c))
		}
	}

	if len(result) > 63 {
		result = result[:63]
	}

	for len(result) > 0 && !isAlphanumeric(result[0]) {
		result = result[1:]
	}
	for len(result) > 0 && !isAlphanumeric(result[len(result)-1]) {
		result = result[:len(result)-1]
	}

	if len(result) == 0 {
		return "bot"
	}

	return string(result)
}

func isAlphanumeric(b byte) bool {
	return (b >= 'a' && b <= 'z') || (b >= 'A' && b <= 'Z') || (b >= '0' && b <= '9')
}

// validateMinIOPath ensures a MinIO path doesn't contain shell metacharacters.
func validateMinIOPath(path string) error {
	for _, c := range path {
		if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') {
			continue
		}
		switch c {
		case '/', '-', '_', '.':
			continue
		default:
			return fmt.Errorf("invalid character '%c' in path", c)
		}
	}
	return nil
}
