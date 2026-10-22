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

	"runtime/internal/constants"
	"runtime/internal/launch"
	"runtime/internal/model"
	"runtime/internal/query"
	runtimepkg "runtime/internal/runtime"
)

const (
	// LabelBotID is the label key for the bot instance ID.
	LabelBotID = "the0.app/bot-id"

	// LabelCustomBotID is the label key for the custom bot type ID.
	LabelCustomBotID = "the0.app/custom-bot-id"

	// LabelCustomBotVersion is the label key for the custom bot version.
	LabelCustomBotVersion = "the0.app/custom-bot-version"

	// LabelRuntime is the label key for the bot runtime (python3.11, nodejs22, etc).
	LabelRuntime = "the0.app/runtime"

	// LabelManagedBy indicates the controller managing this pod.
	LabelManagedBy = "the0.app/managed-by"

	// AnnotationConfigHash stores a hash of the bot config for change detection.
	AnnotationConfigHash = "the0.app/config-hash"
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

	// StorageCredentials, when named, is where pods read MinIOAccessKey and
	// MinIOSecretKey from instead of carrying them as literal values.
	StorageCredentials StorageCredentialsSecret

	// NATSURL enables live log streaming: the sync process publishes bot log
	// lines to NATS for the API's SSE fan-out. Empty disables publishing.
	NATSURL string

	// RuntimeImage is the image for init and sidecar containers (e.g., "the0/runtime:latest")
	RuntimeImage string

	// RuntimeImagePullPolicy for init and sidecar containers (default: IfNotPresent)
	RuntimeImagePullPolicy corev1.PullPolicy
}

// StorageCredentialsSecret is the Secret, and its keys, that holds the storage
// credentials.
type StorageCredentialsSecret struct {
	Name         string
	AccessKeyKey string
	SecretKeyKey string
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
		config.MinIOStateBucket = launch.DefaultStateBucket
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

	image, err := g.runtimeImageFor(botConfig.runtime)
	if err != nil {
		return nil, err
	}

	settings, err := g.launchOf(bot, botConfig)
	if err != nil {
		return nil, err
	}
	settings.Mode = launch.Run
	settings.SkipSync = true
	settings.SkipQueryServer = true
	settings.BotType = "realtime"
	settings.QueryEntrypoint = botConfig.queryEntrypoint
	settings.NATSURL = g.config.NATSURL

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
		WithLaunch(settings).
		WithStorageCredentialsFrom(g.config.StorageCredentials).
		WithResources(botConfig.memoryLimit, botConfig.cpuLimit, botConfig.memoryRequest, botConfig.cpuRequest).
		WithSyncSidecar(g.config.RuntimeImage, bot.ID)

	// Add query entrypoint if configured
	if botConfig.queryEntrypoint != "" {
		builder.WithQuerySidecar(image)
	}

	pod, err := builder.Build()
	if err != nil {
		return nil, err
	}
	pod.Annotations[AnnotationConfigHash] = computeConfigHash(bot, pod)
	return pod, nil
}

// GenerateScheduledPodSpec creates a Pod spec for scheduled bots (CronJobs).
// Scheduled bots run once per trigger as a single container - no sidecars.
// Sync (state/log upload) runs as a process within the container after bot execution.
// No query server sidecar - queries are executed ephemerally.
func (g *PodGenerator) GenerateScheduledPodSpec(bot model.Bot) (*corev1.PodSpec, error) {
	// Extract and validate required configuration
	botConfig, err := g.extractBotConfig(bot)
	if err != nil {
		return nil, err
	}

	image, err := g.runtimeImageFor(botConfig.runtime)
	if err != nil {
		return nil, err
	}

	settings, err := g.launchOf(bot, botConfig)
	if err != nil {
		return nil, err
	}
	settings.Mode = launch.Run
	settings.SkipQueryServer = true
	settings.BotType = "scheduled"
	settings.NATSURL = g.config.NATSURL

	// Build the pod for scheduled execution - NO sidecars
	// Scheduled bots run sync as part of the execute command, not as a sidecar
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
		WithLaunch(settings).
		WithStorageCredentialsFrom(g.config.StorageCredentials).
		WithResources(botConfig.memoryLimit, botConfig.cpuLimit, botConfig.memoryRequest, botConfig.cpuRequest)
	// Note: No query sidecar - queries are ephemeral

	return builder.BuildSpec()
}

// GenerateQueryPod creates a Pod spec for executing a query against a bot.
// Query pods run once without sidecars, and upload their result to resultKey.
func (g *PodGenerator) GenerateQueryPod(bot model.Bot, queryPath string, queryParams map[string]interface{}, resultKey string) (*corev1.Pod, error) {
	// Extract and validate required configuration
	botConfig, err := g.extractBotConfig(bot)
	if err != nil {
		return nil, err
	}

	image, err := g.runtimeImageFor(botConfig.runtime)
	if err != nil {
		return nil, err
	}

	// See query.ErrNoQueryEntrypoint for why there is no bot-entrypoint fallback.
	if botConfig.queryEntrypoint == "" {
		return nil, query.NoQueryEntrypointError(bot.ID)
	}

	settings, err := g.launchOf(bot, botConfig)
	if err != nil {
		return nil, err
	}
	settings.Mode = launch.Query
	settings.Entrypoint = botConfig.queryEntrypoint
	settings.QueryEntrypoint = botConfig.queryEntrypoint
	settings.QueryPath = queryPath
	settings.QueryParams, err = launch.QueryParams(queryParams)
	if err != nil {
		return nil, err
	}
	settings.QueryResultKey = resultKey

	// Build the pod for query execution
	builder := NewPodBuilder(fmt.Sprintf("query-%s", bot.ID), g.config.Namespace).
		WithImage(image).
		WithImagePullPolicy(g.config.RuntimeImagePullPolicy).
		WithRestartPolicy(corev1.RestartPolicyNever).
		WithLabels(map[string]string{
			LabelBotID:            bot.ID,
			LabelCustomBotID:      botConfig.customBotID,
			LabelCustomBotVersion: botConfig.version,
			LabelRuntime:          botConfig.runtime,
			LabelManagedBy:        g.config.ControllerName,
		}).
		WithLaunch(settings).
		WithStorageCredentialsFrom(g.config.StorageCredentials).
		WithResources(botConfig.memoryLimit, botConfig.cpuLimit, botConfig.memoryRequest, botConfig.cpuRequest)

	return builder.Build()
}

// launchOf is the launch of bot that every container of its pods starts
// from, before the pod gives it a mode.
func (g *PodGenerator) launchOf(bot model.Bot, cfg *botConfig) (launch.Settings, error) {
	configJSON, err := json.Marshal(bot.Config)
	if err != nil {
		return launch.Settings{}, fmt.Errorf("failed to marshal config: %w", err)
	}
	return launch.Settings{
		BotID:      bot.ID,
		CodeFile:   cfg.filePath,
		Runtime:    cfg.runtime,
		Entrypoint: cfg.entrypoint,
		BotConfig:  string(configJSON),
		CodePath:   constants.DefaultCodePath,
		StatePath:  constants.DefaultStatePath,
		LogsPath:   constants.DefaultLogsPath,
		StateDir:   launch.DefaultStateDir,
		Storage: launch.Storage{
			Endpoint:    g.config.MinIOEndpoint,
			AccessKey:   g.config.MinIOAccessKey,
			SecretKey:   g.config.MinIOSecretKey,
			UseSSL:      g.config.MinIOUseSSL,
			CodeBucket:  g.config.MinIOBucket,
			StateBucket: g.config.MinIOStateBucket,
		},
	}, nil
}

func (g *PodGenerator) runtimeImageFor(runtime string) (string, error) {
	if _, err := runtimepkg.GetDockerImage(runtime); err != nil {
		return "", fmt.Errorf("failed to validate runtime image: %w", err)
	}
	return g.config.RuntimeImage, nil
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

// ConfigChanged returns true if the pod the generator writes for bot differs
// from the one pod was created from.
func (g *PodGenerator) ConfigChanged(pod *corev1.Pod, bot model.Bot) bool {
	wanted, err := g.GeneratePod(bot)
	if err != nil {
		// The caller replaces a changed pod by deleting it, and a bot that
		// cannot be generated could not be recreated, so keep it running.
		return false
	}
	return ExtractConfigHash(pod) != ExtractConfigHash(wanted)
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

// computeConfigHash hashes the bot together with the pod generated for it.
// Hashing the generated pod, not a list of the generator inputs behind it,
// means any change to what the generator writes, such as the storage endpoint
// or credentials, replaces running pods.
func computeConfigHash(bot model.Bot, pod *corev1.Pod) string {
	data := struct {
		Config           map[string]interface{}
		CustomBotVersion string
		Labels           map[string]string
		Spec             corev1.PodSpec
	}{
		Config:           bot.Config,
		CustomBotVersion: bot.CustomBotVersion.Version,
		Labels:           pod.Labels,
		Spec:             pod.Spec,
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
