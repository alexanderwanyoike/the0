package podgen

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	corev1 "k8s.io/api/core/v1"

	"runtime/internal/bot-runner/model"
)

func TestPodGenerator_GeneratePod_BasicBot(t *testing.T) {
	generator := NewPodGenerator(PodGeneratorConfig{
		Namespace:      "the0",
		ControllerName: "the0-bot-controller",
	})

	bot := model.Bot{
		ID: "test-bot-123",
		Config: map[string]interface{}{
			"symbol":    "BTC/USD",
			"threshold": 5.0,
		},
		CustomBotVersion: model.CustomBotVersion{
			Version: "1.0.0",
			Config: model.APIBotConfig{
				Name:    "price-alerts",
				Runtime: "python3.11",
				Entrypoints: map[string]string{
					"bot": "main.py",
				},
			},
			FilePath: "custom-bots/price-alerts/v1.0.0.tar.gz",
		},
	}

	imageRef := "registry.local:5000/the0/bots/price-alerts:1.0.0"

	pod, err := generator.GeneratePod(bot, imageRef)
	require.NoError(t, err)
	require.NotNil(t, pod)

	// Check metadata
	assert.Equal(t, "bot-test-bot-123", pod.Name)
	assert.Equal(t, "the0", pod.Namespace)

	// Check labels
	assert.Equal(t, "test-bot-123", pod.Labels[LabelBotID])
	assert.Equal(t, "price-alerts", pod.Labels[LabelCustomBotID])
	assert.Equal(t, "1.0.0", pod.Labels[LabelCustomBotVersion])
	assert.Equal(t, "python3.11", pod.Labels[LabelRuntime])
	assert.Equal(t, "the0-bot-controller", pod.Labels[LabelManagedBy])

	// Check annotations
	assert.NotEmpty(t, pod.Annotations[AnnotationConfigHash])

	// Check container
	require.Len(t, pod.Spec.Containers, 1)
	container := pod.Spec.Containers[0]
	assert.Equal(t, "bot", container.Name)
	assert.Equal(t, imageRef, container.Image)
	assert.Equal(t, []string{"/bin/bash", "/bot/entrypoint.sh"}, container.Command)
	assert.Equal(t, "/bot", container.WorkingDir)

	// Check env vars
	envMap := make(map[string]string)
	for _, env := range container.Env {
		envMap[env.Name] = env.Value
	}
	assert.Equal(t, "test-bot-123", envMap["BOT_ID"])
	assert.Contains(t, envMap["BOT_CONFIG"], "BTC/USD")
	assert.Equal(t, "/bot", envMap["CODE_MOUNT_DIR"])

	// Check resources
	assert.Equal(t, DefaultMemoryLimit, container.Resources.Limits.Memory().String())
	assert.Equal(t, DefaultCPULimit, container.Resources.Limits.Cpu().String())
}

func TestPodGenerator_GeneratePod_CustomResources(t *testing.T) {
	generator := NewPodGenerator(PodGeneratorConfig{
		Namespace: "the0",
	})

	bot := model.Bot{
		ID: "resource-bot",
		Config: map[string]interface{}{
			"memory_limit":   "1Gi",
			"cpu_limit":      "1000m",
			"memory_request": "256Mi",
			"cpu_request":    "200m",
		},
		CustomBotVersion: model.CustomBotVersion{
			Version: "2.0.0",
			Config: model.APIBotConfig{
				Name:    "heavy-bot",
				Runtime: "nodejs20",
			},
		},
	}

	pod, err := generator.GeneratePod(bot, "registry/the0/bots/heavy-bot:2.0.0")
	require.NoError(t, err)

	container := pod.Spec.Containers[0]
	assert.Equal(t, "1Gi", container.Resources.Limits.Memory().String())
	assert.Equal(t, "1", container.Resources.Limits.Cpu().String())
	assert.Equal(t, "256Mi", container.Resources.Requests.Memory().String())
	assert.Equal(t, "200m", container.Resources.Requests.Cpu().String())
}

func TestPodGenerator_GeneratePod_NodeJSRuntime(t *testing.T) {
	generator := NewPodGenerator(PodGeneratorConfig{})

	bot := model.Bot{
		ID:     "nodejs-bot",
		Config: map[string]interface{}{},
		CustomBotVersion: model.CustomBotVersion{
			Version: "1.0.0",
			Config: model.APIBotConfig{
				Name:    "js-trader",
				Runtime: "nodejs20",
			},
		},
	}

	pod, err := generator.GeneratePod(bot, "registry/the0/bots/js-trader:1.0.0")
	require.NoError(t, err)

	assert.Equal(t, "nodejs20", pod.Labels[LabelRuntime])
}

func TestPodGenerator_GeneratePod_RustRuntime(t *testing.T) {
	generator := NewPodGenerator(PodGeneratorConfig{})

	bot := model.Bot{
		ID:     "rust-bot",
		Config: map[string]interface{}{},
		CustomBotVersion: model.CustomBotVersion{
			Version: "1.0.0",
			Config: model.APIBotConfig{
				Name:    "rust-trader",
				Runtime: "rust-stable",
			},
		},
	}

	pod, err := generator.GeneratePod(bot, "registry/the0/bots/rust-trader:1.0.0")
	require.NoError(t, err)

	assert.Equal(t, "rust-stable", pod.Labels[LabelRuntime])
}

func TestPodGenerator_DefaultConfig(t *testing.T) {
	// Test that default namespace and controller name are set
	generator := NewPodGenerator(PodGeneratorConfig{})

	bot := model.Bot{
		ID: "default-test",
		CustomBotVersion: model.CustomBotVersion{
			Version: "1.0.0",
			Config: model.APIBotConfig{
				Name:    "test",
				Runtime: "python3.11",
			},
		},
	}

	pod, err := generator.GeneratePod(bot, "image:latest")
	require.NoError(t, err)

	assert.Equal(t, "the0", pod.Namespace)
	assert.Equal(t, "the0-bot-controller", pod.Labels[LabelManagedBy])
}

func TestGeneratePodName(t *testing.T) {
	tests := []struct {
		botID    string
		expected string
	}{
		{"abc123", "bot-abc123"},
		{"my-bot-id", "bot-my-bot-id"},
		{"test", "bot-test"},
	}

	for _, tt := range tests {
		t.Run(tt.botID, func(t *testing.T) {
			result := GeneratePodName(tt.botID)
			assert.Equal(t, tt.expected, result)
		})
	}
}

func TestExtractBotID(t *testing.T) {
	tests := []struct {
		name     string
		labels   map[string]string
		expected string
	}{
		{
			name:     "with label",
			labels:   map[string]string{LabelBotID: "bot-123"},
			expected: "bot-123",
		},
		{
			name:     "without label",
			labels:   map[string]string{},
			expected: "",
		},
		{
			name:     "nil labels",
			labels:   nil,
			expected: "",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			pod := &corev1.Pod{}
			pod.Labels = tt.labels
			result := ExtractBotID(pod)
			assert.Equal(t, tt.expected, result)
		})
	}
}

func TestConfigChanged(t *testing.T) {
	generator := NewPodGenerator(PodGeneratorConfig{})

	bot := model.Bot{
		ID: "change-test",
		Config: map[string]interface{}{
			"key": "value1",
		},
		CustomBotVersion: model.CustomBotVersion{
			Version: "1.0.0",
			Config: model.APIBotConfig{
				Name:    "test",
				Runtime: "python3.11",
			},
		},
	}

	// Generate initial pod
	pod, err := generator.GeneratePod(bot, "image:latest")
	require.NoError(t, err)

	// Same config should not be changed
	assert.False(t, ConfigChanged(pod, bot))

	// Modified config should be detected
	botModified := bot
	botModified.Config = map[string]interface{}{
		"key": "value2",
	}
	assert.True(t, ConfigChanged(pod, botModified))
}

func TestSanitizeLabelValue(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{"simple", "simple"},
		{"with-dash", "with-dash"},
		{"with_underscore", "with_underscore"},
		{"with.dot", "with.dot"},
		{"UPPERCASE", "UPPERCASE"},
		{"Mixed123", "Mixed123"},
		{"special@#$chars", "specialchars"},
		{"-starts-with-dash", "starts-with-dash"},
		{"ends-with-dash-", "ends-with-dash"},
		{"", "bot"},
		{"@@@", "bot"},
		{"a", "a"},
		{"very-long-name-that-exceeds-sixty-three-characters-limit-should-be-truncated", "very-long-name-that-exceeds-sixty-three-characters-limit-should"},
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			result := sanitizeLabelValue(tt.input)
			assert.Equal(t, tt.expected, result)
			// Verify result is valid label value
			assert.LessOrEqual(t, len(result), 63)
		})
	}
}

func TestExtractCustomBotID(t *testing.T) {
	tests := []struct {
		name     string
		bot      model.Bot
		expected string
	}{
		{
			name: "from config name",
			bot: model.Bot{
				ID: "bot-123456789",
				CustomBotVersion: model.CustomBotVersion{
					Config: model.APIBotConfig{
						Name: "my-custom-bot",
					},
				},
			},
			expected: "my-custom-bot",
		},
		{
			name: "fallback to short bot ID",
			bot: model.Bot{
				ID: "bot-123456789",
				CustomBotVersion: model.CustomBotVersion{
					Config: model.APIBotConfig{
						Name: "",
					},
				},
			},
			expected: "bot-1234",
		},
		{
			name: "short bot ID",
			bot: model.Bot{
				ID: "short",
				CustomBotVersion: model.CustomBotVersion{
					Config: model.APIBotConfig{
						Name: "",
					},
				},
			},
			expected: "short",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := extractCustomBotID(tt.bot)
			assert.Equal(t, tt.expected, result)
		})
	}
}
