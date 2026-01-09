package podgen

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	corev1 "k8s.io/api/core/v1"

	"runtime/internal/model"
)

func TestPodGenerator_GeneratePod_RealtimeBot(t *testing.T) {
	generator := NewPodGenerator(PodGeneratorConfig{
		Namespace:      "the0",
		ControllerName: "the0-bot-controller",
		MinIOEndpoint:  "minio:9000",
		MinIOAccessKey: "access-key",
		MinIOSecretKey: "secret-key",
		MinIOBucket:    "custom-bots",
		RuntimeImage:   "the0/runtime:latest",
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
			FilePath: "price-alerts/1.0.0",
		},
	}

	pod, err := generator.GeneratePod(bot)
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

	// Check sync sidecar is in InitContainers (native sidecar K8s 1.28+)
	require.Len(t, pod.Spec.InitContainers, 1)
	syncContainer := pod.Spec.InitContainers[0]
	assert.Equal(t, "sync", syncContainer.Name)
	assert.Equal(t, "the0/runtime:latest", syncContainer.Image)
	assert.Equal(t, []string{"/app/runtime", "daemon", "sync"}, syncContainer.Command)
	assert.NotNil(t, syncContainer.RestartPolicy, "native sidecar should have restartPolicy")
	assert.Equal(t, corev1.ContainerRestartPolicyAlways, *syncContainer.RestartPolicy)
	assert.NotNil(t, syncContainer.StartupProbe, "native sidecar should have startup probe")
	assert.Equal(t, "/readyz", syncContainer.StartupProbe.HTTPGet.Path)

	// Check containers (bot only, no query sidecar since no query entrypoint)
	require.Len(t, pod.Spec.Containers, 1)

	// Bot container
	botContainer := pod.Spec.Containers[0]
	assert.Equal(t, "bot", botContainer.Name)
	assert.Equal(t, "the0/runtime:latest", botContainer.Image)
	assert.Equal(t, []string{"/app/runtime", "execute", "--skip-sync", "--skip-query-server"}, botContainer.Command)
	assert.Equal(t, "/bot", botContainer.WorkingDir)

	// Check env vars
	envMap := make(map[string]string)
	for _, env := range botContainer.Env {
		envMap[env.Name] = env.Value
	}
	assert.Equal(t, "test-bot-123", envMap["BOT_ID"])
	assert.Equal(t, "main.py", envMap["ENTRYPOINT"])
	assert.Equal(t, "python3.11", envMap["RUNTIME"])
	assert.Equal(t, "realtime", envMap["BOT_TYPE"])
	assert.Empty(t, envMap["IS_SCHEDULED"], "realtime bots should not have IS_SCHEDULED set")
	assert.Contains(t, envMap["BOT_CONFIG"], "BTC/USD")

	// Check resources
	assert.Equal(t, DefaultMemoryLimit, botContainer.Resources.Limits.Memory().String())
	assert.Equal(t, DefaultCPULimit, botContainer.Resources.Limits.Cpu().String())

	// Check volumes
	require.Len(t, pod.Spec.Volumes, 3)
	assert.Equal(t, "bot-code", pod.Spec.Volumes[0].Name)
	assert.Equal(t, "bot-state", pod.Spec.Volumes[1].Name)
	assert.Equal(t, "the0", pod.Spec.Volumes[2].Name)
}

func TestPodGenerator_GeneratePod_WithQueryEntrypoint(t *testing.T) {
	generator := NewPodGenerator(PodGeneratorConfig{
		Namespace:      "the0",
		MinIOEndpoint:  "minio:9000",
		MinIOAccessKey: "key",
		MinIOSecretKey: "secret",
		MinIOBucket:    "bots",
		RuntimeImage:   "the0/runtime:latest",
	})

	bot := model.Bot{
		ID:     "query-bot",
		Config: map[string]interface{}{},
		CustomBotVersion: model.CustomBotVersion{
			Version: "1.0.0",
			Config: model.APIBotConfig{
				Name:    "bot-with-query",
				Runtime: "python3.11",
				Entrypoints: map[string]string{
					"bot":   "main.py",
					"query": "query.py",
				},
			},
			FilePath: "bot-with-query/1.0.0",
		},
	}

	pod, err := generator.GeneratePod(bot)
	require.NoError(t, err)

	// Sync sidecar should be in InitContainers (native sidecar)
	require.Len(t, pod.Spec.InitContainers, 1)
	assert.Equal(t, "sync", pod.Spec.InitContainers[0].Name)

	// Should have 2 containers: bot + query-server
	require.Len(t, pod.Spec.Containers, 2)
	assert.Equal(t, "bot", pod.Spec.Containers[0].Name)
	assert.Equal(t, "query-server", pod.Spec.Containers[1].Name)

	// Query server should have correct command
	queryContainer := pod.Spec.Containers[1]
	assert.Equal(t, []string{"/app/runtime", "execute", "--query-server-only"}, queryContainer.Command)
}

func TestPodGenerator_GenerateScheduledPodSpec(t *testing.T) {
	generator := NewPodGenerator(PodGeneratorConfig{
		Namespace:      "the0",
		MinIOEndpoint:  "minio:9000",
		MinIOAccessKey: "key",
		MinIOSecretKey: "secret",
		MinIOBucket:    "bots",
		RuntimeImage:   "the0/runtime:latest",
	})

	bot := model.Bot{
		ID:     "scheduled-bot",
		Config: map[string]interface{}{},
		CustomBotVersion: model.CustomBotVersion{
			Version: "1.0.0",
			Config: model.APIBotConfig{
				Name:    "daily-report",
				Runtime: "nodejs20",
				Entrypoints: map[string]string{
					"bot":   "index.js",
					"query": "query.js", // Even with query entrypoint, scheduled bots don't get query sidecar
				},
			},
			FilePath: "daily-report/1.0.0",
		},
	}

	podSpec, err := generator.GenerateScheduledPodSpec(bot)
	require.NoError(t, err)

	// Scheduled bots should not restart
	assert.Equal(t, corev1.RestartPolicyNever, podSpec.RestartPolicy)

	// Scheduled bots should have only 1 container - no sidecars
	// Sync runs inline as part of the execute command
	require.Len(t, podSpec.Containers, 1)
	assert.Equal(t, "bot", podSpec.Containers[0].Name)

	// Bot should use inline sync (no --skip-sync flag)
	botCommand := podSpec.Containers[0].Command
	assert.Equal(t, []string{"/app/runtime", "execute", "--skip-query-server"}, botCommand)
	assert.NotContains(t, botCommand, "--skip-sync") // Sync runs inline for scheduled bots

	// Bot should have BOT_TYPE=scheduled and IS_SCHEDULED=true
	botEnvMap := make(map[string]string)
	for _, env := range podSpec.Containers[0].Env {
		botEnvMap[env.Name] = env.Value
	}
	assert.Equal(t, "scheduled", botEnvMap["BOT_TYPE"])
	assert.Equal(t, "true", botEnvMap["IS_SCHEDULED"], "scheduled bots must set IS_SCHEDULED=true for done file mechanism")
}

func TestPodGenerator_GenerateQueryPod(t *testing.T) {
	generator := NewPodGenerator(PodGeneratorConfig{
		Namespace:      "the0",
		MinIOEndpoint:  "minio:9000",
		MinIOAccessKey: "key",
		MinIOSecretKey: "secret",
		MinIOBucket:    "bots",
		RuntimeImage:   "the0/runtime:latest",
	})

	bot := model.Bot{
		ID:     "query-target",
		Config: map[string]interface{}{},
		CustomBotVersion: model.CustomBotVersion{
			Version: "1.0.0",
			Config: model.APIBotConfig{
				Name:    "bot",
				Runtime: "python3.11",
				Entrypoints: map[string]string{
					"bot":   "main.py",
					"query": "query.py",
				},
			},
			FilePath: "bot/1.0.0",
		},
	}

	pod, err := generator.GenerateQueryPod(bot, "/status", map[string]interface{}{"key": "value"})
	require.NoError(t, err)

	// Query pods should not restart
	assert.Equal(t, corev1.RestartPolicyNever, pod.Spec.RestartPolicy)

	// Should have only 1 container: bot (no sidecars)
	require.Len(t, pod.Spec.Containers, 1)
	assert.Equal(t, "bot", pod.Spec.Containers[0].Name)

	// Should have QUERY_PATH and QUERY_PARAMS env vars
	envMap := make(map[string]string)
	for _, env := range pod.Spec.Containers[0].Env {
		envMap[env.Name] = env.Value
	}
	assert.Equal(t, "/status", envMap["QUERY_PATH"])
	assert.Contains(t, envMap["QUERY_PARAMS"], "key")

	// Should use query entrypoint when available
	assert.Equal(t, "query.py", envMap["ENTRYPOINT"])
}

func TestPodGenerator_ValidationErrors(t *testing.T) {
	generator := NewPodGenerator(PodGeneratorConfig{
		MinIOEndpoint:  "minio:9000",
		MinIOAccessKey: "key",
		MinIOSecretKey: "secret",
		MinIOBucket:    "bots",
	})

	tests := []struct {
		name        string
		bot         model.Bot
		errContains string
	}{
		{
			name: "missing runtime",
			bot: model.Bot{
				ID: "test",
				CustomBotVersion: model.CustomBotVersion{
					FilePath: "test/1.0.0",
					Config: model.APIBotConfig{
						Entrypoints: map[string]string{"bot": "main.py"},
					},
				},
			},
			errContains: "runtime is required",
		},
		{
			name: "missing entrypoint",
			bot: model.Bot{
				ID: "test",
				CustomBotVersion: model.CustomBotVersion{
					FilePath: "test/1.0.0",
					Config: model.APIBotConfig{
						Runtime: "python3.11",
					},
				},
			},
			errContains: "entrypoint is required",
		},
		{
			name: "missing file path",
			bot: model.Bot{
				ID: "test",
				CustomBotVersion: model.CustomBotVersion{
					Config: model.APIBotConfig{
						Runtime:     "python3.11",
						Entrypoints: map[string]string{"bot": "main.py"},
					},
				},
			},
			errContains: "file path is required",
		},
		{
			name: "invalid file path (command injection)",
			bot: model.Bot{
				ID: "test",
				CustomBotVersion: model.CustomBotVersion{
					FilePath: "test; rm -rf /",
					Config: model.APIBotConfig{
						Runtime:     "python3.11",
						Entrypoints: map[string]string{"bot": "main.py"},
					},
				},
			},
			errContains: "invalid file path",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			_, err := generator.GeneratePod(tt.bot)
			require.Error(t, err)
			assert.Contains(t, err.Error(), tt.errContains)
		})
	}
}

func TestPodGenerator_CustomResources(t *testing.T) {
	generator := NewPodGenerator(PodGeneratorConfig{
		MinIOEndpoint:  "minio:9000",
		MinIOAccessKey: "key",
		MinIOSecretKey: "secret",
		MinIOBucket:    "bots",
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
			Version:  "1.0.0",
			FilePath: "test/1.0.0",
			Config: model.APIBotConfig{
				Name:        "heavy-bot",
				Runtime:     "nodejs20",
				Entrypoints: map[string]string{"bot": "index.js"},
			},
		},
	}

	pod, err := generator.GeneratePod(bot)
	require.NoError(t, err)

	container := pod.Spec.Containers[0]
	assert.Equal(t, "1Gi", container.Resources.Limits.Memory().String())
	assert.Equal(t, "1", container.Resources.Limits.Cpu().String())
	assert.Equal(t, "256Mi", container.Resources.Requests.Memory().String())
	assert.Equal(t, "200m", container.Resources.Requests.Cpu().String())
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
	generator := NewPodGenerator(PodGeneratorConfig{
		MinIOEndpoint:  "minio:9000",
		MinIOAccessKey: "key",
		MinIOSecretKey: "secret",
		MinIOBucket:    "bots",
	})

	bot := model.Bot{
		ID: "change-test",
		Config: map[string]interface{}{
			"key": "value1",
		},
		CustomBotVersion: model.CustomBotVersion{
			Version:  "1.0.0",
			FilePath: "test/1.0.0",
			Config: model.APIBotConfig{
				Name:        "test",
				Runtime:     "python3.11",
				Entrypoints: map[string]string{"bot": "main.py"},
			},
		},
	}

	pod, err := generator.GeneratePod(bot)
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

func TestValidateMinIOPath(t *testing.T) {
	tests := []struct {
		name    string
		path    string
		wantErr bool
	}{
		{"valid simple", "mybot/1.0.0", false},
		{"valid with dashes", "my-bot/1.0.0-beta", false},
		{"valid with underscores", "my_bot/1_0_0", false},
		{"valid with dots", "my.bot/1.0.0", false},
		{"valid alphanumeric", "bot123/v1", false},
		{"shell injection semicolon", "mybot; rm -rf /", true},
		{"shell injection pipe", "mybot | cat /etc/passwd", true},
		{"shell injection backtick", "mybot`whoami`", true},
		{"shell injection dollar", "mybot$(whoami)", true},
		{"shell injection ampersand", "mybot && rm -rf /", true},
		{"shell injection space", "my bot/1.0.0", true},
		{"shell injection quotes", "mybot'test", true},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			err := validateMinIOPath(tt.path)
			if tt.wantErr {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
			}
		})
	}
}
