package docker

import (
	"encoding/json"
	"fmt"
	"runtime/internal/model"
	"testing"

	"github.com/docker/docker/api/types/container"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestNewContainerBuilder(t *testing.T) {
	builder := NewContainerBuilder("python:3.11")

	cfg, hostCfg := builder.Build()

	// Verify defaults
	assert.Equal(t, "python:3.11", cfg.Image)
	assert.Equal(t, "SIGTERM", cfg.StopSignal)
	assert.Equal(t, "/tmp", cfg.WorkingDir)
	assert.NotNil(t, cfg.Labels)
	assert.Contains(t, cfg.Env, "PYTHONDONTWRITEBYTECODE=1")
	assert.Equal(t, container.NetworkMode("bridge"), hostCfg.NetworkMode)
}

func TestContainerBuilder_WithNetwork(t *testing.T) {
	tests := []struct {
		name            string
		networkName     string
		expectedNetwork container.NetworkMode
	}{
		{
			name:            "custom network",
			networkName:     "the0-network",
			expectedNetwork: "the0-network",
		},
		{
			name:            "empty network keeps default",
			networkName:     "",
			expectedNetwork: "bridge",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			builder := NewContainerBuilder("python:3.11").
				WithNetwork(tt.networkName)

			_, hostCfg := builder.Build()

			assert.Equal(t, tt.expectedNetwork, hostCfg.NetworkMode)
		})
	}
}

func TestContainerBuilder_WithCommand(t *testing.T) {
	builder := NewContainerBuilder("python:3.11").
		WithCommand("/bin/bash", "/app/entrypoint.sh")

	cfg, _ := builder.Build()

	assert.Len(t, cfg.Cmd, 2)
	assert.Equal(t, "/bin/bash", cfg.Cmd[0])
	assert.Equal(t, "/app/entrypoint.sh", cfg.Cmd[1])
}

func TestContainerBuilder_WithBinds(t *testing.T) {
	builder := NewContainerBuilder("python:3.11").
		WithBinds("/host/path:/container/path:ro").
		WithBinds("/host/path2:/container/path2")

	_, hostCfg := builder.Build()

	assert.Len(t, hostCfg.Binds, 2)
	assert.Contains(t, hostCfg.Binds, "/host/path:/container/path:ro")
	assert.Contains(t, hostCfg.Binds, "/host/path2:/container/path2")
}

func TestContainerBuilder_WithEnv(t *testing.T) {
	builder := NewContainerBuilder("python:3.11").
		WithEnv("KEY1=value1", "KEY2=value2").
		WithEnv("KEY3=value3")

	cfg, _ := builder.Build()

	// Should have default PYTHONDONTWRITEBYTECODE=1 + 3 new vars
	assert.Len(t, cfg.Env, 4)
	assert.Contains(t, cfg.Env, "KEY1=value1")
	assert.Contains(t, cfg.Env, "KEY2=value2")
	assert.Contains(t, cfg.Env, "KEY3=value3")
}

func TestContainerBuilder_WithResources(t *testing.T) {
	memoryLimit := int64(512 * 1024 * 1024) // 512 MB
	cpuShares := int64(1024)

	builder := NewContainerBuilder("python:3.11").
		WithResources(memoryLimit, cpuShares)

	_, hostCfg := builder.Build()

	assert.Equal(t, memoryLimit, hostCfg.Resources.Memory)
	assert.Equal(t, cpuShares, hostCfg.Resources.CPUShares)
}

func TestContainerBuilder_WithAutoRemove(t *testing.T) {
	tests := []struct {
		name       string
		autoRemove bool
	}{
		{"auto remove enabled", true},
		{"auto remove disabled", false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			builder := NewContainerBuilder("python:3.11").
				WithAutoRemove(tt.autoRemove)

			_, hostCfg := builder.Build()

			assert.Equal(t, tt.autoRemove, hostCfg.AutoRemove)
		})
	}
}

func TestContainerBuilder_WithExecutable(t *testing.T) {
	executable := model.Executable{
		ID:         "bot-123",
		Entrypoint: "bot",
		EntrypointFiles: map[string]string{
			"bot": "main.py",
		},
		Config: map[string]interface{}{
			"symbol": "BTC/USD",
			"threshold": 100,
		},
		Segment: 5,
	}

	builder := NewContainerBuilder("python:3.11").
		WithExecutable(executable)

	cfg, _ := builder.Build()

	// Verify environment variables
	assert.Contains(t, cfg.Env, "ID=bot-123")
	assert.Contains(t, cfg.Env, "ENTRYPOINT_TYPE=bot")
	assert.Contains(t, cfg.Env, "CODE_MOUNT_DIR=bot")
	assert.Contains(t, cfg.Env, "STATE_DIR=/state/.the0-state")

	// Verify CONFIG env var contains marshaled JSON
	var configEnv string
	for _, env := range cfg.Env {
		if len(env) > 7 && env[:7] == "CONFIG=" {
			configEnv = env[7:]
			break
		}
	}
	assert.NotEmpty(t, configEnv)

	// Verify it's valid JSON
	var parsedConfig map[string]interface{}
	err := json.Unmarshal([]byte(configEnv), &parsedConfig)
	require.NoError(t, err)
	assert.Equal(t, "BTC/USD", parsedConfig["symbol"])
	assert.Equal(t, float64(100), parsedConfig["threshold"])

	// Verify labels
	assert.Equal(t, "bot-123", cfg.Labels["runtime.id"])
	assert.Equal(t, "main.py", cfg.Labels["runtime.entrypoint"])
	assert.Equal(t, "true", cfg.Labels["runtime.managed"])
}

func TestContainerBuilder_WithExecutable_MarshalError(t *testing.T) {
	// Create an executable with a config that can't be marshaled
	// (functions can't be marshaled to JSON)
	type unmarshalable struct {
		Fn func()
	}

	executable := model.Executable{
		ID:         "bot-123",
		Entrypoint: "bot",
		EntrypointFiles: map[string]string{
			"bot": "main.py",
		},
		Config: map[string]interface{}{
			"unmarshalable": unmarshalable{Fn: func() {}},
		},
		Segment: 0,
	}

	builder := NewContainerBuilder("python:3.11").
		WithExecutable(executable)

	cfg, _ := builder.Build()

	// Should fallback to empty config
	var configEnv string
	for _, env := range cfg.Env {
		if len(env) > 7 && env[:7] == "CONFIG=" {
			configEnv = env[7:]
			break
		}
	}
	// Fallback should be empty JSON object
	assert.Equal(t, "{}", configEnv)
}

func TestContainerBuilder_WithMinIOConfig(t *testing.T) {
	builder := NewContainerBuilder("python:3.11").
		WithMinIOConfig("minio:9000", "admin", "password", true)

	cfg, _ := builder.Build()

	assert.Contains(t, cfg.Env, "MINIO_ENDPOINT=minio:9000")
	assert.Contains(t, cfg.Env, "MINIO_ACCESS_KEY=admin")
	assert.Contains(t, cfg.Env, "MINIO_SECRET_KEY=password")
	assert.Contains(t, cfg.Env, "MINIO_USE_SSL=true")
}

func TestContainerBuilder_WithDaemonConfig(t *testing.T) {
	builder := NewContainerBuilder("python:3.11").
		WithDaemonConfig("bot-456", "code.zip", "python3.11", "main.py", `{"key":"value"}`, false)

	cfg, _ := builder.Build()

	assert.Contains(t, cfg.Env, "BOT_ID=bot-456")
	assert.Contains(t, cfg.Env, "CODE_FILE=code.zip")
	assert.Contains(t, cfg.Env, "RUNTIME=python3.11")
	assert.Contains(t, cfg.Env, "ENTRYPOINT=main.py")
	assert.Contains(t, cfg.Env, `BOT_CONFIG={"key":"value"}`)
	assert.Contains(t, cfg.Env, "IS_SCHEDULED=false")
}

func TestContainerBuilder_WithDaemonConfigFull(t *testing.T) {
	tests := []struct {
		name           string
		config         DaemonConfig
		expectedEnvs   []string
		unexpectedEnvs []string
	}{
		{
			name: "full config with query entrypoint",
			config: DaemonConfig{
				BotID:           "bot-789",
				CodeFile:        "code.zip",
				Runtime:         "nodejs20",
				Entrypoint:      "index.js",
				QueryEntrypoint: "query.js",
				Config:          `{"timeout":30}`,
				IsScheduled:     true,
				BotType:         "realtime",
			},
			expectedEnvs: []string{
				"BOT_ID=bot-789",
				"CODE_FILE=code.zip",
				"RUNTIME=nodejs20",
				"ENTRYPOINT=index.js",
				"QUERY_ENTRYPOINT=query.js",
				`BOT_CONFIG={"timeout":30}`,
				"IS_SCHEDULED=true",
				"BOT_TYPE=realtime",
			},
			unexpectedEnvs: []string{},
		},
		{
			name: "minimal config without optional fields",
			config: DaemonConfig{
				BotID:       "bot-123",
				CodeFile:    "code.zip",
				Runtime:     "python3.11",
				Entrypoint:  "main.py",
				Config:      "{}",
				IsScheduled: false,
			},
			expectedEnvs: []string{
				"BOT_ID=bot-123",
				"CODE_FILE=code.zip",
				"RUNTIME=python3.11",
				"ENTRYPOINT=main.py",
				"BOT_CONFIG={}",
				"IS_SCHEDULED=false",
			},
			unexpectedEnvs: []string{
				"QUERY_ENTRYPOINT=",
				"BOT_TYPE=",
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			builder := NewContainerBuilder("python:3.11").
				WithDaemonConfigFull(tt.config)

			cfg, _ := builder.Build()

			for _, expected := range tt.expectedEnvs {
				assert.Contains(t, cfg.Env, expected, "Expected env var: %s", expected)
			}

			for _, unexpected := range tt.unexpectedEnvs {
				// Check that no env var starts with this prefix
				for _, env := range cfg.Env {
					assert.False(t, len(env) >= len(unexpected) && env[:len(unexpected)] == unexpected,
						"Unexpected env var prefix: %s in %s", unexpected, env)
				}
			}
		})
	}
}

func TestContainerBuilder_WithDevRuntime(t *testing.T) {
	tests := []struct {
		name        string
		hostPath    string
		expectBind  bool
		expectedBind string
	}{
		{
			name:         "with host path",
			hostPath:     "/home/user/runtime",
			expectBind:   true,
			expectedBind: "/home/user/runtime:/app/runtime:ro",
		},
		{
			name:       "empty host path",
			hostPath:   "",
			expectBind: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			builder := NewContainerBuilder("python:3.11").
				WithDevRuntime(tt.hostPath)

			_, hostCfg := builder.Build()

			if tt.expectBind {
				assert.Contains(t, hostCfg.Binds, tt.expectedBind)
			} else {
				assert.Empty(t, hostCfg.Binds)
			}
		})
	}
}

func TestContainerBuilder_WithQueryConfig(t *testing.T) {
	tests := []struct {
		name        string
		queryPath   string
		queryParams string
		expectedEnvs []string
	}{
		{
			name:        "with query path and params",
			queryPath:   "/api/price",
			queryParams: `{"symbol":"BTC"}`,
			expectedEnvs: []string{
				"QUERY_PATH=/api/price",
				`QUERY_PARAMS={"symbol":"BTC"}`,
			},
		},
		{
			name:        "with query path only",
			queryPath:   "/api/status",
			queryParams: "",
			expectedEnvs: []string{
				"QUERY_PATH=/api/status",
			},
		},
		{
			name:         "empty query path",
			queryPath:    "",
			queryParams:  `{"key":"value"}`,
			expectedEnvs: []string{},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			builder := NewContainerBuilder("python:3.11").
				WithQueryConfig(tt.queryPath, tt.queryParams)

			cfg, _ := builder.Build()

			if len(tt.expectedEnvs) > 0 {
				for _, expected := range tt.expectedEnvs {
					assert.Contains(t, cfg.Env, expected)
				}
			} else {
				// If no query path, QUERY_PATH should not be set
				for _, env := range cfg.Env {
					assert.False(t, len(env) >= 11 && env[:11] == "QUERY_PATH=")
				}
			}
		})
	}
}

func TestContainerBuilder_WithExtraHosts(t *testing.T) {
	builder := NewContainerBuilder("python:3.11").
		WithExtraHosts("host.docker.internal:host-gateway").
		WithExtraHosts("custom.local:192.168.1.100", "another.local:192.168.1.101")

	_, hostCfg := builder.Build()

	assert.Len(t, hostCfg.ExtraHosts, 3)
	assert.Contains(t, hostCfg.ExtraHosts, "host.docker.internal:host-gateway")
	assert.Contains(t, hostCfg.ExtraHosts, "custom.local:192.168.1.100")
	assert.Contains(t, hostCfg.ExtraHosts, "another.local:192.168.1.101")
}

func TestContainerBuilder_ChainedCalls(t *testing.T) {
	// Test that all methods can be chained together
	builder := NewContainerBuilder("python:3.11").
		WithNetwork("the0-network").
		WithCommand("/bin/bash", "/app/start.sh").
		WithBinds("/host:/container:ro").
		WithEnv("KEY1=value1", "KEY2=value2").
		WithResources(1024*1024*1024, 2048).
		WithAutoRemove(true).
		WithMinIOConfig("minio:9000", "admin", "secret", false).
		WithDaemonConfig("bot-1", "code.zip", "python3.11", "main.py", "{}", false).
		WithQueryConfig("/query", `{"param":"value"}`).
		WithExtraHosts("host.docker.internal:host-gateway")

	cfg, hostCfg := builder.Build()

	// Verify config
	assert.Equal(t, "python:3.11", cfg.Image)
	assert.Len(t, cfg.Cmd, 2)
	assert.Equal(t, "/bin/bash", cfg.Cmd[0])
	assert.Equal(t, "/app/start.sh", cfg.Cmd[1])
	assert.Contains(t, cfg.Env, "KEY1=value1")
	assert.Contains(t, cfg.Env, "MINIO_ENDPOINT=minio:9000")
	assert.Contains(t, cfg.Env, "BOT_ID=bot-1")
	assert.Contains(t, cfg.Env, "QUERY_PATH=/query")

	// Verify host config
	assert.Equal(t, container.NetworkMode("the0-network"), hostCfg.NetworkMode)
	assert.Contains(t, hostCfg.Binds, "/host:/container:ro")
	assert.Equal(t, int64(1024*1024*1024), hostCfg.Resources.Memory)
	assert.Equal(t, int64(2048), hostCfg.Resources.CPUShares)
	assert.True(t, hostCfg.AutoRemove)
	assert.Contains(t, hostCfg.ExtraHosts, "host.docker.internal:host-gateway")
}

func TestContainerBuilder_Build(t *testing.T) {
	builder := NewContainerBuilder("python:3.11").
		WithEnv("TEST=value")

	cfg, hostCfg := builder.Build()

	// Verify we get both config objects
	assert.NotNil(t, cfg)
	assert.NotNil(t, hostCfg)

	// Verify they contain expected data
	assert.Equal(t, "python:3.11", cfg.Image)
	assert.Equal(t, container.NetworkMode("bridge"), hostCfg.NetworkMode)
}

func TestContainerBuilder_MultipleBuilds(t *testing.T) {
	builder := NewContainerBuilder("python:3.11").
		WithEnv("INITIAL=value")

	// First build
	cfg1, hostCfg1 := builder.Build()
	assert.Contains(t, cfg1.Env, "INITIAL=value")

	// Add more config
	builder.WithEnv("ADDED=value2")

	// Second build should include both
	cfg2, hostCfg2 := builder.Build()
	assert.Contains(t, cfg2.Env, "INITIAL=value")
	assert.Contains(t, cfg2.Env, "ADDED=value2")

	// Verify they're the same underlying objects (not copies)
	assert.Same(t, cfg1, cfg2)
	assert.Same(t, hostCfg1, hostCfg2)
}

// Benchmark tests for performance-sensitive builder operations
func BenchmarkContainerBuilder_WithEnv(b *testing.B) {
	for i := 0; i < b.N; i++ {
		builder := NewContainerBuilder("python:3.11")
		for j := 0; j < 100; j++ {
			builder.WithEnv(fmt.Sprintf("VAR%d=value%d", j, j))
		}
		builder.Build()
	}
}

func BenchmarkContainerBuilder_FullChain(b *testing.B) {
	for i := 0; i < b.N; i++ {
		NewContainerBuilder("python:3.11").
			WithNetwork("the0-network").
			WithCommand("/bin/bash", "/app/start.sh").
			WithBinds("/host:/container").
			WithEnv("KEY=value").
			WithResources(1024*1024*512, 1024).
			WithAutoRemove(true).
			WithMinIOConfig("minio:9000", "admin", "secret", false).
			WithDaemonConfig("bot-1", "code.zip", "python3.11", "main.py", "{}", false).
			Build()
	}
}
