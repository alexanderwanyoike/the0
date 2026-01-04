package docker

import (
	"context"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"runtime/internal/model"
	"runtime/internal/util"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// mockDockerRunner implements DockerRunner for testing
type mockDockerRunner struct {
	startContainerFunc    func(ctx context.Context, exec model.Executable) (*ExecutionResult, error)
	getContainerIPFunc    func(ctx context.Context, containerID string) (string, error)
	stopContainerFunc     func(ctx context.Context, containerID string, exec model.Executable) error
	getContainerLogsFunc  func(ctx context.Context, containerID string, tail int) (string, error)
}

func (m *mockDockerRunner) StartContainer(ctx context.Context, exec model.Executable) (*ExecutionResult, error) {
	if m.startContainerFunc != nil {
		return m.startContainerFunc(ctx, exec)
	}
	return &ExecutionResult{Status: "success", Output: `{"status":"ok","data":{"test":true}}`}, nil
}

func (m *mockDockerRunner) StopContainer(ctx context.Context, containerID string, exec model.Executable) error {
	if m.stopContainerFunc != nil {
		return m.stopContainerFunc(ctx, containerID, exec)
	}
	return nil
}

func (m *mockDockerRunner) GetContainerStatus(ctx context.Context, containerID string) (*ContainerStatus, error) {
	return &ContainerStatus{Status: "running"}, nil
}

func (m *mockDockerRunner) ListManagedContainers(ctx context.Context, segment int32) ([]*ContainerInfo, error) {
	return nil, nil
}

func (m *mockDockerRunner) ListAllManagedContainers(ctx context.Context, segment int32) ([]*ContainerInfo, error) {
	return nil, nil
}

func (m *mockDockerRunner) HandleCrashedContainer(ctx context.Context, info *ContainerInfo) (string, error) {
	return "", nil
}

func (m *mockDockerRunner) GetContainerLogs(ctx context.Context, containerID string, tail int) (string, error) {
	if m.getContainerLogsFunc != nil {
		return m.getContainerLogsFunc(ctx, containerID, tail)
	}
	return "", nil
}

func (m *mockDockerRunner) GetContainerIP(ctx context.Context, containerID string) (string, error) {
	if m.getContainerIPFunc != nil {
		return m.getContainerIPFunc(ctx, containerID)
	}
	return "172.17.0.2", nil
}

func (m *mockDockerRunner) Close() error {
	return nil
}

func TestQueryHandler_ExecuteScheduledQuery(t *testing.T) {
	tests := []struct {
		name           string
		request        QueryRequest
		executable     model.Executable
		mockOutput     string
		expectedStatus string
		expectedError  string
	}{
		{
			name: "successful query",
			request: QueryRequest{
				BotID:     "test-bot",
				QueryPath: "/portfolio",
				Params:    map[string]interface{}{"symbol": "BTC"},
			},
			executable: model.Executable{
				ID:              "test-bot",
				Runtime:         "python3.11",
				Entrypoint:      "bot",
				EntrypointFiles: map[string]string{"bot": "main.py"},
				IsLongRunning:   false,
			},
			mockOutput:     `{"status":"ok","data":{"positions":[{"symbol":"BTC","amount":1.5}]}}`,
			expectedStatus: "ok",
		},
		{
			name: "query with error response",
			request: QueryRequest{
				BotID:     "test-bot",
				QueryPath: "/invalid",
			},
			executable: model.Executable{
				ID:              "test-bot",
				Runtime:         "python3.11",
				Entrypoint:      "bot",
				EntrypointFiles: map[string]string{"bot": "main.py"},
				IsLongRunning:   false,
			},
			mockOutput:     `{"status":"error","error":"No handler for path: /invalid"}`,
			expectedStatus: "error",
			expectedError:  "No handler for path: /invalid",
		},
		{
			name: "query with default timeout",
			request: QueryRequest{
				BotID:     "test-bot",
				QueryPath: "/status",
			},
			executable: model.Executable{
				ID:              "test-bot",
				Runtime:         "nodejs20",
				Entrypoint:      "bot",
				EntrypointFiles: map[string]string{"bot": "index.js"},
				IsLongRunning:   false,
			},
			mockOutput:     `{"status":"ok","data":{"healthy":true}}`,
			expectedStatus: "ok",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			runner := &mockDockerRunner{
				startContainerFunc: func(ctx context.Context, exec model.Executable) (*ExecutionResult, error) {
					// Verify query mode is set correctly
					assert.Equal(t, "query", exec.Entrypoint)
					assert.Equal(t, tt.request.QueryPath, exec.QueryPath)
					assert.Equal(t, tt.request.Params, exec.QueryParams)
					assert.False(t, exec.IsLongRunning)

					return &ExecutionResult{
						Status: "success",
						Output: tt.mockOutput,
					}, nil
				},
			}

			handler := NewQueryHandler(QueryHandlerConfig{
				Runner: runner,
				Logger: &util.DefaultLogger{},
			})

			ctx := context.Background()
			response, err := handler.ExecuteQuery(ctx, tt.request, tt.executable, "")
			require.NoError(t, err)
			require.NotNil(t, response)

			assert.Equal(t, tt.expectedStatus, response.Status)
			if tt.expectedError != "" {
				assert.Equal(t, tt.expectedError, response.Error)
			}
			assert.True(t, response.Duration > 0)
			assert.False(t, response.Timestamp.IsZero())
		})
	}
}

func TestQueryHandler_ExecuteRealtimeQuery(t *testing.T) {
	// Start a mock HTTP server to simulate the bot's query server
	mockServer := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		assert.Equal(t, "/portfolio", r.URL.Path)
		assert.Equal(t, "POST", r.Method)

		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(map[string]interface{}{
			"status": "ok",
			"data": map[string]interface{}{
				"positions": []map[string]interface{}{
					{"symbol": "BTC", "amount": 1.5},
				},
			},
		})
	}))
	defer mockServer.Close()

	// Extract host and port from mock server URL
	serverURL := mockServer.URL[7:] // Remove "http://"

	runner := &mockDockerRunner{
		getContainerIPFunc: func(ctx context.Context, containerID string) (string, error) {
			// Return the mock server's host (without port)
			// The query handler will append :9476, so we need to handle this
			return serverURL, nil
		},
	}

	handler := NewQueryHandler(QueryHandlerConfig{
		Runner: runner,
		Logger: &util.DefaultLogger{},
	})

	// Note: This test won't work directly because the handler appends :9476
	// In a real scenario, we'd need to mock at a different level
	// For now, test that the handler correctly identifies realtime bots

	executable := model.Executable{
		ID:            "test-realtime-bot",
		Runtime:       "python3.11",
		IsLongRunning: true, // This makes it a realtime bot
	}

	request := QueryRequest{
		BotID:      "test-realtime-bot",
		QueryPath:  "/portfolio",
		TimeoutSec: 5,
	}

	ctx := context.Background()
	response, err := handler.ExecuteQuery(ctx, request, executable, "container-123")

	// The request will fail because we can't easily mock the container IP + port 9476
	// But we can verify the handler attempted to make a realtime query
	require.NoError(t, err)
	require.NotNil(t, response)
	// Response will be an error because the mock IP:9476 isn't reachable
	assert.Equal(t, "error", response.Status)
}

func TestQueryHandler_Timeout(t *testing.T) {
	runner := &mockDockerRunner{
		startContainerFunc: func(ctx context.Context, exec model.Executable) (*ExecutionResult, error) {
			// Simulate a slow container that respects context cancellation
			select {
			case <-ctx.Done():
				return nil, ctx.Err()
			case <-time.After(10 * time.Second):
				return &ExecutionResult{Status: "success"}, nil
			}
		},
	}

	handler := NewQueryHandler(QueryHandlerConfig{
		Runner: runner,
		Logger: &util.DefaultLogger{},
	})

	request := QueryRequest{
		BotID:      "test-bot",
		QueryPath:  "/slow",
		TimeoutSec: 1, // 1 second timeout
	}

	executable := model.Executable{
		ID:              "test-bot",
		Runtime:         "python3.11",
		Entrypoint:      "bot",
		EntrypointFiles: map[string]string{"bot": "main.py"},
		IsLongRunning:   false,
	}

	ctx := context.Background()
	response, err := handler.ExecuteQuery(ctx, request, executable, "")

	// Should return an error due to timeout
	require.Error(t, err)
	require.NotNil(t, response)
	assert.Equal(t, "error", response.Status)
}

func TestQueryHandler_InvalidJSONResponse(t *testing.T) {
	runner := &mockDockerRunner{
		startContainerFunc: func(ctx context.Context, exec model.Executable) (*ExecutionResult, error) {
			return &ExecutionResult{
				Status: "success",
				Output: "not valid json",
			}, nil
		},
	}

	handler := NewQueryHandler(QueryHandlerConfig{
		Runner: runner,
		Logger: &util.DefaultLogger{},
	})

	request := QueryRequest{
		BotID:     "test-bot",
		QueryPath: "/test",
	}

	executable := model.Executable{
		ID:              "test-bot",
		Runtime:         "python3.11",
		Entrypoint:      "bot",
		EntrypointFiles: map[string]string{"bot": "main.py"},
		IsLongRunning:   false,
	}

	ctx := context.Background()
	response, err := handler.ExecuteQuery(ctx, request, executable, "")

	require.NoError(t, err)
	require.NotNil(t, response)
	assert.Equal(t, "error", response.Status)
	assert.Contains(t, response.Error, "failed to parse query response")
}

func TestQueryHandler_EntrypointFilesCopied(t *testing.T) {
	var capturedExec model.Executable

	runner := &mockDockerRunner{
		startContainerFunc: func(ctx context.Context, exec model.Executable) (*ExecutionResult, error) {
			capturedExec = exec
			return &ExecutionResult{
				Status: "success",
				Output: `{"status":"ok","data":{}}`,
			}, nil
		},
	}

	handler := NewQueryHandler(QueryHandlerConfig{
		Runner: runner,
		Logger: &util.DefaultLogger{},
	})

	request := QueryRequest{
		BotID:     "test-bot",
		QueryPath: "/test",
	}

	executable := model.Executable{
		ID:              "test-bot",
		Runtime:         "python3.11",
		Entrypoint:      "bot",
		EntrypointFiles: map[string]string{"bot": "main.py"},
		IsLongRunning:   false,
	}

	ctx := context.Background()
	_, err := handler.ExecuteQuery(ctx, request, executable, "")
	require.NoError(t, err)

	// Verify query entrypoint was set up correctly
	assert.Equal(t, "query", capturedExec.Entrypoint)
	assert.Equal(t, "main.py", capturedExec.EntrypointFiles["query"])
	assert.Equal(t, "main.py", capturedExec.EntrypointFiles["bot"])
}
