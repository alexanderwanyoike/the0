package docker

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"net/http/httptest"
	"runtime/internal/model"
	"runtime/internal/util"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// mockBotResolver implements BotResolver for testing
type mockBotResolver struct {
	bots       map[string]*model.Bot
	containers map[string]string // botID -> containerID
}

func newMockBotResolver() *mockBotResolver {
	return &mockBotResolver{
		bots:       make(map[string]*model.Bot),
		containers: make(map[string]string),
	}
}

func (m *mockBotResolver) GetBot(ctx context.Context, botID string) (*model.Bot, error) {
	bot, ok := m.bots[botID]
	if !ok {
		return nil, fmt.Errorf("bot not found: %s", botID)
	}
	return bot, nil
}

func (m *mockBotResolver) GetContainerID(ctx context.Context, botID string) (string, bool) {
	containerID, ok := m.containers[botID]
	return containerID, ok
}

func (m *mockBotResolver) addBot(bot *model.Bot, containerID string) {
	m.bots[bot.ID] = bot
	if containerID != "" {
		m.containers[bot.ID] = containerID
	}
}

// mockQueryHandler implements a simple query handler for testing
type mockQueryHandler struct {
	executeFunc func(ctx context.Context, req QueryRequest, exec model.Executable, containerID string) (*QueryResponse, error)
}

func (m *mockQueryHandler) ExecuteQuery(ctx context.Context, req QueryRequest, exec model.Executable, containerID string) (*QueryResponse, error) {
	if m.executeFunc != nil {
		return m.executeFunc(ctx, req, exec, containerID)
	}
	return &QueryResponse{
		Status:    "ok",
		Data:      json.RawMessage(`{"test":true}`),
		Duration:  100 * time.Millisecond,
		Timestamp: time.Now(),
	}, nil
}

func TestQueryServer_HandleQuery_Success(t *testing.T) {
	resolver := newMockBotResolver()
	resolver.addBot(&model.Bot{
		ID: "test-bot",
		CustomBotVersion: model.CustomBotVersion{
			Config: model.APIBotConfig{
				Runtime:     "python3.11",
				Entrypoints: map[string]string{"bot": "main.py", "query": "query.py"},
			},
			FilePath: "test-bot/code.zip",
		},
		Config: map[string]interface{}{"symbol": "BTC"},
	}, "")

	// Create test request
	reqBody := map[string]interface{}{
		"bot_id":     "test-bot",
		"query_path": "/portfolio",
		"params":     map[string]interface{}{"symbol": "BTC"},
	}
	body, _ := json.Marshal(reqBody)

	req := httptest.NewRequest(http.MethodPost, "/query", bytes.NewReader(body))
	req.Header.Set("Content-Type", "application/json")
	w := httptest.NewRecorder()

	// We need to test the handler directly since we can't easily inject the mock handler
	// Let's create a proper test setup
	realHandler := NewQueryHandler(QueryHandlerConfig{
		Runner: &mockDockerRunner{
			startContainerFunc: func(ctx context.Context, exec model.Executable) (*ExecutionResult, error) {
				return &ExecutionResult{
					Status: "success",
					Output: `{"status":"ok","data":{"positions":[{"symbol":"BTC","amount":1.5}]}}`,
				}, nil
			},
		},
		Logger: &util.DefaultLogger{},
	})

	realServer := NewQueryServer(QueryServerConfig{
		Port:        9477,
		Handler:     realHandler,
		BotResolver: resolver,
		Logger:      &util.DefaultLogger{},
	})

	// Test the handler directly
	realServer.handleQuery(w, req)

	resp := w.Result()
	assert.Equal(t, http.StatusOK, resp.StatusCode)

	var result QueryResponse
	err := json.NewDecoder(resp.Body).Decode(&result)
	require.NoError(t, err)
	assert.Equal(t, "ok", result.Status)
}

func TestQueryServer_HandleQuery_BotNotFound(t *testing.T) {
	resolver := newMockBotResolver() // Empty resolver - no bots

	handler := NewQueryHandler(QueryHandlerConfig{
		Runner: &mockDockerRunner{},
		Logger: &util.DefaultLogger{},
	})

	server := NewQueryServer(QueryServerConfig{
		Port:        9477,
		Handler:     handler,
		BotResolver: resolver,
		Logger:      &util.DefaultLogger{},
	})

	reqBody := map[string]interface{}{
		"bot_id":     "nonexistent-bot",
		"query_path": "/test",
	}
	body, _ := json.Marshal(reqBody)

	req := httptest.NewRequest(http.MethodPost, "/query", bytes.NewReader(body))
	req.Header.Set("Content-Type", "application/json")
	w := httptest.NewRecorder()

	server.handleQuery(w, req)

	resp := w.Result()
	assert.Equal(t, http.StatusNotFound, resp.StatusCode)

	var result QueryResponse
	err := json.NewDecoder(resp.Body).Decode(&result)
	require.NoError(t, err)
	assert.Equal(t, "error", result.Status)
	assert.Contains(t, result.Error, "bot not found")
}

func TestQueryServer_HandleQuery_MissingBotID(t *testing.T) {
	resolver := newMockBotResolver()
	handler := NewQueryHandler(QueryHandlerConfig{
		Runner: &mockDockerRunner{},
		Logger: &util.DefaultLogger{},
	})

	server := NewQueryServer(QueryServerConfig{
		Port:        9477,
		Handler:     handler,
		BotResolver: resolver,
		Logger:      &util.DefaultLogger{},
	})

	reqBody := map[string]interface{}{
		"query_path": "/test",
		// Missing bot_id
	}
	body, _ := json.Marshal(reqBody)

	req := httptest.NewRequest(http.MethodPost, "/query", bytes.NewReader(body))
	req.Header.Set("Content-Type", "application/json")
	w := httptest.NewRecorder()

	server.handleQuery(w, req)

	resp := w.Result()
	assert.Equal(t, http.StatusBadRequest, resp.StatusCode)

	var result QueryResponse
	err := json.NewDecoder(resp.Body).Decode(&result)
	require.NoError(t, err)
	assert.Equal(t, "error", result.Status)
	assert.Contains(t, result.Error, "bot_id is required")
}

func TestQueryServer_HandleQuery_MissingQueryPath(t *testing.T) {
	resolver := newMockBotResolver()
	handler := NewQueryHandler(QueryHandlerConfig{
		Runner: &mockDockerRunner{},
		Logger: &util.DefaultLogger{},
	})

	server := NewQueryServer(QueryServerConfig{
		Port:        9477,
		Handler:     handler,
		BotResolver: resolver,
		Logger:      &util.DefaultLogger{},
	})

	reqBody := map[string]interface{}{
		"bot_id": "test-bot",
		// Missing query_path
	}
	body, _ := json.Marshal(reqBody)

	req := httptest.NewRequest(http.MethodPost, "/query", bytes.NewReader(body))
	req.Header.Set("Content-Type", "application/json")
	w := httptest.NewRecorder()

	server.handleQuery(w, req)

	resp := w.Result()
	assert.Equal(t, http.StatusBadRequest, resp.StatusCode)

	var result QueryResponse
	err := json.NewDecoder(resp.Body).Decode(&result)
	require.NoError(t, err)
	assert.Equal(t, "error", result.Status)
	assert.Contains(t, result.Error, "query_path is required")
}

func TestQueryServer_HandleQuery_InvalidMethod(t *testing.T) {
	resolver := newMockBotResolver()
	handler := NewQueryHandler(QueryHandlerConfig{
		Runner: &mockDockerRunner{},
		Logger: &util.DefaultLogger{},
	})

	server := NewQueryServer(QueryServerConfig{
		Port:        9477,
		Handler:     handler,
		BotResolver: resolver,
		Logger:      &util.DefaultLogger{},
	})

	req := httptest.NewRequest(http.MethodGet, "/query", nil)
	w := httptest.NewRecorder()

	server.handleQuery(w, req)

	resp := w.Result()
	assert.Equal(t, http.StatusMethodNotAllowed, resp.StatusCode)
}

func TestQueryServer_HandleHealth(t *testing.T) {
	resolver := newMockBotResolver()
	handler := NewQueryHandler(QueryHandlerConfig{
		Runner: &mockDockerRunner{},
		Logger: &util.DefaultLogger{},
	})

	server := NewQueryServer(QueryServerConfig{
		Port:        9477,
		Handler:     handler,
		BotResolver: resolver,
		Logger:      &util.DefaultLogger{},
	})

	req := httptest.NewRequest(http.MethodGet, "/health", nil)
	w := httptest.NewRecorder()

	server.handleHealth(w, req)

	resp := w.Result()
	assert.Equal(t, http.StatusOK, resp.StatusCode)

	var result map[string]string
	err := json.NewDecoder(resp.Body).Decode(&result)
	require.NoError(t, err)
	assert.Equal(t, "ok", result["status"])
}

func TestQueryServer_HandleQuery_InvalidJSON(t *testing.T) {
	resolver := newMockBotResolver()
	handler := NewQueryHandler(QueryHandlerConfig{
		Runner: &mockDockerRunner{},
		Logger: &util.DefaultLogger{},
	})

	server := NewQueryServer(QueryServerConfig{
		Port:        9477,
		Handler:     handler,
		BotResolver: resolver,
		Logger:      &util.DefaultLogger{},
	})

	req := httptest.NewRequest(http.MethodPost, "/query", bytes.NewReader([]byte("not valid json")))
	req.Header.Set("Content-Type", "application/json")
	w := httptest.NewRecorder()

	server.handleQuery(w, req)

	resp := w.Result()
	assert.Equal(t, http.StatusBadRequest, resp.StatusCode)

	var result QueryResponse
	err := json.NewDecoder(resp.Body).Decode(&result)
	require.NoError(t, err)
	assert.Equal(t, "error", result.Status)
	assert.Contains(t, result.Error, "invalid request body")
}

func TestQueryServer_BotToExecutable(t *testing.T) {
	resolver := newMockBotResolver()
	handler := NewQueryHandler(QueryHandlerConfig{
		Runner: &mockDockerRunner{},
		Logger: &util.DefaultLogger{},
	})

	server := NewQueryServer(QueryServerConfig{
		Port:        9477,
		Handler:     handler,
		BotResolver: resolver,
		Logger:      &util.DefaultLogger{},
	})

	// Bot with query entrypoint
	botWithQuery := &model.Bot{
		ID: "test-bot",
		CustomBotVersion: model.CustomBotVersion{
			Config: model.APIBotConfig{
				Runtime:     "python3.11",
				Entrypoints: map[string]string{"bot": "main.py", "query": "query.py"},
			},
			FilePath: "test-bot/v1.0.0/code.zip",
		},
		Config: map[string]interface{}{
			"symbol": "BTC",
			"amount": 100,
		},
	}

	// Test scheduled bot (not running)
	exec := server.botToExecutable(botWithQuery, false, "")
	require.NotNil(t, exec)
	assert.Equal(t, "test-bot", exec.ID)
	assert.Equal(t, "python3.11", exec.Runtime)
	assert.Equal(t, "query", exec.Entrypoint)
	assert.Equal(t, "main.py", exec.EntrypointFiles["bot"])
	assert.Equal(t, "query.py", exec.EntrypointFiles["query"])
	assert.Equal(t, "test-bot/v1.0.0/code.zip", exec.FilePath)
	assert.False(t, exec.IsLongRunning)
	assert.False(t, exec.PersistResults)

	// Test realtime bot (running)
	exec = server.botToExecutable(botWithQuery, true, "container-123")
	require.NotNil(t, exec)
	assert.True(t, exec.IsLongRunning)

	// Bot without query entrypoint should return nil
	botWithoutQuery := &model.Bot{
		ID: "no-query-bot",
		CustomBotVersion: model.CustomBotVersion{
			Config: model.APIBotConfig{
				Runtime:     "python3.11",
				Entrypoints: map[string]string{"bot": "main.py"},
			},
			FilePath: "no-query/v1.0.0/code.zip",
		},
	}
	exec = server.botToExecutable(botWithoutQuery, false, "")
	assert.Nil(t, exec)
}

func TestQueryServer_StartStop(t *testing.T) {
	resolver := newMockBotResolver()
	handler := NewQueryHandler(QueryHandlerConfig{
		Runner: &mockDockerRunner{},
		Logger: &util.DefaultLogger{},
	})

	// Use a random port to avoid conflicts
	server := NewQueryServer(QueryServerConfig{
		Port:        0, // Will use default 9477
		Handler:     handler,
		BotResolver: resolver,
		Logger:      &util.DefaultLogger{},
	})

	// Start server
	err := server.Start()
	require.NoError(t, err)

	// Give server time to start
	time.Sleep(100 * time.Millisecond)

	// Stop server
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	err = server.Stop(ctx)
	require.NoError(t, err)
}
