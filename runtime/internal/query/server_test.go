package query

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
)

// MockBotResolver is a mock implementation of BotResolver
type MockBotResolver struct {
	mock.Mock
}

func (m *MockBotResolver) ResolveBot(ctx context.Context, botID string) (string, error) {
	args := m.Called(ctx, botID)
	return args.String(0), args.Error(1)
}

// MockExecutor is a mock implementation of Executor
type MockExecutor struct {
	mock.Mock
}

func (m *MockExecutor) ExecuteQuery(ctx context.Context, req Request, targetIP string) *Response {
	args := m.Called(ctx, req, targetIP)
	return args.Get(0).(*Response)
}

func TestNewServer(t *testing.T) {
	resolver := &MockBotResolver{}
	executor := &MockExecutor{}

	server := NewServer(ServerConfig{
		Port:     9999,
		Resolver: resolver,
		Executor: executor,
	})

	assert.NotNil(t, server)
	assert.Equal(t, 9999, server.port)
	assert.NotNil(t, server.logger)
}

func TestNewServer_DefaultPort(t *testing.T) {
	server := NewServer(ServerConfig{})

	assert.Equal(t, 9477, server.port)
	assert.NotNil(t, server.logger)
}

func TestNewServer_DefaultLogger(t *testing.T) {
	server := NewServer(ServerConfig{
		Port: 9999,
	})

	assert.NotNil(t, server.logger)
}

func TestServer_HandleHealth(t *testing.T) {
	server := NewServer(ServerConfig{})

	req := httptest.NewRequest(http.MethodGet, "/health", nil)
	w := httptest.NewRecorder()

	server.HandleHealth(w, req)

	assert.Equal(t, http.StatusOK, w.Code)
	assert.Equal(t, "application/json", w.Header().Get("Content-Type"))

	var response map[string]string
	err := json.NewDecoder(w.Body).Decode(&response)
	assert.NoError(t, err)
	assert.Equal(t, "ok", response["status"])
}

func TestServer_HandleQuery_Success(t *testing.T) {
	resolver := &MockBotResolver{}
	executor := &MockExecutor{}

	resolver.On("ResolveBot", mock.Anything, "bot-123").
		Return("192.168.1.100", nil)

	executor.On("ExecuteQuery", mock.Anything, mock.MatchedBy(func(req Request) bool {
		return req.BotID == "bot-123" && req.QueryPath == "/price"
	}), "192.168.1.100").
		Return(&Response{
			Status:    "ok",
			Data:      json.RawMessage(`{"price":100}`),
			Timestamp: time.Now(),
		})

	server := NewServer(ServerConfig{
		Resolver: resolver,
		Executor: executor,
	})

	body := `{"bot_id":"bot-123","query_path":"/price"}`
	req := httptest.NewRequest(http.MethodPost, "/query", strings.NewReader(body))
	w := httptest.NewRecorder()

	server.HandleQuery(w, req)

	assert.Equal(t, http.StatusOK, w.Code)
	assert.Equal(t, "application/json", w.Header().Get("Content-Type"))

	var response Response
	err := json.NewDecoder(w.Body).Decode(&response)
	assert.NoError(t, err)
	assert.Equal(t, "ok", response.Status)

	resolver.AssertExpectations(t)
	executor.AssertExpectations(t)
}

func TestServer_HandleQuery_MissingBotID(t *testing.T) {
	server := NewServer(ServerConfig{})

	body := `{"query_path":"/price"}`
	req := httptest.NewRequest(http.MethodPost, "/query", strings.NewReader(body))
	w := httptest.NewRecorder()

	server.HandleQuery(w, req)

	assert.Equal(t, http.StatusBadRequest, w.Code)

	var response Response
	json.NewDecoder(w.Body).Decode(&response)
	assert.Equal(t, "error", response.Status)
	assert.Contains(t, response.Error, "bot_id is required")
}

func TestServer_HandleQuery_MissingQueryPath(t *testing.T) {
	server := NewServer(ServerConfig{})

	body := `{"bot_id":"bot-123"}`
	req := httptest.NewRequest(http.MethodPost, "/query", strings.NewReader(body))
	w := httptest.NewRecorder()

	server.HandleQuery(w, req)

	assert.Equal(t, http.StatusBadRequest, w.Code)

	var response Response
	json.NewDecoder(w.Body).Decode(&response)
	assert.Equal(t, "error", response.Status)
	assert.Contains(t, response.Error, "query_path is required")
}

func TestServer_HandleQuery_InvalidJSON(t *testing.T) {
	server := NewServer(ServerConfig{})

	body := `{invalid json}`
	req := httptest.NewRequest(http.MethodPost, "/query", strings.NewReader(body))
	w := httptest.NewRecorder()

	server.HandleQuery(w, req)

	assert.Equal(t, http.StatusBadRequest, w.Code)

	var response Response
	json.NewDecoder(w.Body).Decode(&response)
	assert.Equal(t, "error", response.Status)
	assert.Contains(t, response.Error, "invalid request body")
}

func TestServer_HandleQuery_MethodNotAllowed(t *testing.T) {
	server := NewServer(ServerConfig{})

	methods := []string{
		http.MethodGet,
		http.MethodPut,
		http.MethodDelete,
		http.MethodPatch,
	}

	for _, method := range methods {
		t.Run(method, func(t *testing.T) {
			req := httptest.NewRequest(method, "/query", nil)
			w := httptest.NewRecorder()

			server.HandleQuery(w, req)

			assert.Equal(t, http.StatusMethodNotAllowed, w.Code)

			var response Response
			json.NewDecoder(w.Body).Decode(&response)
			assert.Equal(t, "error", response.Status)
			assert.Contains(t, response.Error, "method not allowed")
		})
	}
}

func TestServer_HandleQuery_BotNotFound(t *testing.T) {
	resolver := &MockBotResolver{}
	resolver.On("ResolveBot", mock.Anything, "bot-999").
		Return("", errors.New("bot not found"))

	server := NewServer(ServerConfig{
		Resolver: resolver,
	})

	body := `{"bot_id":"bot-999","query_path":"/price"}`
	req := httptest.NewRequest(http.MethodPost, "/query", strings.NewReader(body))
	w := httptest.NewRecorder()

	server.HandleQuery(w, req)

	assert.Equal(t, http.StatusNotFound, w.Code)

	var response Response
	json.NewDecoder(w.Body).Decode(&response)
	assert.Equal(t, "error", response.Status)
	assert.Contains(t, response.Error, "bot not found")

	resolver.AssertExpectations(t)
}

func TestServer_HandleQuery_ExecutorError(t *testing.T) {
	resolver := &MockBotResolver{}
	executor := &MockExecutor{}

	resolver.On("ResolveBot", mock.Anything, "bot-123").
		Return("192.168.1.100", nil)

	executor.On("ExecuteQuery", mock.Anything, mock.Anything, "192.168.1.100").
		Return(&Response{
			Status:    "error",
			Error:     "execution failed",
			Timestamp: time.Now(),
		})

	server := NewServer(ServerConfig{
		Resolver: resolver,
		Executor: executor,
	})

	body := `{"bot_id":"bot-123","query_path":"/price"}`
	req := httptest.NewRequest(http.MethodPost, "/query", strings.NewReader(body))
	w := httptest.NewRecorder()

	server.HandleQuery(w, req)

	assert.Equal(t, http.StatusInternalServerError, w.Code)

	var response Response
	json.NewDecoder(w.Body).Decode(&response)
	assert.Equal(t, "error", response.Status)
	assert.Equal(t, "execution failed", response.Error)

	resolver.AssertExpectations(t)
	executor.AssertExpectations(t)
}

func TestServer_HandleQuery_PathNormalization(t *testing.T) {
	resolver := &MockBotResolver{}
	executor := &MockExecutor{}

	resolver.On("ResolveBot", mock.Anything, "bot-123").
		Return("192.168.1.100", nil)

	// Executor should receive normalized path with leading slash
	executor.On("ExecuteQuery", mock.Anything, mock.MatchedBy(func(req Request) bool {
		return req.QueryPath == "/price" // Normalized with leading slash
	}), "192.168.1.100").
		Return(&Response{
			Status:    "ok",
			Timestamp: time.Now(),
		})

	server := NewServer(ServerConfig{
		Resolver: resolver,
		Executor: executor,
	})

	// Send path without leading slash
	body := `{"bot_id":"bot-123","query_path":"price"}`
	req := httptest.NewRequest(http.MethodPost, "/query", strings.NewReader(body))
	w := httptest.NewRecorder()

	server.HandleQuery(w, req)

	assert.Equal(t, http.StatusOK, w.Code)

	executor.AssertExpectations(t)
}

func TestServer_HandleQuery_WithParams(t *testing.T) {
	resolver := &MockBotResolver{}
	executor := &MockExecutor{}

	resolver.On("ResolveBot", mock.Anything, "bot-123").
		Return("192.168.1.100", nil)

	executor.On("ExecuteQuery", mock.Anything, mock.MatchedBy(func(req Request) bool {
		return req.Params != nil && req.Params["symbol"] == "BTC"
	}), "192.168.1.100").
		Return(&Response{
			Status:    "ok",
			Timestamp: time.Now(),
		})

	server := NewServer(ServerConfig{
		Resolver: resolver,
		Executor: executor,
	})

	body := `{"bot_id":"bot-123","query_path":"/price","params":{"symbol":"BTC"}}`
	req := httptest.NewRequest(http.MethodPost, "/query", strings.NewReader(body))
	w := httptest.NewRecorder()

	server.HandleQuery(w, req)

	assert.Equal(t, http.StatusOK, w.Code)

	executor.AssertExpectations(t)
}

func TestServer_HandleQuery_WithTimeout(t *testing.T) {
	resolver := &MockBotResolver{}
	executor := &MockExecutor{}

	resolver.On("ResolveBot", mock.Anything, "bot-123").
		Return("192.168.1.100", nil)

	executor.On("ExecuteQuery", mock.Anything, mock.MatchedBy(func(req Request) bool {
		return req.TimeoutSec == 60
	}), "192.168.1.100").
		Return(&Response{
			Status:    "ok",
			Timestamp: time.Now(),
		})

	server := NewServer(ServerConfig{
		Resolver: resolver,
		Executor: executor,
	})

	body := `{"bot_id":"bot-123","query_path":"/price","timeout_sec":60}`
	req := httptest.NewRequest(http.MethodPost, "/query", strings.NewReader(body))
	w := httptest.NewRecorder()

	server.HandleQuery(w, req)

	assert.Equal(t, http.StatusOK, w.Code)

	executor.AssertExpectations(t)
}

func TestServer_StartAndStop(t *testing.T) {
	server := NewServer(ServerConfig{
		Port: 19999, // Use unique port
	})

	err := server.Start()
	assert.NoError(t, err)

	// Give it a moment to start
	time.Sleep(50 * time.Millisecond)

	// Make a request to verify it's running
	resp, err := http.Get("http://localhost:19999/health")
	assert.NoError(t, err)
	assert.Equal(t, http.StatusOK, resp.StatusCode)
	resp.Body.Close()

	// Stop the server
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
	defer cancel()

	err = server.Stop(ctx)
	assert.NoError(t, err)

	// Give it a moment to stop
	time.Sleep(50 * time.Millisecond)

	// Verify server is stopped
	_, err = http.Get("http://localhost:19999/health")
	assert.Error(t, err)
}

func TestServer_StopWithoutStart(t *testing.T) {
	server := NewServer(ServerConfig{})

	ctx, cancel := context.WithTimeout(context.Background(), 1*time.Second)
	defer cancel()

	err := server.Stop(ctx)
	assert.NoError(t, err) // Should not error when stopping unstarted server
}

func TestServer_SendError(t *testing.T) {
	server := NewServer(ServerConfig{})

	tests := []struct {
		name           string
		status         int
		message        string
		expectedStatus int
	}{
		{
			name:           "bad request",
			status:         http.StatusBadRequest,
			message:        "invalid input",
			expectedStatus: http.StatusBadRequest,
		},
		{
			name:           "not found",
			status:         http.StatusNotFound,
			message:        "resource not found",
			expectedStatus: http.StatusNotFound,
		},
		{
			name:           "internal error",
			status:         http.StatusInternalServerError,
			message:        "internal server error",
			expectedStatus: http.StatusInternalServerError,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			w := httptest.NewRecorder()

			server.sendError(w, tt.status, tt.message)

			assert.Equal(t, tt.expectedStatus, w.Code)
			assert.Equal(t, "application/json", w.Header().Get("Content-Type"))

			var response Response
			err := json.NewDecoder(w.Body).Decode(&response)
			assert.NoError(t, err)
			assert.Equal(t, "error", response.Status)
			assert.Equal(t, tt.message, response.Error)
			assert.NotZero(t, response.Timestamp)
		})
	}
}

func TestServer_HandleQuery_EmptyBody(t *testing.T) {
	server := NewServer(ServerConfig{})

	req := httptest.NewRequest(http.MethodPost, "/query", bytes.NewReader([]byte{}))
	w := httptest.NewRecorder()

	server.HandleQuery(w, req)

	assert.Equal(t, http.StatusBadRequest, w.Code)

	var response Response
	json.NewDecoder(w.Body).Decode(&response)
	assert.Equal(t, "error", response.Status)
}

func TestServer_HandleQuery_ResponseContainsData(t *testing.T) {
	resolver := &MockBotResolver{}
	executor := &MockExecutor{}

	resolver.On("ResolveBot", mock.Anything, "bot-123").
		Return("192.168.1.100", nil)

	testData := map[string]interface{}{
		"price":  100.5,
		"volume": 1000,
	}
	dataJSON, _ := json.Marshal(testData)

	executor.On("ExecuteQuery", mock.Anything, mock.Anything, "192.168.1.100").
		Return(&Response{
			Status:    "ok",
			Data:      json.RawMessage(dataJSON),
			Timestamp: time.Now(),
		})

	server := NewServer(ServerConfig{
		Resolver: resolver,
		Executor: executor,
	})

	body := `{"bot_id":"bot-123","query_path":"/data"}`
	req := httptest.NewRequest(http.MethodPost, "/query", strings.NewReader(body))
	w := httptest.NewRecorder()

	server.HandleQuery(w, req)

	assert.Equal(t, http.StatusOK, w.Code)

	var response Response
	err := json.NewDecoder(w.Body).Decode(&response)
	assert.NoError(t, err)
	assert.Equal(t, "ok", response.Status)
	assert.NotNil(t, response.Data)

	// Verify data can be unmarshaled
	var data map[string]interface{}
	err = json.Unmarshal(response.Data, &data)
	assert.NoError(t, err)
	assert.Equal(t, 100.5, data["price"])
	assert.Equal(t, float64(1000), data["volume"])
}

// Benchmark tests
func BenchmarkServer_HandleHealth(b *testing.B) {
	server := NewServer(ServerConfig{})
	req := httptest.NewRequest(http.MethodGet, "/health", nil)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		w := httptest.NewRecorder()
		server.HandleHealth(w, req)
	}
}

func BenchmarkServer_HandleQuery_Success(b *testing.B) {
	resolver := &MockBotResolver{}
	executor := &MockExecutor{}

	resolver.On("ResolveBot", mock.Anything, mock.Anything).
		Return("192.168.1.100", nil)

	executor.On("ExecuteQuery", mock.Anything, mock.Anything, mock.Anything).
		Return(&Response{
			Status:    "ok",
			Timestamp: time.Now(),
		})

	server := NewServer(ServerConfig{
		Resolver: resolver,
		Executor: executor,
	})

	body := `{"bot_id":"bot-123","query_path":"/price"}`
	req := httptest.NewRequest(http.MethodPost, "/query", strings.NewReader(body))

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		w := httptest.NewRecorder()
		server.HandleQuery(w, req)
	}
}
