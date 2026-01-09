package query

import (
	"context"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"strconv"
	"strings"
	"testing"
	"time"

	"runtime/internal/util"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestRealtimeExecutor_Execute(t *testing.T) {
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
	serverAddr := strings.TrimPrefix(mockServer.URL, "http://")
	parts := strings.Split(serverAddr, ":")
	host := parts[0]
	port, _ := strconv.Atoi(parts[1])

	executor := NewRealtimeExecutor(port, &util.DefaultLogger{})

	request := Request{
		BotID:      "test-bot",
		QueryPath:  "/portfolio",
		TimeoutSec: 5,
	}

	ctx := context.Background()
	response := executor.Execute(ctx, request, host)

	require.NotNil(t, response)
	assert.Equal(t, "ok", response.Status)
	assert.NotNil(t, response.Data)
	assert.True(t, response.Duration > 0)
	assert.False(t, response.Timestamp.IsZero())
}

func TestRealtimeExecutor_ExecuteWithParams(t *testing.T) {
	var receivedParams map[string]interface{}

	mockServer := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		json.NewDecoder(r.Body).Decode(&receivedParams)

		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(map[string]interface{}{
			"status": "ok",
			"data":   receivedParams,
		})
	}))
	defer mockServer.Close()

	serverAddr := strings.TrimPrefix(mockServer.URL, "http://")
	parts := strings.Split(serverAddr, ":")
	host := parts[0]
	port, _ := strconv.Atoi(parts[1])

	executor := NewRealtimeExecutor(port, &util.DefaultLogger{})

	request := Request{
		BotID:      "test-bot",
		QueryPath:  "/test",
		Params:     map[string]interface{}{"symbol": "BTC", "amount": 100},
		TimeoutSec: 5,
	}

	ctx := context.Background()
	response := executor.Execute(ctx, request, host)

	require.NotNil(t, response)
	assert.Equal(t, "ok", response.Status)
	assert.Equal(t, "BTC", receivedParams["symbol"])
	assert.Equal(t, float64(100), receivedParams["amount"])
}

func TestRealtimeExecutor_ExecuteError(t *testing.T) {
	executor := NewRealtimeExecutor(99999, &util.DefaultLogger{})

	request := Request{
		BotID:      "test-bot",
		QueryPath:  "/test",
		TimeoutSec: 1,
	}

	ctx := context.Background()
	response := executor.Execute(ctx, request, "127.0.0.1")

	require.NotNil(t, response)
	assert.Equal(t, "error", response.Status)
	assert.Contains(t, response.Error, "failed to execute query request")
}

func TestRealtimeExecutor_InvalidResponse(t *testing.T) {
	mockServer := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		w.Write([]byte("not valid json"))
	}))
	defer mockServer.Close()

	serverAddr := strings.TrimPrefix(mockServer.URL, "http://")
	parts := strings.Split(serverAddr, ":")
	host := parts[0]
	port, _ := strconv.Atoi(parts[1])

	executor := NewRealtimeExecutor(port, &util.DefaultLogger{})

	request := Request{
		BotID:      "test-bot",
		QueryPath:  "/test",
		TimeoutSec: 5,
	}

	ctx := context.Background()
	response := executor.Execute(ctx, request, host)

	require.NotNil(t, response)
	assert.Equal(t, "error", response.Status)
	assert.Contains(t, response.Error, "failed to parse query response")
}

func TestRealtimeExecutor_DefaultPort(t *testing.T) {
	executor := NewRealtimeExecutor(0, nil)
	assert.Equal(t, DefaultQueryPort, executor.queryPort)
	assert.NotNil(t, executor.logger)
}

func TestParseQueryOutput(t *testing.T) {
	tests := []struct {
		name           string
		input          string
		expectedStatus string
		expectError    bool
	}{
		{
			name:           "valid response",
			input:          `{"status":"ok","data":{"value":123}}`,
			expectedStatus: "ok",
			expectError:    false,
		},
		{
			name:           "error response",
			input:          `{"status":"error","error":"something went wrong"}`,
			expectedStatus: "error",
			expectError:    false,
		},
		{
			name:           "invalid json",
			input:          "not json",
			expectedStatus: "error",
			expectError:    true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			response := ParseQueryOutput([]byte(tt.input), time.Now())

			assert.Equal(t, tt.expectedStatus, response.Status)
			if tt.expectError {
				assert.Contains(t, response.Error, "failed to parse query response")
			}
		})
	}
}
