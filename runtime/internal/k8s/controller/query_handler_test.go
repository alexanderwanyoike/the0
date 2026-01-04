package controller

import (
	"context"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"runtime/internal/model"
	"runtime/internal/util"
	"strconv"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestK8sQueryHandler_ExecuteRealtimeQuery(t *testing.T) {
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

	// Extract host and port from mock server URL (e.g., "127.0.0.1:12345")
	serverAddr := strings.TrimPrefix(mockServer.URL, "http://")
	parts := strings.Split(serverAddr, ":")
	host := parts[0]
	port, _ := strconv.Atoi(parts[1])

	handler := NewK8sQueryHandler(K8sQueryHandlerConfig{
		Namespace: "the0",
		Logger:    &util.DefaultLogger{},
		QueryPort: port, // Use mock server's port
	})

	request := QueryRequest{
		BotID:      "test-realtime-bot",
		QueryPath:  "/portfolio",
		TimeoutSec: 5,
	}

	ctx := context.Background()
	response, err := handler.ExecuteQuery(ctx, request, model.Bot{}, host)

	require.NoError(t, err)
	require.NotNil(t, response)
	assert.Equal(t, "ok", response.Status)
	assert.NotNil(t, response.Data)
}

func TestK8sQueryHandler_ExecuteRealtimeQuery_WithParams(t *testing.T) {
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

	handler := NewK8sQueryHandler(K8sQueryHandlerConfig{
		Namespace: "the0",
		Logger:    &util.DefaultLogger{},
		QueryPort: port,
	})

	request := QueryRequest{
		BotID:      "test-bot",
		QueryPath:  "/test",
		Params:     map[string]interface{}{"symbol": "BTC", "amount": 100},
		TimeoutSec: 5,
	}

	ctx := context.Background()
	response, err := handler.ExecuteQuery(ctx, request, model.Bot{}, host)

	require.NoError(t, err)
	require.NotNil(t, response)
	assert.Equal(t, "ok", response.Status)
	assert.Equal(t, "BTC", receivedParams["symbol"])
	assert.Equal(t, float64(100), receivedParams["amount"])
}

func TestK8sQueryHandler_ExecuteRealtimeQuery_Error(t *testing.T) {
	handler := NewK8sQueryHandler(K8sQueryHandlerConfig{
		Namespace: "the0",
		QueryPort: 99999, // Invalid port
		Logger:    &util.DefaultLogger{},
	})

	request := QueryRequest{
		BotID:      "test-bot",
		QueryPath:  "/test",
		TimeoutSec: 1,
	}

	ctx := context.Background()
	// Use an invalid IP that should fail fast
	response, err := handler.ExecuteQuery(ctx, request, model.Bot{}, "127.0.0.1:99999")

	require.NoError(t, err) // Error is in response, not returned
	require.NotNil(t, response)
	assert.Equal(t, "error", response.Status)
	assert.Contains(t, response.Error, "failed to execute query request")
}
