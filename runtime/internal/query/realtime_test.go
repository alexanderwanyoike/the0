package query

import (
	"context"
	"encoding/json"
	"io"
	"net/http"
	"net/http/httptest"
	"net/url"
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

func TestParseQueryOutput_UnknownPathListsAvailablePaths(t *testing.T) {
	response := ParseQueryOutput(
		[]byte(`{"status":"error","error":"No handler for path: /status","available":["/today","/history"]}`),
		time.Now(),
	)

	assert.Equal(t, "error", response.Status)
	assert.Equal(t, "No handler for path: /status (available: /today, /history)", response.Error)
}

// receivedQuery is what a bot's query server saw of one realtime query.
type receivedQuery struct {
	requestURI string
	path       string
	params     map[string]string
	body       string
}

// queryBotServer runs a query against a stand-in for a bot's query server
// that, like every SDK in server mode, reads params only from the URL query
// string into a map of strings.
func queryBotServer(t *testing.T, queryPath string, params map[string]interface{}) (*Response, receivedQuery) {
	t.Helper()

	received := make(chan receivedQuery, 1)
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		params := map[string]string{}
		for key, values := range r.URL.Query() {
			params[key] = values[0]
		}
		body, err := io.ReadAll(r.Body)
		if err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}
		received <- receivedQuery{requestURI: r.RequestURI, path: r.URL.Path, params: params, body: string(body)}

		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(map[string]interface{}{"status": "ok", "data": params})
	}))
	t.Cleanup(server.Close)

	serverURL, err := url.Parse(server.URL)
	require.NoError(t, err)
	port, err := strconv.Atoi(serverURL.Port())
	require.NoError(t, err)

	executor := NewRealtimeExecutor(port, &util.DefaultLogger{})
	response := executor.Execute(context.Background(), Request{
		BotID:      "test-bot",
		QueryPath:  queryPath,
		Params:     params,
		TimeoutSec: 5,
	}, serverURL.Hostname())
	require.NotNil(t, response)

	select {
	case query := <-received:
		return response, query
	default:
		return response, receivedQuery{}
	}
}

func TestRealtimeExecutor_ParamsReachBotAsURLQueryString(t *testing.T) {
	tests := []struct {
		name     string
		params   map[string]interface{}
		expected map[string]string
	}{
		{
			name:     "string arrives as is",
			params:   map[string]interface{}{"symbol": "BTC"},
			expected: map[string]string{"symbol": "BTC"},
		},
		{
			name:     "whole number arrives as plain text",
			params:   map[string]interface{}{"limit": float64(100)},
			expected: map[string]string{"limit": "100"},
		},
		{
			name:     "fractional number arrives as plain text",
			params:   map[string]interface{}{"threshold": 0.25},
			expected: map[string]string{"threshold": "0.25"},
		},
		{
			name:     "booleans arrive as plain text",
			params:   map[string]interface{}{"live": true, "verbose": false},
			expected: map[string]string{"live": "true", "verbose": "false"},
		},
		{
			name: "object arrives as JSON text",
			params: map[string]interface{}{
				"range": map[string]interface{}{"from": "2026-01-01", "to": "2026-02-01"},
			},
			expected: map[string]string{"range": `{"from":"2026-01-01","to":"2026-02-01"}`},
		},
		{
			name:     "array arrives as JSON text",
			params:   map[string]interface{}{"symbols": []interface{}{"BTC", "ETH"}},
			expected: map[string]string{"symbols": `["BTC","ETH"]`},
		},
		{
			name:     "reserved and non-ASCII characters survive encoding",
			params:   map[string]interface{}{"note & key": "a&b=c d+e/f?g#h%20 €"},
			expected: map[string]string{"note & key": "a&b=c d+e/f?g#h%20 €"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			response, received := queryBotServer(t, "/portfolio", tt.params)

			assert.Equal(t, "ok", response.Status, response.Error)
			assert.Equal(t, "/portfolio", received.path)
			assert.Equal(t, tt.expected, received.params)
		})
	}
}

func TestRealtimeExecutor_NoParamsLeavesQueryStringOut(t *testing.T) {
	for name, params := range map[string]map[string]interface{}{
		"nil params":   nil,
		"empty params": {},
	} {
		t.Run(name, func(t *testing.T) {
			response, received := queryBotServer(t, "/portfolio", params)

			assert.Equal(t, "ok", response.Status, response.Error)
			assert.Equal(t, "/portfolio", received.requestURI)
		})
	}
}

func TestRealtimeExecutor_NullParamsStayOutOfQueryString(t *testing.T) {
	t.Run("null param is left out alongside other params", func(t *testing.T) {
		response, received := queryBotServer(t, "/portfolio", map[string]interface{}{
			"cursor": nil,
			"symbol": "BTC",
		})

		assert.Equal(t, "ok", response.Status, response.Error)
		assert.Equal(t, map[string]string{"symbol": "BTC"}, received.params)
		assert.JSONEq(t, `{"cursor":null,"symbol":"BTC"}`, received.body)
	})

	t.Run("only null params add no query string", func(t *testing.T) {
		response, received := queryBotServer(t, "/portfolio", map[string]interface{}{"cursor": nil})

		assert.Equal(t, "ok", response.Status, response.Error)
		assert.Equal(t, "/portfolio", received.requestURI)
		assert.JSONEq(t, `{"cursor":null}`, received.body)
	})
}

func TestRealtimeExecutor_QueryStringInPathCombinesWithParams(t *testing.T) {
	t.Run("path query string passes through untouched without params", func(t *testing.T) {
		response, received := queryBotServer(t, "/history?limit=5&sort=desc", nil)

		assert.Equal(t, "ok", response.Status, response.Error)
		assert.Equal(t, "/history?limit=5&sort=desc", received.requestURI)
	})

	t.Run("params join the path query string and win on the same key", func(t *testing.T) {
		response, received := queryBotServer(t, "/history?limit=5&sort=desc", map[string]interface{}{
			"symbol": "BTC",
			"limit":  float64(10),
		})

		assert.Equal(t, "ok", response.Status, response.Error)
		assert.Equal(t, "/history", received.path)
		assert.Equal(t, map[string]string{"limit": "10", "sort": "desc", "symbol": "BTC"}, received.params)

		_, rawQuery, _ := strings.Cut(received.requestURI, "?")
		sent, err := url.ParseQuery(rawQuery)
		require.NoError(t, err)
		assert.Len(t, sent["limit"], 1, "SDKs disagree on which of a repeated key wins")
	})

	t.Run("malformed path query string is rejected rather than partly dropped", func(t *testing.T) {
		response, received := queryBotServer(t, "/history?limit=%zz", map[string]interface{}{"symbol": "BTC"})

		assert.Equal(t, "error", response.Status)
		assert.Contains(t, response.Error, "query string")
		assert.Empty(t, received.requestURI, "the bot should not have been queried")
	})
}
