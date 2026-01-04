// Package docker provides query execution for bot containers.
package docker

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"runtime/internal/model"
	"runtime/internal/util"
	"time"
)

// QueryRequest represents a query to execute against a bot.
type QueryRequest struct {
	BotID      string                 `json:"bot_id"`
	QueryPath  string                 `json:"query_path"`
	Params     map[string]interface{} `json:"params,omitempty"`
	TimeoutSec int                    `json:"timeout_sec,omitempty"` // Default: 30 seconds
}

// QueryResponse represents the result of a query execution.
type QueryResponse struct {
	Status    string          `json:"status"`              // "ok" or "error"
	Data      json.RawMessage `json:"data,omitempty"`      // Query result data
	Error     string          `json:"error,omitempty"`     // Error message if status is "error"
	Duration  time.Duration   `json:"duration"`            // Execution time
	Timestamp time.Time       `json:"timestamp"`           // When the query was executed
}

// QueryHandler executes queries against bot containers.
// For scheduled bots, it spawns ephemeral containers with QUERY_PATH set.
// For realtime bots, it proxies to the bot's query HTTP server on port 9476.
type QueryHandler struct {
	runner DockerRunner
	logger util.Logger
}

// QueryHandlerConfig contains configuration for the QueryHandler.
type QueryHandlerConfig struct {
	Runner DockerRunner
	Logger util.Logger
}

// NewQueryHandler creates a new QueryHandler instance.
func NewQueryHandler(config QueryHandlerConfig) *QueryHandler {
	if config.Logger == nil {
		config.Logger = &util.DefaultLogger{}
	}
	return &QueryHandler{
		runner: config.Runner,
		logger: config.Logger,
	}
}

// ExecuteQuery executes a query against a bot.
// For scheduled bots: Spawns an ephemeral container with QUERY_PATH and QUERY_PARAMS set.
// For realtime bots: Proxies the request to the bot's query HTTP server.
// containerID is required for realtime bots (empty string for scheduled bots).
func (h *QueryHandler) ExecuteQuery(ctx context.Context, req QueryRequest, executable model.Executable, containerID string) (*QueryResponse, error) {
	start := time.Now()

	// Set defaults
	if req.TimeoutSec <= 0 {
		req.TimeoutSec = 30
	}

	// Create timeout context
	queryCtx, cancel := context.WithTimeout(ctx, time.Duration(req.TimeoutSec)*time.Second)
	defer cancel()

	// For realtime bots, proxy to the running container's query server
	if executable.IsLongRunning {
		return h.executeRealtimeQuery(queryCtx, req, executable, containerID, start)
	}

	// For scheduled bots, spawn an ephemeral container
	return h.executeScheduledQuery(queryCtx, req, executable, start)
}

// executeScheduledQuery spawns an ephemeral container to execute the query.
func (h *QueryHandler) executeScheduledQuery(ctx context.Context, req QueryRequest, executable model.Executable, start time.Time) (*QueryResponse, error) {
	// Configure executable for query mode
	queryExecutable := executable
	queryExecutable.Entrypoint = "query"
	queryExecutable.QueryPath = req.QueryPath
	queryExecutable.QueryParams = req.Params
	queryExecutable.IsLongRunning = false
	queryExecutable.PersistResults = false
	queryExecutable.ResultFilePath = "/query/result.json"

	// Add query entrypoint file if not present
	if queryExecutable.EntrypointFiles == nil {
		queryExecutable.EntrypointFiles = make(map[string]string)
	}
	// The query entrypoint uses the same file as the bot entrypoint
	// The SDK detects QUERY_PATH and runs query mode instead of bot mode
	if _, ok := queryExecutable.EntrypointFiles["query"]; !ok {
		queryExecutable.EntrypointFiles["query"] = queryExecutable.EntrypointFiles["bot"]
	}

	h.logger.Info("Executing scheduled query: bot=%s path=%s", req.BotID, req.QueryPath)

	// Execute the container
	result, err := h.runner.StartContainer(ctx, queryExecutable)
	if err != nil {
		return &QueryResponse{
			Status:    "error",
			Error:     fmt.Sprintf("failed to execute query container: %v", err),
			Duration:  time.Since(start),
			Timestamp: time.Now(),
		}, err
	}

	// Parse the result from the result file (result.ResultFileContents)
	response := &QueryResponse{
		Duration:  time.Since(start),
		Timestamp: time.Now(),
	}

	// The query result should be in result.json file
	var resultData []byte
	if len(result.ResultFileContents) > 0 {
		resultData = result.ResultFileContents
	} else {
		// Fallback to stdout if no result file (shouldn't happen with proper SDK)
		resultData = []byte(result.Output)
	}

	var output struct {
		Status string          `json:"status"`
		Data   json.RawMessage `json:"data"`
		Error  string          `json:"error"`
	}

	if err := json.Unmarshal(resultData, &output); err != nil {
		response.Status = "error"
		response.Error = fmt.Sprintf("failed to parse query response: %v (result: %s)", err, string(resultData))
		return response, nil
	}

	response.Status = output.Status
	response.Data = output.Data
	response.Error = output.Error

	return response, nil
}

// executeRealtimeQuery proxies the query to the bot's HTTP server.
func (h *QueryHandler) executeRealtimeQuery(ctx context.Context, req QueryRequest, executable model.Executable, containerID string, start time.Time) (*QueryResponse, error) {
	h.logger.Info("Executing realtime query: bot=%s path=%s container=%s", req.BotID, req.QueryPath, containerID)

	// Get the container's IP address
	containerIP, err := h.runner.GetContainerIP(ctx, containerID)
	if err != nil {
		return &QueryResponse{
			Status:    "error",
			Error:     fmt.Sprintf("failed to get container IP: %v", err),
			Duration:  time.Since(start),
			Timestamp: time.Now(),
		}, nil
	}

	// Build the request URL (SDK query server runs on port 9476)
	url := fmt.Sprintf("http://%s:9476%s", containerIP, req.QueryPath)

	// Prepare request body with params
	var reqBody io.Reader
	if len(req.Params) > 0 {
		paramsJSON, err := json.Marshal(req.Params)
		if err != nil {
			return &QueryResponse{
				Status:    "error",
				Error:     fmt.Sprintf("failed to marshal query params: %v", err),
				Duration:  time.Since(start),
				Timestamp: time.Now(),
			}, nil
		}
		reqBody = bytes.NewReader(paramsJSON)
	}

	// Create HTTP request
	httpReq, err := http.NewRequestWithContext(ctx, http.MethodPost, url, reqBody)
	if err != nil {
		return &QueryResponse{
			Status:    "error",
			Error:     fmt.Sprintf("failed to create HTTP request: %v", err),
			Duration:  time.Since(start),
			Timestamp: time.Now(),
		}, nil
	}
	httpReq.Header.Set("Content-Type", "application/json")

	// Execute request with a short timeout
	client := &http.Client{
		Timeout: time.Duration(req.TimeoutSec) * time.Second,
	}
	resp, err := client.Do(httpReq)
	if err != nil {
		return &QueryResponse{
			Status:    "error",
			Error:     fmt.Sprintf("failed to execute query request: %v", err),
			Duration:  time.Since(start),
			Timestamp: time.Now(),
		}, nil
	}
	defer resp.Body.Close()

	// Read response body
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return &QueryResponse{
			Status:    "error",
			Error:     fmt.Sprintf("failed to read query response: %v", err),
			Duration:  time.Since(start),
			Timestamp: time.Now(),
		}, nil
	}

	// Parse response JSON
	var output struct {
		Status string          `json:"status"`
		Data   json.RawMessage `json:"data"`
		Error  string          `json:"error"`
	}

	if err := json.Unmarshal(body, &output); err != nil {
		return &QueryResponse{
			Status:    "error",
			Error:     fmt.Sprintf("failed to parse query response: %v (body: %s)", err, string(body)),
			Duration:  time.Since(start),
			Timestamp: time.Now(),
		}, nil
	}

	return &QueryResponse{
		Status:    output.Status,
		Data:      output.Data,
		Error:     output.Error,
		Duration:  time.Since(start),
		Timestamp: time.Now(),
	}, nil
}
