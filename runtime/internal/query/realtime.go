// Package query provides shared utilities for bot query execution.
package query

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"time"

	"runtime/internal/util"
)

// RealtimeExecutor executes queries against running bot instances via HTTP.
type RealtimeExecutor struct {
	queryPort int
	logger    util.Logger
}

// NewRealtimeExecutor creates a new RealtimeExecutor.
func NewRealtimeExecutor(queryPort int, logger util.Logger) *RealtimeExecutor {
	if queryPort == 0 {
		queryPort = DefaultQueryPort
	}
	if logger == nil {
		logger = &util.DefaultLogger{}
	}
	return &RealtimeExecutor{
		queryPort: queryPort,
		logger:    logger,
	}
}

// Execute sends a query to a running bot's HTTP query server.
// targetIP is the IP address of the container/pod running the bot.
func (e *RealtimeExecutor) Execute(ctx context.Context, req Request, targetIP string) *Response {
	start := time.Now()

	e.logger.Info("Executing realtime query: bot=%s path=%s ip=%s", req.BotID, req.QueryPath, targetIP)

	// Build the request URL
	url := fmt.Sprintf("http://%s:%d%s", targetIP, e.queryPort, req.QueryPath)

	// Prepare request body with params
	var reqBody io.Reader
	if len(req.Params) > 0 {
		paramsJSON, err := json.Marshal(req.Params)
		if err != nil {
			return ErrorResponse(fmt.Sprintf("failed to marshal query params: %v", err), start)
		}
		reqBody = bytes.NewReader(paramsJSON)
	}

	// Create HTTP request
	httpReq, err := http.NewRequestWithContext(ctx, http.MethodPost, url, reqBody)
	if err != nil {
		return ErrorResponse(fmt.Sprintf("failed to create HTTP request: %v", err), start)
	}
	httpReq.Header.Set("Content-Type", "application/json")

	// Execute request
	timeout := time.Duration(req.TimeoutSec) * time.Second
	if timeout == 0 {
		timeout = DefaultTimeout * time.Second
	}
	client := &http.Client{Timeout: timeout}

	resp, err := client.Do(httpReq)
	if err != nil {
		return ErrorResponse(fmt.Sprintf("failed to execute query request: %v", err), start)
	}
	defer resp.Body.Close()

	// Read response body
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return ErrorResponse(fmt.Sprintf("failed to read query response: %v", err), start)
	}

	// Parse response JSON
	return parseQueryOutput(body, start)
}

// parseQueryOutput parses the JSON output from a query execution.
func parseQueryOutput(data []byte, start time.Time) *Response {
	var output struct {
		Status string          `json:"status"`
		Data   json.RawMessage `json:"data"`
		Error  string          `json:"error"`
	}

	if err := json.Unmarshal(data, &output); err != nil {
		return ErrorResponse(fmt.Sprintf("failed to parse query response: %v (body: %s)", err, string(data)), start)
	}

	return &Response{
		Status:    output.Status,
		Data:      output.Data,
		Error:     output.Error,
		Duration:  time.Since(start),
		Timestamp: time.Now(),
	}
}

// ParseQueryOutput parses JSON output from a query execution (exported for scheduled executors).
func ParseQueryOutput(data []byte, start time.Time) *Response {
	return parseQueryOutput(data, start)
}
