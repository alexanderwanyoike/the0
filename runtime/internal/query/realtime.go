// Package query provides shared utilities for bot query execution.
package query

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"strings"
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
	if err := addParamsToQuery(httpReq.URL, req.Params); err != nil {
		return ErrorResponse(fmt.Sprintf("failed to add query params to URL: %v", err), start)
	}

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

// addParamsToQuery puts params in the URL query string as well as the JSON
// body, because SDK query servers read params only from the query string.
// A param replaces a key of the same name already in the path, since SDKs
// disagree on which of a repeated key wins. A null param stays out, because
// handlers fall back to a default for a missing key and the string "null"
// would defeat that.
func addParamsToQuery(u *url.URL, params map[string]interface{}) error {
	if len(params) == 0 {
		return nil
	}
	query, err := url.ParseQuery(u.RawQuery)
	if err != nil {
		return fmt.Errorf("invalid query string in path: %w", err)
	}
	for key, value := range params {
		if value == nil {
			continue
		}
		text, err := paramText(value)
		if err != nil {
			return fmt.Errorf("param %q: %w", key, err)
		}
		query.Set(key, text)
	}
	u.RawQuery = query.Encode()
	return nil
}

// paramText renders a param as SDKs expect to read it from a query string:
// a string as itself (JSON would wrap it in quotes) and anything else as its
// JSON text.
func paramText(value interface{}) (string, error) {
	if text, ok := value.(string); ok {
		return text, nil
	}
	encoded, err := json.Marshal(value)
	if err != nil {
		return "", err
	}
	return string(encoded), nil
}

// parseQueryOutput parses the JSON output from a query execution.
func parseQueryOutput(data []byte, start time.Time) *Response {
	var output struct {
		Status    string          `json:"status"`
		Data      json.RawMessage `json:"data"`
		Error     string          `json:"error"`
		Available []string        `json:"available"`
	}

	if err := json.Unmarshal(data, &output); err != nil {
		return ErrorResponse(fmt.Sprintf("failed to parse query response: %v (body: %s)", err, string(data)), start)
	}

	if output.Error != "" && len(output.Available) > 0 {
		output.Error = fmt.Sprintf("%s (available: %s)", output.Error, strings.Join(output.Available, ", "))
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
