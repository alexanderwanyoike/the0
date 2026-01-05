// Package docker provides query execution for bot containers.
package docker

import (
	"context"
	"fmt"
	"time"

	"runtime/internal/model"
	"runtime/internal/query"
	"runtime/internal/util"
)

// QueryHandler executes queries against bot containers.
// For scheduled bots, it spawns ephemeral containers with QUERY_PATH set.
// For realtime bots, it proxies to the bot's query HTTP server on port 9476.
type QueryHandler struct {
	runner           DockerRunner
	realtimeExecutor *query.RealtimeExecutor
	logger           util.Logger
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
		runner:           config.Runner,
		realtimeExecutor: query.NewRealtimeExecutor(query.DefaultQueryPort, config.Logger),
		logger:           config.Logger,
	}
}

// ExecuteQuery executes a query against a bot.
// For scheduled bots: Spawns an ephemeral container with QUERY_PATH and QUERY_PARAMS set.
// For realtime bots: Proxies the request to the bot's query HTTP server.
// containerID is required for realtime bots (empty string for scheduled bots).
func (h *QueryHandler) ExecuteQuery(ctx context.Context, req query.Request, executable model.Executable, containerID string) (*query.Response, error) {
	start := time.Now()

	// Set defaults
	if req.TimeoutSec <= 0 {
		req.TimeoutSec = query.DefaultTimeout
	}

	// Create timeout context
	queryCtx, cancel := context.WithTimeout(ctx, time.Duration(req.TimeoutSec)*time.Second)
	defer cancel()

	// For realtime bots, proxy to the running container's query server
	if executable.IsLongRunning {
		return h.executeRealtimeQuery(queryCtx, req, containerID, start)
	}

	// For scheduled bots, spawn an ephemeral container
	return h.executeScheduledQuery(queryCtx, req, executable, start)
}

// executeScheduledQuery spawns an ephemeral container to execute the query.
func (h *QueryHandler) executeScheduledQuery(ctx context.Context, req query.Request, executable model.Executable, start time.Time) (*query.Response, error) {
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
		return query.ErrorResponse(fmt.Sprintf("failed to execute query container: %v", err), start), err
	}

	// Parse the result from the result file
	var resultData []byte
	if len(result.ResultFileContents) > 0 {
		resultData = result.ResultFileContents
	} else {
		// Fallback to stdout if no result file
		resultData = []byte(result.Output)
	}

	return query.ParseQueryOutput(resultData, start), nil
}

// executeRealtimeQuery proxies the query to the bot's HTTP server.
func (h *QueryHandler) executeRealtimeQuery(ctx context.Context, req query.Request, containerID string, start time.Time) (*query.Response, error) {
	// Get the container's IP address
	containerIP, err := h.runner.GetContainerIP(ctx, containerID)
	if err != nil {
		return query.ErrorResponse(fmt.Sprintf("failed to get container IP: %v", err), start), nil
	}

	return h.realtimeExecutor.Execute(ctx, req, containerIP), nil
}
