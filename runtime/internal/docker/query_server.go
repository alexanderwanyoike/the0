// Package docker provides an HTTP server for bot query execution.
package docker

import (
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"runtime/internal/model"
	"runtime/internal/util"
	"time"
)

// QueryServer provides an HTTP API for executing queries against bots.
type QueryServer struct {
	server      *http.Server
	handler     *QueryHandler
	botResolver BotResolver
	logger      util.Logger
	port        int
}

// BotResolver resolves a bot ID to its executable configuration.
type BotResolver interface {
	// GetBot returns the bot configuration for a given bot ID.
	GetBot(ctx context.Context, botID string) (*model.Bot, error)
	// GetContainerID returns the container ID for a running bot.
	GetContainerID(ctx context.Context, botID string) (string, bool)
}

// QueryServerConfig contains configuration for the QueryServer.
type QueryServerConfig struct {
	Port        int
	Handler     *QueryHandler
	BotResolver BotResolver
	Logger      util.Logger
}

// NewQueryServer creates a new QueryServer instance.
func NewQueryServer(config QueryServerConfig) *QueryServer {
	if config.Port == 0 {
		config.Port = 9477 // Default query server port
	}
	if config.Logger == nil {
		config.Logger = &util.DefaultLogger{}
	}

	return &QueryServer{
		handler:     config.Handler,
		botResolver: config.BotResolver,
		logger:      config.Logger,
		port:        config.Port,
	}
}

// Start begins serving HTTP requests.
func (s *QueryServer) Start() error {
	mux := http.NewServeMux()
	mux.HandleFunc("/query", s.handleQuery)
	mux.HandleFunc("/health", s.handleHealth)

	s.server = &http.Server{
		Addr:         fmt.Sprintf(":%d", s.port),
		Handler:      mux,
		ReadTimeout:  60 * time.Second,
		WriteTimeout: 60 * time.Second,
	}

	s.logger.Info("Query server starting on port %d", s.port)

	go func() {
		if err := s.server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			s.logger.Error("Query server error: %v", err)
		}
	}()

	return nil
}

// Stop gracefully shuts down the server.
func (s *QueryServer) Stop(ctx context.Context) error {
	if s.server != nil {
		return s.server.Shutdown(ctx)
	}
	return nil
}

// handleHealth handles health check requests.
func (s *QueryServer) handleHealth(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusOK)
	json.NewEncoder(w).Encode(map[string]string{"status": "ok"})
}

// handleQuery handles query execution requests.
func (s *QueryServer) handleQuery(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		s.sendError(w, http.StatusMethodNotAllowed, "method not allowed")
		return
	}

	// Parse request body
	var req struct {
		BotID      string                 `json:"bot_id"`
		QueryPath  string                 `json:"query_path"`
		Params     map[string]interface{} `json:"params,omitempty"`
		TimeoutSec int                    `json:"timeout_sec,omitempty"`
	}

	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		s.sendError(w, http.StatusBadRequest, fmt.Sprintf("invalid request body: %v", err))
		return
	}

	// Validate required fields
	if req.BotID == "" {
		s.sendError(w, http.StatusBadRequest, "bot_id is required")
		return
	}
	if req.QueryPath == "" {
		s.sendError(w, http.StatusBadRequest, "query_path is required")
		return
	}

	// Resolve the bot
	bot, err := s.botResolver.GetBot(r.Context(), req.BotID)
	if err != nil {
		s.sendError(w, http.StatusNotFound, fmt.Sprintf("bot not found: %s", req.BotID))
		return
	}

	// Check if bot is running (for realtime queries)
	containerID, isRunning := s.botResolver.GetContainerID(r.Context(), req.BotID)

	// Convert bot to executable
	executable := s.botToExecutable(bot, isRunning, containerID)
	if executable == nil {
		s.sendError(w, http.StatusBadRequest, fmt.Sprintf("bot %s does not have a query entrypoint defined", req.BotID))
		return
	}

	// Execute the query
	queryReq := QueryRequest{
		BotID:      req.BotID,
		QueryPath:  req.QueryPath,
		Params:     req.Params,
		TimeoutSec: req.TimeoutSec,
	}

	response, err := s.handler.ExecuteQuery(r.Context(), queryReq, *executable, containerID)
	if err != nil {
		s.sendError(w, http.StatusInternalServerError, fmt.Sprintf("query execution failed: %v", err))
		return
	}

	// Send response
	w.Header().Set("Content-Type", "application/json")
	if response.Status == "error" {
		w.WriteHeader(http.StatusInternalServerError)
	} else {
		w.WriteHeader(http.StatusOK)
	}
	json.NewEncoder(w).Encode(response)
}

// sendError sends an error response.
func (s *QueryServer) sendError(w http.ResponseWriter, status int, message string) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(status)
	json.NewEncoder(w).Encode(QueryResponse{
		Status:    "error",
		Error:     message,
		Timestamp: time.Now(),
	})
}

// botToExecutable converts a Bot model to an Executable for query execution.
// Returns nil if the bot has no query entrypoint defined.
func (s *QueryServer) botToExecutable(bot *model.Bot, isRunning bool, containerID string) *model.Executable {
	if bot.CustomBotVersion.Config.Entrypoints == nil {
		return nil
	}

	// Check if query entrypoint exists
	queryEntrypoint, hasQuery := bot.CustomBotVersion.Config.Entrypoints["query"]
	if !hasQuery {
		return nil
	}

	entrypointFiles := make(map[string]string)
	for k, v := range bot.CustomBotVersion.Config.Entrypoints {
		entrypointFiles[k] = v
	}

	// Use queryEntrypoint to avoid unused variable warning
	_ = queryEntrypoint

	return &model.Executable{
		ID:              bot.ID,
		Runtime:         bot.CustomBotVersion.Config.Runtime,
		Entrypoint:      "query",
		EntrypointFiles: entrypointFiles,
		Config:          bot.Config,
		FilePath:        bot.CustomBotVersion.FilePath,
		IsLongRunning:   isRunning,
		PersistResults:  false,
	}
}
