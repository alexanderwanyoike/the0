// Package query provides an HTTP server for bot query execution.
package query

import (
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"time"

	"runtime/internal/util"
)

// BotResolver resolves bot information for query execution.
type BotResolver interface {
	// ResolveBot returns the bot info and target IP for a query.
	// Returns targetIP="" for scheduled bots (need ephemeral execution).
	// Returns error if bot not found or doesn't support queries.
	ResolveBot(ctx context.Context, botID string) (targetIP string, err error)
}

// Executor executes queries against bots.
type Executor interface {
	// ExecuteQuery executes a query and returns the response.
	ExecuteQuery(ctx context.Context, req Request, targetIP string) *Response
}

// Server provides an HTTP API for executing queries against bots.
type Server struct {
	server   *http.Server
	resolver BotResolver
	executor Executor
	logger   util.Logger
	port     int
}

// ServerConfig contains configuration for the Server.
type ServerConfig struct {
	Port     int
	Resolver BotResolver
	Executor Executor
	Logger   util.Logger
}

// NewServer creates a new query Server instance.
func NewServer(config ServerConfig) *Server {
	if config.Port == 0 {
		config.Port = 9477 // Default query server port
	}
	if config.Logger == nil {
		config.Logger = &util.DefaultLogger{}
	}

	return &Server{
		resolver: config.Resolver,
		executor: config.Executor,
		logger:   config.Logger,
		port:     config.Port,
	}
}

// Start begins serving HTTP requests.
func (s *Server) Start() error {
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
func (s *Server) Stop(ctx context.Context) error {
	if s.server != nil {
		return s.server.Shutdown(ctx)
	}
	return nil
}

// handleHealth handles health check requests.
func (s *Server) handleHealth(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusOK)
	json.NewEncoder(w).Encode(map[string]string{"status": "ok"})
}

// handleQuery handles query execution requests.
func (s *Server) handleQuery(w http.ResponseWriter, r *http.Request) {
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

	// Normalize query path to ensure leading slash
	if req.QueryPath[0] != '/' {
		req.QueryPath = "/" + req.QueryPath
	}

	// Resolve the bot
	targetIP, err := s.resolver.ResolveBot(r.Context(), req.BotID)
	if err != nil {
		s.sendError(w, http.StatusNotFound, err.Error())
		return
	}

	// Execute the query
	queryReq := Request{
		BotID:      req.BotID,
		QueryPath:  req.QueryPath,
		Params:     req.Params,
		TimeoutSec: req.TimeoutSec,
	}

	response := s.executor.ExecuteQuery(r.Context(), queryReq, targetIP)

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
func (s *Server) sendError(w http.ResponseWriter, status int, message string) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(status)
	json.NewEncoder(w).Encode(Response{
		Status:    "error",
		Error:     message,
		Timestamp: time.Now(),
	})
}

// HandleQuery is exposed for direct testing without HTTP.
func (s *Server) HandleQuery(w http.ResponseWriter, r *http.Request) {
	s.handleQuery(w, r)
}

// HandleHealth is exposed for direct testing without HTTP.
func (s *Server) HandleHealth(w http.ResponseWriter, r *http.Request) {
	s.handleHealth(w, r)
}
