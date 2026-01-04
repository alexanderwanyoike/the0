// Package controller provides an HTTP server for bot query execution in Kubernetes.
package controller

import (
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"time"

	corev1 "k8s.io/api/core/v1"
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"
	"k8s.io/client-go/kubernetes"

	"runtime/internal/k8s/podgen"
	"runtime/internal/model"
	"runtime/internal/util"
)

// K8sQueryServer provides an HTTP API for executing queries against bot pods.
type K8sQueryServer struct {
	server       *http.Server
	handler      *K8sQueryHandler
	botRepo      BotRepository
	clientset    *kubernetes.Clientset
	namespace    string
	logger       util.Logger
	port         int
}

// K8sQueryServerConfig contains configuration for the K8sQueryServer.
type K8sQueryServerConfig struct {
	Port         int
	Handler      *K8sQueryHandler
	BotRepo      BotRepository
	Clientset    *kubernetes.Clientset
	Namespace    string
	Logger       util.Logger
}

// NewK8sQueryServer creates a new K8sQueryServer instance.
func NewK8sQueryServer(config K8sQueryServerConfig) *K8sQueryServer {
	if config.Port == 0 {
		config.Port = 9477 // Default query server port
	}
	if config.Logger == nil {
		config.Logger = &util.DefaultLogger{}
	}
	if config.Namespace == "" {
		config.Namespace = "the0"
	}

	return &K8sQueryServer{
		handler:   config.Handler,
		botRepo:   config.BotRepo,
		clientset: config.Clientset,
		namespace: config.Namespace,
		logger:    config.Logger,
		port:      config.Port,
	}
}

// Start begins serving HTTP requests.
func (s *K8sQueryServer) Start() error {
	mux := http.NewServeMux()
	mux.HandleFunc("/query", s.handleQuery)
	mux.HandleFunc("/health", s.handleHealth)

	s.server = &http.Server{
		Addr:         fmt.Sprintf(":%d", s.port),
		Handler:      mux,
		ReadTimeout:  60 * time.Second,
		WriteTimeout: 60 * time.Second,
	}

	s.logger.Info("K8s Query server starting on port %d", s.port)

	go func() {
		if err := s.server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			s.logger.Error("K8s Query server error: %v", err)
		}
	}()

	return nil
}

// Stop gracefully shuts down the server.
func (s *K8sQueryServer) Stop(ctx context.Context) error {
	if s.server != nil {
		return s.server.Shutdown(ctx)
	}
	return nil
}

// handleHealth handles health check requests.
func (s *K8sQueryServer) handleHealth(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusOK)
	json.NewEncoder(w).Encode(map[string]string{"status": "ok"})
}

// handleQuery handles query execution requests.
func (s *K8sQueryServer) handleQuery(w http.ResponseWriter, r *http.Request) {
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

	// Get bot from repository
	bots, err := s.botRepo.FindAllEnabled(r.Context())
	if err != nil {
		s.sendError(w, http.StatusInternalServerError, fmt.Sprintf("failed to query bots: %v", err))
		return
	}

	// Find the specific bot
	var bot *model.Bot
	for i := range bots {
		if bots[i].ID == req.BotID {
			bot = &bots[i]
			break
		}
	}

	if bot == nil {
		s.sendError(w, http.StatusNotFound, fmt.Sprintf("bot not found: %s", req.BotID))
		return
	}

	// Check if bot has a running pod (for realtime queries)
	podIP := s.getBotPodIP(r.Context(), req.BotID)

	// Execute the query
	queryReq := QueryRequest{
		BotID:      req.BotID,
		QueryPath:  req.QueryPath,
		Params:     req.Params,
		TimeoutSec: req.TimeoutSec,
	}

	response, err := s.handler.ExecuteQuery(r.Context(), queryReq, *bot, podIP)
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

// getBotPodIP returns the IP address of the running pod for a bot, or empty string if not found.
func (s *K8sQueryServer) getBotPodIP(ctx context.Context, botID string) string {
	// List pods with the bot ID label
	pods, err := s.clientset.CoreV1().Pods(s.namespace).List(ctx, metav1.ListOptions{
		LabelSelector: fmt.Sprintf("%s=%s", podgen.LabelBotID, botID),
	})
	if err != nil {
		s.logger.Error("Failed to list pods for bot %s: %v", botID, err)
		return ""
	}

	// Find a running pod
	for _, pod := range pods.Items {
		if pod.Status.Phase == corev1.PodRunning && pod.Status.PodIP != "" {
			return pod.Status.PodIP
		}
	}

	return ""
}

// sendError sends an error response.
func (s *K8sQueryServer) sendError(w http.ResponseWriter, status int, message string) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(status)
	json.NewEncoder(w).Encode(QueryResponse{
		Status:    "error",
		Error:     message,
		Timestamp: time.Now(),
	})
}
