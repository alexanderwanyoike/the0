// Package health provides HTTP health check endpoints for Kubernetes probes.
package health

import (
	"context"
	"fmt"
	"net/http"
	"sync/atomic"
	"time"

	"runtime/internal/util"
)

// Server provides HTTP health check endpoints for Kubernetes liveness and readiness probes.
type Server struct {
	server  *http.Server
	ready   atomic.Bool
	healthy atomic.Bool
}

// NewServer creates a new health server listening on the specified port.
func NewServer(port int) *Server {
	s := &Server{}
	s.healthy.Store(true) // Start healthy

	mux := http.NewServeMux()
	mux.HandleFunc("/healthz", s.healthzHandler)
	mux.HandleFunc("/readyz", s.readyzHandler)

	s.server = &http.Server{
		Addr:         fmt.Sprintf(":%d", port),
		Handler:      mux,
		ReadTimeout:  5 * time.Second,
		WriteTimeout: 5 * time.Second,
	}

	return s
}

// Start starts the health server in a goroutine.
func (s *Server) Start() {
	go func() {
		util.LogMaster("Health server listening on %s", s.server.Addr)
		if err := s.server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			util.LogMaster("Health server error: %v", err)
		}
	}()
}

// Stop gracefully shuts down the health server.
func (s *Server) Stop(ctx context.Context) error {
	return s.server.Shutdown(ctx)
}

// SetReady marks the server as ready to receive traffic.
func (s *Server) SetReady(ready bool) {
	s.ready.Store(ready)
}

// SetHealthy marks the server as healthy/unhealthy.
func (s *Server) SetHealthy(healthy bool) {
	s.healthy.Store(healthy)
}

// healthzHandler handles liveness probe requests.
// Returns 200 if healthy, 503 if unhealthy.
func (s *Server) healthzHandler(w http.ResponseWriter, r *http.Request) {
	if s.healthy.Load() {
		w.WriteHeader(http.StatusOK)
		w.Write([]byte("ok"))
	} else {
		w.WriteHeader(http.StatusServiceUnavailable)
		w.Write([]byte("unhealthy"))
	}
}

// readyzHandler handles readiness probe requests.
// Returns 200 if ready, 503 if not ready.
func (s *Server) readyzHandler(w http.ResponseWriter, r *http.Request) {
	if s.ready.Load() {
		w.WriteHeader(http.StatusOK)
		w.Write([]byte("ok"))
	} else {
		w.WriteHeader(http.StatusServiceUnavailable)
		w.Write([]byte("not ready"))
	}
}
