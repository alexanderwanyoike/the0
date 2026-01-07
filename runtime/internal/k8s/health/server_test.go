package health

import (
	"context"
	"net/http"
	"net/http/httptest"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestNewServer(t *testing.T) {
	server := NewServer(8080)

	assert.NotNil(t, server)
	assert.NotNil(t, server.server)
	assert.Equal(t, ":8080", server.server.Addr)

	// Server should start healthy but not ready
	assert.True(t, server.healthy.Load(), "Server should start healthy")
	assert.False(t, server.ready.Load(), "Server should start not ready")

	// Verify timeouts are set
	assert.Equal(t, 5*time.Second, server.server.ReadTimeout)
	assert.Equal(t, 5*time.Second, server.server.WriteTimeout)
}

func TestNewServer_DifferentPorts(t *testing.T) {
	tests := []struct {
		name         string
		port         int
		expectedAddr string
	}{
		{"standard port", 8080, ":8080"},
		{"high port", 9090, ":9090"},
		{"low port", 3000, ":3000"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			server := NewServer(tt.port)
			assert.Equal(t, tt.expectedAddr, server.server.Addr)
		})
	}
}

func TestServer_SetReady(t *testing.T) {
	server := NewServer(8080)

	// Initially not ready
	assert.False(t, server.ready.Load())

	// Set ready
	server.SetReady(true)
	assert.True(t, server.ready.Load())

	// Set not ready
	server.SetReady(false)
	assert.False(t, server.ready.Load())
}

func TestServer_SetHealthy(t *testing.T) {
	server := NewServer(8080)

	// Initially healthy
	assert.True(t, server.healthy.Load())

	// Set unhealthy
	server.SetHealthy(false)
	assert.False(t, server.healthy.Load())

	// Set healthy again
	server.SetHealthy(true)
	assert.True(t, server.healthy.Load())
}

func TestServer_HealthzHandler_Healthy(t *testing.T) {
	server := NewServer(8080)
	server.SetHealthy(true)

	req := httptest.NewRequest(http.MethodGet, "/healthz", nil)
	w := httptest.NewRecorder()

	server.healthzHandler(w, req)

	assert.Equal(t, http.StatusOK, w.Code)
	assert.Equal(t, "ok", w.Body.String())
}

func TestServer_HealthzHandler_Unhealthy(t *testing.T) {
	server := NewServer(8080)
	server.SetHealthy(false)

	req := httptest.NewRequest(http.MethodGet, "/healthz", nil)
	w := httptest.NewRecorder()

	server.healthzHandler(w, req)

	assert.Equal(t, http.StatusServiceUnavailable, w.Code)
	assert.Equal(t, "unhealthy", w.Body.String())
}

func TestServer_ReadyzHandler_Ready(t *testing.T) {
	server := NewServer(8080)
	server.SetReady(true)

	req := httptest.NewRequest(http.MethodGet, "/readyz", nil)
	w := httptest.NewRecorder()

	server.readyzHandler(w, req)

	assert.Equal(t, http.StatusOK, w.Code)
	assert.Equal(t, "ok", w.Body.String())
}

func TestServer_ReadyzHandler_NotReady(t *testing.T) {
	server := NewServer(8080)
	server.SetReady(false)

	req := httptest.NewRequest(http.MethodGet, "/readyz", nil)
	w := httptest.NewRecorder()

	server.readyzHandler(w, req)

	assert.Equal(t, http.StatusServiceUnavailable, w.Code)
	assert.Equal(t, "not ready", w.Body.String())
}

func TestServer_StateTransitions(t *testing.T) {
	server := NewServer(8080)

	// Test multiple state transitions
	states := []struct {
		ready   bool
		healthy bool
	}{
		{false, true},  // Not ready but healthy (starting up)
		{true, true},   // Ready and healthy (normal operation)
		{true, false},  // Ready but unhealthy (degraded)
		{false, false}, // Not ready and unhealthy (shutting down)
		{true, true},   // Back to normal
	}

	for _, state := range states {
		server.SetReady(state.ready)
		server.SetHealthy(state.healthy)

		assert.Equal(t, state.ready, server.ready.Load())
		assert.Equal(t, state.healthy, server.healthy.Load())
	}
}

func TestServer_ConcurrentAccess(t *testing.T) {
	server := NewServer(8080)

	// Test concurrent reads and writes to ensure atomic operations work
	done := make(chan bool)
	iterations := 100

	// Writer goroutine
	go func() {
		for i := 0; i < iterations; i++ {
			server.SetReady(i%2 == 0)
			server.SetHealthy(i%3 == 0)
		}
		done <- true
	}()

	// Reader goroutine
	go func() {
		for i := 0; i < iterations; i++ {
			_ = server.ready.Load()
			_ = server.healthy.Load()
		}
		done <- true
	}()

	// Wait for both goroutines
	<-done
	<-done

	// No assertion needed - if there's a race condition, go test -race will catch it
}

func TestServer_HTTPMethods(t *testing.T) {
	server := NewServer(8080)
	server.SetReady(true)
	server.SetHealthy(true)

	methods := []string{
		http.MethodGet,
		http.MethodPost,
		http.MethodPut,
		http.MethodDelete,
		http.MethodHead,
	}

	for _, method := range methods {
		t.Run("healthz_"+method, func(t *testing.T) {
			req := httptest.NewRequest(method, "/healthz", nil)
			w := httptest.NewRecorder()

			server.healthzHandler(w, req)

			// All methods should work (handler doesn't check method)
			assert.Equal(t, http.StatusOK, w.Code)
		})

		t.Run("readyz_"+method, func(t *testing.T) {
			req := httptest.NewRequest(method, "/readyz", nil)
			w := httptest.NewRecorder()

			server.readyzHandler(w, req)

			// All methods should work (handler doesn't check method)
			assert.Equal(t, http.StatusOK, w.Code)
		})
	}
}

func TestServer_StartAndStop(t *testing.T) {
	// Use a unique port for this test to avoid conflicts
	server := NewServer(18080)

	// Start the server
	server.Start()

	// Give it a moment to start
	time.Sleep(50 * time.Millisecond)

	// Make a real HTTP request to verify it's running
	resp, err := http.Get("http://localhost:18080/healthz")
	require.NoError(t, err)
	assert.Equal(t, http.StatusOK, resp.StatusCode)
	resp.Body.Close()

	// Stop the server
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
	defer cancel()

	err = server.Stop(ctx)
	assert.NoError(t, err)

	// Give it a moment to stop
	time.Sleep(50 * time.Millisecond)

	// Verify server is stopped (connection should fail)
	_, err = http.Get("http://localhost:18080/healthz")
	assert.Error(t, err, "Expected error when connecting to stopped server")
}

func TestServer_StopWithoutStart(t *testing.T) {
	server := NewServer(28080)

	// Try to stop a server that was never started
	ctx, cancel := context.WithTimeout(context.Background(), 1*time.Second)
	defer cancel()

	// Should not panic
	assert.NotPanics(t, func() {
		_ = server.Stop(ctx)
	})
}

func TestServer_StopWithCancelledContext(t *testing.T) {
	server := NewServer(38080)
	server.Start()

	// Give it a moment to start
	time.Sleep(50 * time.Millisecond)

	// Create a context and cancel it immediately
	ctx, cancel := context.WithCancel(context.Background())
	cancel() // Cancel immediately

	err := server.Stop(ctx)
	// Might get context cancelled error or nil if shutdown completes quickly
	// Both are acceptable - just verify it doesn't panic
	_ = err
	assert.NotPanics(t, func() {
		_ = server.Stop(ctx)
	})
}

func TestServer_ResponseHeaders(t *testing.T) {
	server := NewServer(8080)
	server.SetHealthy(true)
	server.SetReady(true)

	tests := []struct {
		name     string
		endpoint string
		handler  func(http.ResponseWriter, *http.Request)
	}{
		{
			name:     "healthz",
			endpoint: "/healthz",
			handler:  server.healthzHandler,
		},
		{
			name:     "readyz",
			endpoint: "/readyz",
			handler:  server.readyzHandler,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			req := httptest.NewRequest(http.MethodGet, tt.endpoint, nil)
			w := httptest.NewRecorder()

			tt.handler(w, req)

			// Verify response has expected format
			assert.Equal(t, http.StatusOK, w.Code)
			assert.NotEmpty(t, w.Body.String())
		})
	}
}

func TestServer_DefaultState(t *testing.T) {
	server := NewServer(8080)

	// Test default state immediately after creation
	req := httptest.NewRequest(http.MethodGet, "/healthz", nil)
	w := httptest.NewRecorder()
	server.healthzHandler(w, req)
	assert.Equal(t, http.StatusOK, w.Code, "Should be healthy by default")

	req = httptest.NewRequest(http.MethodGet, "/readyz", nil)
	w = httptest.NewRecorder()
	server.readyzHandler(w, req)
	assert.Equal(t, http.StatusServiceUnavailable, w.Code, "Should not be ready by default")
}

// Benchmark tests
func BenchmarkServer_HealthzHandler(b *testing.B) {
	server := NewServer(8080)
	server.SetHealthy(true)

	req := httptest.NewRequest(http.MethodGet, "/healthz", nil)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		w := httptest.NewRecorder()
		server.healthzHandler(w, req)
	}
}

func BenchmarkServer_ReadyzHandler(b *testing.B) {
	server := NewServer(8080)
	server.SetReady(true)

	req := httptest.NewRequest(http.MethodGet, "/readyz", nil)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		w := httptest.NewRecorder()
		server.readyzHandler(w, req)
	}
}

func BenchmarkServer_SetReady(b *testing.B) {
	server := NewServer(8080)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		server.SetReady(i%2 == 0)
	}
}

func BenchmarkServer_SetHealthy(b *testing.B) {
	server := NewServer(8080)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		server.SetHealthy(i%2 == 0)
	}
}
