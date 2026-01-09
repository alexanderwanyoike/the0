package daemon

import (
	"context"
	"fmt"
	"net/http"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"runtime/internal/util"
)

func TestNewSyncRunner(t *testing.T) {
	t.Run("creates runner with defaults", func(t *testing.T) {
		runner := NewSyncRunner(SyncOptions{
			BotID: "test-bot",
		}, nil)
		require.NotNil(t, runner)
		assert.NotNil(t, runner.logger)
		assert.NotNil(t, runner.ready)
		assert.Equal(t, "test-bot", runner.opts.BotID)
	})

	t.Run("creates runner with custom logger", func(t *testing.T) {
		logger := &util.DefaultLogger{}
		runner := NewSyncRunner(SyncOptions{
			BotID:     "test-bot",
			StatePath: "/custom/state",
			LogsPath:  "/custom/logs",
			Interval:  30 * time.Second,
		}, logger)
		require.NotNil(t, runner)
		assert.Equal(t, logger, runner.logger)
		assert.Equal(t, "/custom/state", runner.opts.StatePath)
		assert.Equal(t, "/custom/logs", runner.opts.LogsPath)
		assert.Equal(t, 30*time.Second, runner.opts.Interval)
	})
}

func TestSyncRunner_WaitReady_Timeout(t *testing.T) {
	// Create a runner but don't start it - should timeout
	runner := NewSyncRunner(SyncOptions{
		BotID: "test-bot",
	}, nil)

	// WaitReady should timeout since we didn't start the runner
	err := runner.WaitReady(100 * time.Millisecond)
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "timeout")
}

func TestSyncRunner_StartAndStop(t *testing.T) {
	t.Run("stop without start does not block", func(t *testing.T) {
		runner := NewSyncRunner(SyncOptions{
			BotID: "test-bot",
		}, nil)

		// Stop without start should not block
		done := make(chan struct{})
		go func() {
			runner.Stop()
			close(done)
		}()

		select {
		case <-done:
			// Success
		case <-time.After(2 * time.Second):
			t.Fatal("Stop() blocked when Start() was never called")
		}
	})
}

func TestSyncRunner_DoSync_ThreadSafe(t *testing.T) {
	runner := NewSyncRunner(SyncOptions{
		BotID: "test-bot",
	}, nil)

	// DoSync should not panic even when syncers is empty
	assert.NotPanics(t, func() {
		runner.DoSync()
	})
}

// mockSyncerImpl implements the Syncer interface for testing
type mockSyncerImpl struct {
	syncCount int
}

func (m *mockSyncerImpl) Sync(ctx context.Context) bool {
	m.syncCount++
	return true
}

func TestSyncRunner_DoSync_CallsSyncers(t *testing.T) {
	runner := NewSyncRunner(SyncOptions{
		BotID: "test-bot",
	}, nil)

	// Add mock syncer directly (bypassing normal initialization)
	mockSyncer := &mockSyncerImpl{syncCount: 0}
	runner.syncers = []Syncer{mockSyncer}

	// Call DoSync
	runner.DoSync()

	assert.Equal(t, 1, mockSyncer.syncCount)

	// Call again
	runner.DoSync()
	assert.Equal(t, 2, mockSyncer.syncCount)
}

func TestSyncOptions_ReadinessPort(t *testing.T) {
	t.Run("default is 0 (disabled)", func(t *testing.T) {
		opts := SyncOptions{
			BotID: "test-bot",
		}
		assert.Equal(t, 0, opts.ReadinessPort)
	})

	t.Run("can be set to custom port", func(t *testing.T) {
		opts := SyncOptions{
			BotID:         "test-bot",
			ReadinessPort: 8079,
		}
		assert.Equal(t, 8079, opts.ReadinessPort)
	})
}

func TestSyncHealthServer(t *testing.T) {
	t.Run("health server starts and serves readiness endpoint", func(t *testing.T) {
		// Use a random high port to avoid conflicts
		port := 19876

		// Start health server directly (simulating what Sync() does)
		healthServer := startTestHealthServer(t, port)
		defer func() {
			ctx, cancel := context.WithTimeout(context.Background(), 1*time.Second)
			defer cancel()
			healthServer.Stop(ctx)
		}()

		// Give server time to start
		time.Sleep(50 * time.Millisecond)

		// Initially should not be ready
		resp, err := http.Get(fmt.Sprintf("http://localhost:%d/readyz", port))
		require.NoError(t, err)
		defer resp.Body.Close()
		assert.Equal(t, http.StatusServiceUnavailable, resp.StatusCode)

		// Set ready
		healthServer.SetReady(true)

		// Now should be ready
		resp2, err := http.Get(fmt.Sprintf("http://localhost:%d/readyz", port))
		require.NoError(t, err)
		defer resp2.Body.Close()
		assert.Equal(t, http.StatusOK, resp2.StatusCode)
	})

	t.Run("health server serves healthz endpoint", func(t *testing.T) {
		port := 19877

		healthServer := startTestHealthServer(t, port)
		defer func() {
			ctx, cancel := context.WithTimeout(context.Background(), 1*time.Second)
			defer cancel()
			healthServer.Stop(ctx)
		}()

		time.Sleep(50 * time.Millisecond)

		// Healthz should always be OK (server starts healthy)
		resp, err := http.Get(fmt.Sprintf("http://localhost:%d/healthz", port))
		require.NoError(t, err)
		defer resp.Body.Close()
		assert.Equal(t, http.StatusOK, resp.StatusCode)
	})
}

// startTestHealthServer creates and starts a health server for testing
func startTestHealthServer(t *testing.T, port int) *testHealthServer {
	t.Helper()

	server := &testHealthServer{
		ready:   false,
		healthy: true,
	}

	mux := http.NewServeMux()
	mux.HandleFunc("/readyz", func(w http.ResponseWriter, r *http.Request) {
		if server.ready {
			w.WriteHeader(http.StatusOK)
			w.Write([]byte("ok"))
		} else {
			w.WriteHeader(http.StatusServiceUnavailable)
			w.Write([]byte("not ready"))
		}
	})
	mux.HandleFunc("/healthz", func(w http.ResponseWriter, r *http.Request) {
		if server.healthy {
			w.WriteHeader(http.StatusOK)
			w.Write([]byte("ok"))
		} else {
			w.WriteHeader(http.StatusServiceUnavailable)
			w.Write([]byte("unhealthy"))
		}
	})

	server.httpServer = &http.Server{
		Addr:    fmt.Sprintf(":%d", port),
		Handler: mux,
	}

	go server.httpServer.ListenAndServe()

	return server
}

type testHealthServer struct {
	httpServer *http.Server
	ready      bool
	healthy    bool
}

func (s *testHealthServer) SetReady(ready bool) {
	s.ready = ready
}

func (s *testHealthServer) Stop(ctx context.Context) error {
	return s.httpServer.Shutdown(ctx)
}

func TestSyncRunner_FinalSyncUsesNewContext(t *testing.T) {
	// Verify that final sync doesn't use the canceled context
	runner := NewSyncRunner(SyncOptions{
		BotID: "test-bot",
	}, nil)

	// Track sync calls and the context state
	var syncedWithCanceledCtx bool
	mockSyncer := &contextTrackingSyncer{
		onSync: func(ctx context.Context) {
			if ctx.Err() != nil {
				syncedWithCanceledCtx = true
			}
		},
	}
	runner.syncers = []Syncer{mockSyncer}

	// Start and immediately stop
	runner.Start()
	time.Sleep(50 * time.Millisecond) // Let it initialize
	runner.Stop()

	// The final sync should NOT have been called with a canceled context
	assert.False(t, syncedWithCanceledCtx, "final sync should use a fresh context, not the canceled one")
}

// contextTrackingSyncer tracks context state during sync
type contextTrackingSyncer struct {
	onSync func(ctx context.Context)
}

func (s *contextTrackingSyncer) Sync(ctx context.Context) bool {
	if s.onSync != nil {
		s.onSync(ctx)
	}
	return true
}
