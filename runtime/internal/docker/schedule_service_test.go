package docker

import (
	"testing"
	"time"

	"runtime/internal/k8s/health"
	"runtime/internal/model"
	"runtime/internal/util"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestNewScheduleService_RequiresMongoURI(t *testing.T) {
	_, err := NewScheduleService(ScheduleServiceConfig{
		MongoURI: "",
		NATSUrl:  "nats://localhost:4222",
	})
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "MongoURI is required")
}

func TestNewScheduleService_RequiresNATSUrl(t *testing.T) {
	_, err := NewScheduleService(ScheduleServiceConfig{
		MongoURI: "mongodb://localhost:27017",
		NATSUrl:  "",
	})
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "NATSUrl is required")
}

func TestNewScheduleService_ValidConfig(t *testing.T) {
	service, err := NewScheduleService(ScheduleServiceConfig{
		MongoURI: "mongodb://localhost:27017",
		NATSUrl:  "nats://localhost:4222",
	})
	require.NoError(t, err)
	require.NotNil(t, service)
	defer service.Stop()

	assert.NotNil(t, service.state)
	assert.NotNil(t, service.logger)
	assert.NotNil(t, service.ctx)
	assert.NotNil(t, service.cancel)
}

func TestNewScheduleService_DefaultsDBName(t *testing.T) {
	service, err := NewScheduleService(ScheduleServiceConfig{
		MongoURI: "mongodb://localhost:27017",
		NATSUrl:  "nats://localhost:4222",
	})
	require.NoError(t, err)
	defer service.Stop()

	assert.Equal(t, "bot_scheduler", service.config.DBName)
	assert.Equal(t, "bot_schedules", service.config.Collection)
}

func TestNewScheduleService_CustomDBName(t *testing.T) {
	service, err := NewScheduleService(ScheduleServiceConfig{
		MongoURI:   "mongodb://localhost:27017",
		NATSUrl:    "nats://localhost:4222",
		DBName:     "custom_db",
		Collection: "custom_collection",
	})
	require.NoError(t, err)
	defer service.Stop()

	assert.Equal(t, "custom_db", service.config.DBName)
	assert.Equal(t, "custom_collection", service.config.Collection)
}

func TestScheduleService_ToExecutable(t *testing.T) {
	service, err := NewScheduleService(ScheduleServiceConfig{
		MongoURI: "mongodb://localhost:27017",
		NATSUrl:  "nats://localhost:4222",
	})
	require.NoError(t, err)
	defer service.Stop()

	schedule := model.BotSchedule{
		ID:                "schedule-123",
		NextExecutionTime: time.Now().Unix(),
		Config: map[string]interface{}{
			"symbol":   "BTC/USD",
			"schedule": "0 * * * *",
		},
		CustomBotVersion: model.CustomBotVersion{
			FilePath: "bots/scheduled-bot.zip",
			Config: model.APIBotConfig{
				Runtime: "nodejs20",
				Entrypoints: map[string]string{
					"bot": "index.js",
				},
			},
		},
	}

	executable := service.toExecutable(schedule)

	assert.Equal(t, "schedule-123", executable.ID)
	assert.Equal(t, "nodejs20", executable.Runtime)
	assert.Equal(t, "bot", executable.Entrypoint)
	assert.Equal(t, "bots/scheduled-bot.zip", executable.FilePath)
	assert.False(t, executable.IsLongRunning) // Scheduled bots run once
	assert.Equal(t, int32(-1), executable.Segment)
	assert.Equal(t, "BTC/USD", executable.Config["symbol"])
}

func TestScheduleService_Stop(t *testing.T) {
	service, err := NewScheduleService(ScheduleServiceConfig{
		MongoURI: "mongodb://localhost:27017",
		NATSUrl:  "nats://localhost:4222",
	})
	require.NoError(t, err)

	// Should not panic
	service.Stop()

	// Context should be cancelled
	select {
	case <-service.ctx.Done():
		// Expected
	default:
		t.Error("Context should be cancelled after Stop()")
	}
}

func TestScheduleService_GetStatus_EmptyState(t *testing.T) {
	service, err := NewScheduleService(ScheduleServiceConfig{
		MongoURI: "mongodb://localhost:27017",
		NATSUrl:  "nats://localhost:4222",
	})
	require.NoError(t, err)
	defer service.Stop()

	status := service.GetStatus()

	assert.Equal(t, int64(0), status["execution_count"])
	assert.Equal(t, int64(0), status["failed_count"])
	assert.Equal(t, 0, status["active_executions"])
}

func TestDefaultScheduleServiceConfig(t *testing.T) {
	t.Setenv("MONGO_URI", "mongodb://test:27017")
	t.Setenv("NATS_URL", "nats://test:4222")

	config := DefaultScheduleServiceConfig()

	assert.Equal(t, "mongodb://test:27017", config.MongoURI)
	assert.Equal(t, "nats://test:4222", config.NATSUrl)
	assert.NotNil(t, config.Logger)
}

func TestNewScheduleState(t *testing.T) {
	state := NewScheduleState()
	require.NotNil(t, state)
	assert.NotNil(t, state.executing)
	assert.Empty(t, state.executing)
}

func TestScheduleState_ExecutingTracking(t *testing.T) {
	service, err := NewScheduleService(ScheduleServiceConfig{
		MongoURI: "mongodb://localhost:27017",
		NATSUrl:  "nats://localhost:4222",
	})
	require.NoError(t, err)
	defer service.Stop()

	// Initially not executing
	assert.False(t, service.isExecuting("schedule-1"))

	// Mark as executing
	service.state.mu.Lock()
	service.state.executing["schedule-1"] = time.Now()
	service.state.activeExecutions++
	service.state.mu.Unlock()

	// Now should be executing
	assert.True(t, service.isExecuting("schedule-1"))

	// Remove from executing
	service.state.mu.Lock()
	delete(service.state.executing, "schedule-1")
	service.state.activeExecutions--
	service.state.mu.Unlock()

	// No longer executing
	assert.False(t, service.isExecuting("schedule-1"))
}

func TestScheduleState_MetricsTracking(t *testing.T) {
	service, err := NewScheduleService(ScheduleServiceConfig{
		MongoURI: "mongodb://localhost:27017",
		NATSUrl:  "nats://localhost:4222",
	})
	require.NoError(t, err)
	defer service.Stop()

	// Simulate execution counts
	service.state.mu.Lock()
	service.state.executionCount = 5
	service.state.failedCount = 2
	service.state.activeExecutions = 1
	service.state.mu.Unlock()

	status := service.GetStatus()
	assert.Equal(t, int64(5), status["execution_count"])
	assert.Equal(t, int64(2), status["failed_count"])
	assert.Equal(t, 1, status["active_executions"])
}

// ===== Edge Case Tests =====

// Test tryStartExecution - atomic check and mark
func TestScheduleService_TryStartExecution(t *testing.T) {
	service, err := NewScheduleService(ScheduleServiceConfig{
		MongoURI: "mongodb://localhost:27017",
		NATSUrl:  "nats://localhost:4222",
	})
	require.NoError(t, err)
	defer service.Stop()

	// First call should succeed
	success := service.tryStartExecution("schedule-1")
	assert.True(t, success, "first call should succeed")

	// Verify it's marked as executing
	assert.True(t, service.isExecuting("schedule-1"))

	// Second call for same schedule should fail (already executing)
	success = service.tryStartExecution("schedule-1")
	assert.False(t, success, "second call should fail for same schedule")

	// Different schedule should still succeed
	success = service.tryStartExecution("schedule-2")
	assert.True(t, success, "different schedule should succeed")

	// Verify active executions count
	service.state.mu.RLock()
	assert.Equal(t, 2, service.state.activeExecutions)
	service.state.mu.RUnlock()
}

// Test concurrent tryStartExecution calls
func TestScheduleService_TryStartExecution_Concurrent(t *testing.T) {
	service, err := NewScheduleService(ScheduleServiceConfig{
		MongoURI: "mongodb://localhost:27017",
		NATSUrl:  "nats://localhost:4222",
	})
	require.NoError(t, err)
	defer service.Stop()

	// Run multiple concurrent attempts to start the same schedule
	const goroutines = 10
	results := make(chan bool, goroutines)

	for i := 0; i < goroutines; i++ {
		go func() {
			results <- service.tryStartExecution("schedule-concurrent")
		}()
	}

	// Collect results
	successCount := 0
	for i := 0; i < goroutines; i++ {
		if <-results {
			successCount++
		}
	}

	// Exactly one should succeed (atomic operation)
	assert.Equal(t, 1, successCount, "exactly one goroutine should succeed")
}

// Test default check interval
func TestNewScheduleService_DefaultsCheckInterval(t *testing.T) {
	service, err := NewScheduleService(ScheduleServiceConfig{
		MongoURI: "mongodb://localhost:27017",
		NATSUrl:  "nats://localhost:4222",
	})
	require.NoError(t, err)
	defer service.Stop()

	assert.Equal(t, 10*time.Second, service.config.CheckInterval)
}

// Test custom check interval
func TestNewScheduleService_CustomCheckInterval(t *testing.T) {
	service, err := NewScheduleService(ScheduleServiceConfig{
		MongoURI:      "mongodb://localhost:27017",
		NATSUrl:       "nats://localhost:4222",
		CheckInterval: 30 * time.Second,
	})
	require.NoError(t, err)
	defer service.Stop()

	assert.Equal(t, 30*time.Second, service.config.CheckInterval)
}

// Test nil logger defaults to DefaultLogger
func TestNewScheduleService_NilLogger(t *testing.T) {
	service, err := NewScheduleService(ScheduleServiceConfig{
		MongoURI: "mongodb://localhost:27017",
		NATSUrl:  "nats://localhost:4222",
		Logger:   nil,
	})
	require.NoError(t, err)
	defer service.Stop()

	assert.NotNil(t, service.logger)
}

// Test runner StartContainer with error status
// Note: Full executeSchedule requires MongoDB; this tests the runner behavior
func TestScheduleService_StartContainer_ErrorStatus(t *testing.T) {
	service, err := NewScheduleService(ScheduleServiceConfig{
		MongoURI: "mongodb://localhost:27017",
		NATSUrl:  "nats://localhost:4222",
	})
	require.NoError(t, err)
	defer service.Stop()

	mockRunner := NewMockDockerRunner()
	mockRunner.shouldFailStart = true
	service.runner = mockRunner

	schedule := model.BotSchedule{
		ID: "schedule-error",
		Config: map[string]interface{}{
			"schedule": "0 * * * *",
		},
		CustomBotVersion: model.CustomBotVersion{
			FilePath: "bots/test.zip",
			Config: model.APIBotConfig{
				Runtime: "python3.11",
				Entrypoints: map[string]string{
					"bot": "main.py",
				},
			},
		},
	}

	executable := service.toExecutable(schedule)

	// Call runner directly to test error handling
	result, err := service.runner.StartContainer(service.ctx, executable)

	assert.NoError(t, err) // Mock doesn't return Go error
	assert.Equal(t, "error", result.Status)
	assert.Equal(t, "mock start failed", result.Error)
}

// Test runner StartContainer successful execution
func TestScheduleService_StartContainer_Success(t *testing.T) {
	service, err := NewScheduleService(ScheduleServiceConfig{
		MongoURI: "mongodb://localhost:27017",
		NATSUrl:  "nats://localhost:4222",
	})
	require.NoError(t, err)
	defer service.Stop()

	mockRunner := NewMockDockerRunner()
	service.runner = mockRunner

	schedule := model.BotSchedule{
		ID: "schedule-success",
		Config: map[string]interface{}{
			"schedule": "0 * * * *",
		},
		CustomBotVersion: model.CustomBotVersion{
			FilePath: "bots/test.zip",
			Config: model.APIBotConfig{
				Runtime: "python3.11",
				Entrypoints: map[string]string{
					"bot": "main.py",
				},
			},
		},
	}

	executable := service.toExecutable(schedule)

	// Call runner directly
	result, err := service.runner.StartContainer(service.ctx, executable)

	assert.NoError(t, err)
	assert.Equal(t, "running", result.Status)
	assert.NotEmpty(t, result.ContainerID)
}

// Test state cleanup after execution completion
func TestScheduleService_ExecutingStateCleanup(t *testing.T) {
	service, err := NewScheduleService(ScheduleServiceConfig{
		MongoURI: "mongodb://localhost:27017",
		NATSUrl:  "nats://localhost:4222",
	})
	require.NoError(t, err)
	defer service.Stop()

	scheduleID := "schedule-cleanup"

	// Mark as executing
	success := service.tryStartExecution(scheduleID)
	require.True(t, success)
	require.True(t, service.isExecuting(scheduleID))

	// Simulate cleanup that happens at end of executeSchedule
	service.state.mu.Lock()
	delete(service.state.executing, scheduleID)
	service.state.activeExecutions--
	service.state.mu.Unlock()

	// Verify cleanup
	assert.False(t, service.isExecuting(scheduleID))

	service.state.mu.RLock()
	assert.Equal(t, 0, service.state.activeExecutions)
	service.state.mu.RUnlock()
}

// Test toExecutable with different runtimes
func TestScheduleService_ToExecutable_DifferentRuntimes(t *testing.T) {
	service, err := NewScheduleService(ScheduleServiceConfig{
		MongoURI: "mongodb://localhost:27017",
		NATSUrl:  "nats://localhost:4222",
	})
	require.NoError(t, err)
	defer service.Stop()

	tests := []struct {
		name    string
		runtime string
	}{
		{"python", "python3.11"},
		{"nodejs", "nodejs20"},
		{"rust", "rust1.75"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			schedule := model.BotSchedule{
				ID: "schedule-" + tt.name,
				Config: map[string]interface{}{
					"symbol":   "BTC/USD",
					"schedule": "0 * * * *",
				},
				CustomBotVersion: model.CustomBotVersion{
					FilePath: "bots/" + tt.name + "-bot.zip",
					Config: model.APIBotConfig{
						Runtime: tt.runtime,
						Entrypoints: map[string]string{
							"bot": "main",
						},
					},
				},
			}

			executable := service.toExecutable(schedule)

			assert.Equal(t, tt.runtime, executable.Runtime)
			assert.False(t, executable.IsLongRunning)
		})
	}
}

// Test waitForExecutions with no active executions
func TestScheduleService_WaitForExecutions_NoActive(t *testing.T) {
	service, err := NewScheduleService(ScheduleServiceConfig{
		MongoURI: "mongodb://localhost:27017",
		NATSUrl:  "nats://localhost:4222",
	})
	require.NoError(t, err)
	defer service.Stop()

	// Should complete immediately when no executions
	done := make(chan struct{})
	go func() {
		service.waitForExecutions(100 * time.Millisecond)
		close(done)
	}()

	select {
	case <-done:
		// Expected - completed quickly
	case <-time.After(200 * time.Millisecond):
		t.Error("waitForExecutions should complete immediately when no active executions")
	}
}

// Test schedule loop skips when deps are unhealthy by exercising the actual goroutine
func TestScheduleLoop_SkipsWhenDepsUnhealthy(t *testing.T) {
	service, err := NewScheduleService(ScheduleServiceConfig{
		MongoURI:      "mongodb://localhost:27017",
		NATSUrl:       "nats://localhost:4222",
		CheckInterval: 20 * time.Millisecond,
	})
	require.NoError(t, err)

	mockRunner := NewMockDockerRunner()
	service.runner = mockRunner

	// Create unhealthy dep checker
	dc := NewDependencyChecker(nil, nil, "test-bucket", &util.NopLogger{})
	dc.SetLastResult(false, false)
	service.depChecker = dc

	// NOTE: mongoClient is nil, so if the dep gate regressed and checkSchedules() ran,
	// it would panic on the MongoDB query, causing the test to fail. This provides
	// an implicit safety net beyond the explicit start-count assertion below.

	// Start the actual schedule loop goroutine
	service.wg.Add(1)
	go service.runScheduleLoop()

	// Let a few ticks pass
	time.Sleep(80 * time.Millisecond)

	// Stop the service to terminate the loop
	service.Stop()
	service.wg.Wait()

	// No containers should have been started (schedule check was skipped)
	assert.Equal(t, 0, mockRunner.GetStartCallCount(), "no containers should start when deps unhealthy")
}

// Test that health server is updated when deps fail
func TestScheduleService_HealthServerUpdatedOnDepFailure(t *testing.T) {
	hs := health.NewServer(0) // port 0 = don't actually listen

	service, err := NewScheduleService(ScheduleServiceConfig{
		MongoURI:            "mongodb://localhost:27017",
		NATSUrl:             "nats://localhost:4222",
		HealthCheckInterval: 20 * time.Millisecond,
	})
	require.NoError(t, err)

	service.healthServer = hs

	// Create unhealthy dep checker
	dc := NewDependencyChecker(nil, nil, "test-bucket", &util.NopLogger{})
	service.depChecker = dc

	// Simulate health check loop iteration
	mongoOK, minioOK := dc.CheckHealth(service.ctx)
	dc.SetLastResult(mongoOK, minioOK)

	// Both should be false (nil clients)
	assert.False(t, mongoOK)
	assert.False(t, minioOK)

	// Simulate what the health check loop does on unhealthy deps
	if !mongoOK || !minioOK {
		hs.SetReady(false)
	}

	// Readiness should be false
	assert.False(t, hs.IsReady(), "readiness should be false when deps are down")
	// Liveness should remain true (not gated on deps)
	assert.True(t, hs.IsHealthy(), "liveness should remain true regardless of dep state")

	service.Stop()
}

// Test multiple schedules in state tracking
func TestScheduleState_MultipleSchedules(t *testing.T) {
	service, err := NewScheduleService(ScheduleServiceConfig{
		MongoURI: "mongodb://localhost:27017",
		NATSUrl:  "nats://localhost:4222",
	})
	require.NoError(t, err)
	defer service.Stop()

	// Start multiple schedules
	scheduleIDs := []string{"schedule-a", "schedule-b", "schedule-c"}
	for _, id := range scheduleIDs {
		success := service.tryStartExecution(id)
		assert.True(t, success, "should succeed for %s", id)
	}

	// Verify all are executing
	for _, id := range scheduleIDs {
		assert.True(t, service.isExecuting(id), "%s should be executing", id)
	}

	// Verify active count
	service.state.mu.RLock()
	assert.Equal(t, 3, service.state.activeExecutions)
	service.state.mu.RUnlock()

	// Mark one as completed
	service.state.mu.Lock()
	delete(service.state.executing, "schedule-b")
	service.state.activeExecutions--
	service.state.mu.Unlock()

	// Verify state updated correctly
	assert.True(t, service.isExecuting("schedule-a"))
	assert.False(t, service.isExecuting("schedule-b"))
	assert.True(t, service.isExecuting("schedule-c"))

	service.state.mu.RLock()
	assert.Equal(t, 2, service.state.activeExecutions)
	service.state.mu.RUnlock()
}

func TestScheduleService_StartsUnready_WhenDepsDown(t *testing.T) {
	hs := health.NewServer(0)

	service, err := NewScheduleService(ScheduleServiceConfig{
		MongoURI: "mongodb://localhost:27017",
		NATSUrl:  "nats://localhost:4222",
	})
	require.NoError(t, err)

	service.healthServer = hs
	hs.SetReady(true) // ensure we verify an actual transition

	// Create dep checker with nil clients (unhealthy)
	service.depChecker = NewDependencyChecker(nil, nil, "test-bucket", &util.NopLogger{})

	// Start the actual health check loop to drive readiness
	service.wg.Add(1)
	go runDepHealthLoop(service.ctx, 20*time.Millisecond, service.depChecker, hs, &util.NopLogger{}, service.wg.Done)
	defer func() {
		service.Stop()
		service.wg.Wait()
	}()

	// Readiness should be driven to false by the health loop
	require.Eventually(t, func() bool {
		return !hs.IsReady()
	}, 250*time.Millisecond, 10*time.Millisecond, "readiness should be driven to false by health loop")

	// Liveness should remain true (not gated on deps)
	assert.True(t, hs.IsHealthy(), "liveness should remain true regardless of dep state")
}
