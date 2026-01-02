package docker

import (
	"testing"
	"time"

	"runtime/internal/model"

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
	service, _ := NewScheduleService(ScheduleServiceConfig{
		MongoURI: "mongodb://localhost:27017",
		NATSUrl:  "nats://localhost:4222",
	})
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
	assert.True(t, executable.PersistResults)  // Results should be persisted
	assert.Equal(t, int32(-1), executable.Segment)
	assert.Equal(t, "BTC/USD", executable.Config["symbol"])
}

func TestScheduleService_Stop(t *testing.T) {
	service, _ := NewScheduleService(ScheduleServiceConfig{
		MongoURI: "mongodb://localhost:27017",
		NATSUrl:  "nats://localhost:4222",
	})

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
	service, _ := NewScheduleService(ScheduleServiceConfig{
		MongoURI: "mongodb://localhost:27017",
		NATSUrl:  "nats://localhost:4222",
	})
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
	service, _ := NewScheduleService(ScheduleServiceConfig{
		MongoURI: "mongodb://localhost:27017",
		NATSUrl:  "nats://localhost:4222",
	})
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
	service, _ := NewScheduleService(ScheduleServiceConfig{
		MongoURI: "mongodb://localhost:27017",
		NATSUrl:  "nats://localhost:4222",
	})
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
