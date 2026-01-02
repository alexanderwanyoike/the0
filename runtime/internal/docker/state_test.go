package docker

import (
	"sync"
	"testing"
	"time"

	"runtime/internal/model"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestNewServiceState(t *testing.T) {
	state := NewServiceState()
	require.NotNil(t, state)
	assert.NotNil(t, state.runningBots)
	assert.Empty(t, state.runningBots)
}

func TestServiceState_SetAndGetRunningBot(t *testing.T) {
	state := NewServiceState()

	bot := &RunningBot{
		BotID:       "bot-123",
		ContainerID: "container-abc",
		Status:      "running",
		StartTime:   time.Now(),
		Bot: model.Bot{
			ID: "bot-123",
		},
	}

	// Set the bot
	state.SetRunningBot(bot)

	// Get the bot
	retrieved, exists := state.GetRunningBot("bot-123")
	assert.True(t, exists)
	require.NotNil(t, retrieved)
	assert.Equal(t, "bot-123", retrieved.BotID)
	assert.Equal(t, "container-abc", retrieved.ContainerID)
	assert.Equal(t, "running", retrieved.Status)
}

func TestServiceState_GetRunningBot_NotFound(t *testing.T) {
	state := NewServiceState()

	retrieved, exists := state.GetRunningBot("nonexistent")
	assert.False(t, exists)
	assert.Nil(t, retrieved)
}

func TestServiceState_GetRunningBot_ReturnsCopy(t *testing.T) {
	state := NewServiceState()

	bot := &RunningBot{
		BotID:       "bot-123",
		ContainerID: "container-abc",
		Status:      "running",
	}
	state.SetRunningBot(bot)

	// Get the bot and modify it
	retrieved, _ := state.GetRunningBot("bot-123")
	retrieved.Status = "modified"

	// Original should be unchanged
	original, _ := state.GetRunningBot("bot-123")
	assert.Equal(t, "running", original.Status)
}

func TestServiceState_GetAllRunningBots(t *testing.T) {
	state := NewServiceState()

	// Add multiple bots
	state.SetRunningBot(&RunningBot{BotID: "bot-1", ContainerID: "c1"})
	state.SetRunningBot(&RunningBot{BotID: "bot-2", ContainerID: "c2"})
	state.SetRunningBot(&RunningBot{BotID: "bot-3", ContainerID: "c3"})

	all := state.GetAllRunningBots()
	assert.Len(t, all, 3)
	assert.Contains(t, all, "bot-1")
	assert.Contains(t, all, "bot-2")
	assert.Contains(t, all, "bot-3")
}

func TestServiceState_GetAllRunningBots_ReturnsCopy(t *testing.T) {
	state := NewServiceState()

	state.SetRunningBot(&RunningBot{BotID: "bot-1", ContainerID: "c1"})

	// Get all and modify
	all := state.GetAllRunningBots()
	delete(all, "bot-1")

	// Original should still have the bot
	allAgain := state.GetAllRunningBots()
	assert.Len(t, allAgain, 1)
}

func TestServiceState_RemoveRunningBot(t *testing.T) {
	state := NewServiceState()

	state.SetRunningBot(&RunningBot{BotID: "bot-123", ContainerID: "c1"})

	// Verify it exists
	_, exists := state.GetRunningBot("bot-123")
	assert.True(t, exists)

	// Remove it
	state.RemoveRunningBot("bot-123")

	// Verify it's gone
	_, exists = state.GetRunningBot("bot-123")
	assert.False(t, exists)
}

func TestServiceState_RemoveRunningBot_NonExistent(t *testing.T) {
	state := NewServiceState()

	// Should not panic
	state.RemoveRunningBot("nonexistent")
}

func TestServiceState_IncrementFailedRestarts(t *testing.T) {
	state := NewServiceState()

	metrics := state.GetMetrics()
	assert.Equal(t, 0, metrics["failed_restarts"])

	state.IncrementFailedRestarts()
	state.IncrementFailedRestarts()
	state.IncrementFailedRestarts()

	metrics = state.GetMetrics()
	assert.Equal(t, 3, metrics["failed_restarts"])
}

func TestServiceState_UpdateReconcileMetrics(t *testing.T) {
	state := NewServiceState()

	before := time.Now()
	state.UpdateReconcileMetrics()
	after := time.Now()

	metrics := state.GetMetrics()
	assert.Equal(t, int64(1), metrics["reconcile_count"])

	lastReconcile := metrics["last_reconcile"].(time.Time)
	assert.True(t, lastReconcile.After(before) || lastReconcile.Equal(before))
	assert.True(t, lastReconcile.Before(after) || lastReconcile.Equal(after))
}

func TestServiceState_ActiveContainersCount(t *testing.T) {
	state := NewServiceState()

	metrics := state.GetMetrics()
	assert.Equal(t, 0, metrics["active_containers"])

	state.SetRunningBot(&RunningBot{BotID: "bot-1"})
	metrics = state.GetMetrics()
	assert.Equal(t, 1, metrics["active_containers"])

	state.SetRunningBot(&RunningBot{BotID: "bot-2"})
	metrics = state.GetMetrics()
	assert.Equal(t, 2, metrics["active_containers"])

	state.RemoveRunningBot("bot-1")
	metrics = state.GetMetrics()
	assert.Equal(t, 1, metrics["active_containers"])
}

func TestServiceState_ClearAllRunningBots(t *testing.T) {
	state := NewServiceState()

	state.SetRunningBot(&RunningBot{BotID: "bot-1"})
	state.SetRunningBot(&RunningBot{BotID: "bot-2"})
	state.SetRunningBot(&RunningBot{BotID: "bot-3"})

	assert.Len(t, state.GetAllRunningBots(), 3)

	state.ClearAllRunningBots()

	assert.Empty(t, state.GetAllRunningBots())
	metrics := state.GetMetrics()
	assert.Equal(t, 0, metrics["active_containers"])
}

func TestServiceState_UpdateRunningBotsFromMap(t *testing.T) {
	state := NewServiceState()

	state.SetRunningBot(&RunningBot{BotID: "old-bot"})

	newBots := map[string]*RunningBot{
		"new-1": {BotID: "new-1"},
		"new-2": {BotID: "new-2"},
	}

	state.UpdateRunningBotsFromMap(newBots)

	all := state.GetAllRunningBots()
	assert.Len(t, all, 2)
	assert.Contains(t, all, "new-1")
	assert.Contains(t, all, "new-2")
	assert.NotContains(t, all, "old-bot")
}

func TestServiceState_ConcurrentAccess(t *testing.T) {
	state := NewServiceState()
	var wg sync.WaitGroup

	// Spawn multiple goroutines doing concurrent operations
	for i := 0; i < 100; i++ {
		wg.Add(3)

		// Writer
		go func(id int) {
			defer wg.Done()
			state.SetRunningBot(&RunningBot{
				BotID:       "bot-" + string(rune(id)),
				ContainerID: "container-" + string(rune(id)),
			})
		}(i)

		// Reader
		go func(id int) {
			defer wg.Done()
			state.GetRunningBot("bot-" + string(rune(id)))
		}(i)

		// Metrics reader
		go func() {
			defer wg.Done()
			state.GetMetrics()
		}()
	}

	wg.Wait()
	// If we get here without deadlock or panic, the test passes
}
