package docker

import (
	"sync"
	"time"

	"runtime/internal/model"
)

// RunningBot represents a running bot with its container state
type RunningBot struct {
	BotID       string
	ContainerID string
	Status      string // "starting", "running", "stopping", "failed"
	StartTime   time.Time
	Bot         model.Bot
	Restarts    int
}

// ServiceState contains all service state in a single structure
// Protected by a single RWMutex to eliminate deadlock sources
type ServiceState struct {
	mu sync.RWMutex

	// Current running containers (botID -> container info)
	runningBots map[string]*RunningBot

	// Metrics
	lastReconcile    time.Time
	reconcileCount   int64
	activeContainers int
	failedRestarts   int
}

// NewServiceState creates a new ServiceState instance
func NewServiceState() *ServiceState {
	return &ServiceState{
		runningBots: make(map[string]*RunningBot),
	}
}

// GetRunningBot returns a deep copy of a running bot by ID
func (s *ServiceState) GetRunningBot(botID string) (*RunningBot, bool) {
	s.mu.RLock()
	defer s.mu.RUnlock()
	bot, ok := s.runningBots[botID]
	if !ok {
		return nil, false
	}
	// Return a deep copy to prevent data races on nested references
	return &RunningBot{
		BotID:       bot.BotID,
		ContainerID: bot.ContainerID,
		Status:      bot.Status,
		StartTime:   bot.StartTime,
		Bot:         bot.Bot.DeepCopy(),
		Restarts:    bot.Restarts,
	}, true
}

// GetAllRunningBots returns deep copies of all running bots
func (s *ServiceState) GetAllRunningBots() map[string]*RunningBot {
	s.mu.RLock()
	defer s.mu.RUnlock()
	result := make(map[string]*RunningBot, len(s.runningBots))
	for k, v := range s.runningBots {
		result[k] = &RunningBot{
			BotID:       v.BotID,
			ContainerID: v.ContainerID,
			Status:      v.Status,
			StartTime:   v.StartTime,
			Bot:         v.Bot.DeepCopy(),
			Restarts:    v.Restarts,
		}
	}
	return result
}

// SetRunningBot adds or updates a running bot
func (s *ServiceState) SetRunningBot(bot *RunningBot) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.runningBots[bot.BotID] = bot
	s.activeContainers = len(s.runningBots)
}

// RemoveRunningBot removes a bot from the running state
func (s *ServiceState) RemoveRunningBot(botID string) {
	s.mu.Lock()
	defer s.mu.Unlock()
	delete(s.runningBots, botID)
	s.activeContainers = len(s.runningBots)
}

// IncrementFailedRestarts increments the failed restarts counter
func (s *ServiceState) IncrementFailedRestarts() {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.failedRestarts++
}

// UpdateReconcileMetrics updates the reconciliation metrics
func (s *ServiceState) UpdateReconcileMetrics() {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.lastReconcile = time.Now()
	s.reconcileCount++
}

// GetMetrics returns current service metrics
func (s *ServiceState) GetMetrics() map[string]interface{} {
	s.mu.RLock()
	defer s.mu.RUnlock()
	return map[string]interface{}{
		"active_containers": s.activeContainers,
		"reconcile_count":   s.reconcileCount,
		"last_reconcile":    s.lastReconcile,
		"failed_restarts":   s.failedRestarts,
	}
}

// ClearAllRunningBots removes all bots from running state
func (s *ServiceState) ClearAllRunningBots() {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.runningBots = make(map[string]*RunningBot)
	s.activeContainers = 0
}

// UpdateRunningBotsFromMap replaces the running bots map with a defensive copy
func (s *ServiceState) UpdateRunningBotsFromMap(newState map[string]*RunningBot) {
	s.mu.Lock()
	defer s.mu.Unlock()
	// Create defensive copy to prevent external modifications
	s.runningBots = make(map[string]*RunningBot, len(newState))
	for k, v := range newState {
		s.runningBots[k] = &RunningBot{
			BotID:       v.BotID,
			ContainerID: v.ContainerID,
			Status:      v.Status,
			StartTime:   v.StartTime,
			Bot:         v.Bot.DeepCopy(),
			Restarts:    v.Restarts,
		}
	}
	s.activeContainers = len(s.runningBots)
}
