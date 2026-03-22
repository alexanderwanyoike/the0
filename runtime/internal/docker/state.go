package docker

import (
	"math"
	"sync"
	"time"

	"runtime/internal/model"
)

// BotFailure tracks consecutive failures for a single bot, enabling
// exponential backoff to prevent cascading container spawns.
type BotFailure struct {
	ConsecutiveFailures int
	LastFailureTime     time.Time
}

const (
	// maxConsecutiveFailures is the threshold after which backoff kicks in.
	maxConsecutiveFailures = 5
	// maxBackoff caps the exponential backoff window.
	maxBackoff = 5 * time.Minute
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

	// Per-bot failure tracking for backoff
	botFailures map[string]*BotFailure

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
		botFailures: make(map[string]*BotFailure),
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

// RecordBotFailure increments the consecutive failure counter for a bot.
func (s *ServiceState) RecordBotFailure(botID string) {
	s.mu.Lock()
	defer s.mu.Unlock()
	f, ok := s.botFailures[botID]
	if !ok {
		f = &BotFailure{}
		s.botFailures[botID] = f
	}
	f.ConsecutiveFailures++
	f.LastFailureTime = time.Now()
}

// ResetBotFailure clears the failure counter for a bot after a successful start.
func (s *ServiceState) ResetBotFailure(botID string) {
	s.mu.Lock()
	defer s.mu.Unlock()
	delete(s.botFailures, botID)
}

// ShouldSkipBot returns true if the bot has exceeded the failure threshold
// and is still within its exponential backoff window.
func (s *ServiceState) ShouldSkipBot(botID string) bool {
	s.mu.RLock()
	defer s.mu.RUnlock()
	f, ok := s.botFailures[botID]
	if !ok {
		return false
	}
	if f.ConsecutiveFailures < maxConsecutiveFailures {
		return false
	}
	// Exponential backoff: 2^(failures - threshold) seconds, capped at maxBackoff
	exp := f.ConsecutiveFailures - maxConsecutiveFailures
	backoff := time.Duration(math.Min(math.Pow(2, float64(exp)), maxBackoff.Seconds())) * time.Second
	return time.Since(f.LastFailureTime) < backoff
}

// GetBotFailure returns the failure info for a bot (test helper).
func (s *ServiceState) GetBotFailure(botID string) (*BotFailure, bool) {
	s.mu.RLock()
	defer s.mu.RUnlock()
	f, ok := s.botFailures[botID]
	if !ok {
		return nil, false
	}
	cp := *f
	return &cp, true
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
