// Package desired decides which Bots and schedules should run, and when a
// schedule is due, from what the runtime stores. Docker mode and Kubernetes
// mode both decide through it, so a Bot runs in both or in neither.
package desired

import (
	"context"
	"fmt"
	"time"

	"runtime/internal/model"
	"runtime/internal/util"
)

// Repository is where the runtime stores the Bots and schedules the api
// announced.
type Repository interface {
	// Bots are every stored realtime Bot, enabled or not.
	Bots(ctx context.Context) ([]model.Bot, error)
	// Schedules are every stored schedule, enabled or not.
	Schedules(ctx context.Context) ([]model.BotSchedule, error)
	SetNextExecutionTime(ctx context.Context, scheduleID string, next time.Time) error
}

// State is the desired state of the runtime, read from a Repository.
type State struct {
	repo Repository
}

func New(repo Repository) *State {
	return &State{repo: repo}
}

// IsEnabled is the one enabled rule: a Bot or schedule runs unless its
// config.enabled is false, which is what the CLI and the api write. Any other
// value, or none, leaves it enabled.
func IsEnabled(config map[string]interface{}) bool {
	enabled, ok := config["enabled"].(bool)
	return !ok || enabled
}

// Bots are the realtime Bots that should be running.
func (s *State) Bots(ctx context.Context) ([]model.Bot, error) {
	bots, err := s.repo.Bots(ctx)
	if err != nil {
		return nil, err
	}
	enabled := make([]model.Bot, 0, len(bots))
	for _, bot := range bots {
		if IsEnabled(bot.Config) {
			enabled = append(enabled, bot)
		}
	}
	return enabled, nil
}

// Schedules are the schedules that should run on their cron.
func (s *State) Schedules(ctx context.Context) ([]model.BotSchedule, error) {
	schedules, err := s.repo.Schedules(ctx)
	if err != nil {
		return nil, err
	}
	enabled := make([]model.BotSchedule, 0, len(schedules))
	for _, schedule := range schedules {
		if IsEnabled(schedule.Config) {
			enabled = append(enabled, schedule)
		}
	}
	return enabled, nil
}

// DueSchedules are the schedules that should run at now.
func (s *State) DueSchedules(ctx context.Context, now time.Time) ([]model.BotSchedule, error) {
	schedules, err := s.Schedules(ctx)
	if err != nil {
		return nil, err
	}
	due := make([]model.BotSchedule, 0, len(schedules))
	for _, schedule := range schedules {
		if schedule.NextExecutionTime <= now.Unix() {
			due = append(due, schedule)
		}
	}
	return due, nil
}

// ScheduleRan records that schedule ran at ranAt, so its next run is the
// cron's next tick after ranAt, which it returns.
func (s *State) ScheduleRan(ctx context.Context, schedule model.BotSchedule, ranAt time.Time) (time.Time, error) {
	cron, ok := schedule.Config["schedule"].(string)
	if !ok || cron == "" {
		return time.Time{}, fmt.Errorf("schedule %s has no cron expression", schedule.ID)
	}
	next, err := util.CalculateNextExecutionTime(cron, ranAt)
	if err != nil {
		return time.Time{}, fmt.Errorf("schedule %s: %w", schedule.ID, err)
	}
	if err := s.repo.SetNextExecutionTime(ctx, schedule.ID, next); err != nil {
		return time.Time{}, err
	}
	return next, nil
}

// ScheduleToBot is schedule as the Bot its runs and queries launch.
func ScheduleToBot(schedule model.BotSchedule) model.Bot {
	return model.Bot{
		ID:               schedule.ID,
		Config:           schedule.Config,
		CustomBotVersion: schedule.CustomBotVersion,
	}
}
