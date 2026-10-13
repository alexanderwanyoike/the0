package desired_test

import (
	"context"
	"errors"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"runtime/internal/desired"
	"runtime/internal/model"
)

type fakeRepository struct {
	bots      []model.Bot
	schedules []model.BotSchedule
	err       error
	nextRuns  map[string]time.Time
}

func (f *fakeRepository) Bots(context.Context) ([]model.Bot, error) {
	return f.bots, f.err
}

func (f *fakeRepository) Schedules(context.Context) ([]model.BotSchedule, error) {
	return f.schedules, f.err
}

func (f *fakeRepository) SetNextExecutionTime(_ context.Context, scheduleID string, next time.Time) error {
	if f.err != nil {
		return f.err
	}
	if f.nextRuns == nil {
		f.nextRuns = map[string]time.Time{}
	}
	f.nextRuns[scheduleID] = next
	return nil
}

func TestIsEnabled_UnlessConfigEnabledIsFalse(t *testing.T) {
	tests := []struct {
		name   string
		config map[string]interface{}
		want   bool
	}{
		{"no config", nil, true},
		{"config without enabled", map[string]interface{}{"symbol": "BTC/USD"}, true},
		{"enabled", map[string]interface{}{"enabled": true}, true},
		{"disabled", map[string]interface{}{"enabled": false}, false},
		{"enabled as text", map[string]interface{}{"enabled": "false"}, true},
		{"enabled as a number", map[string]interface{}{"enabled": 0}, true},
		{"enabled is null", map[string]interface{}{"enabled": nil}, true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			assert.Equal(t, tt.want, desired.IsEnabled(tt.config))
		})
	}
}

func TestBots_AreTheStoredBotsThatAreEnabled(t *testing.T) {
	repo := &fakeRepository{bots: []model.Bot{
		{ID: "enabled", Config: map[string]interface{}{"enabled": true}},
		{ID: "disabled", Config: map[string]interface{}{"enabled": false}},
		{ID: "no-config"},
	}}

	bots, err := desired.New(repo).Bots(context.Background())

	require.NoError(t, err)
	assert.Equal(t, []string{"enabled", "no-config"}, botIDs(bots))
}

func TestBots_FailWhenTheRepositoryFails(t *testing.T) {
	repo := &fakeRepository{err: errors.New("store down")}

	_, err := desired.New(repo).Bots(context.Background())

	assert.ErrorContains(t, err, "store down")
}

func TestSchedules_AreTheStoredSchedulesThatAreEnabled(t *testing.T) {
	repo := &fakeRepository{schedules: []model.BotSchedule{
		schedule("enabled", "0 9 * * *", time.Time{}),
		disabled(schedule("disabled", "0 9 * * *", time.Time{})),
	}}

	schedules, err := desired.New(repo).Schedules(context.Background())

	require.NoError(t, err)
	assert.Equal(t, []string{"enabled"}, scheduleIDs(schedules))
}

func TestSchedules_FailWhenTheRepositoryFails(t *testing.T) {
	repo := &fakeRepository{err: errors.New("store down")}

	_, err := desired.New(repo).Schedules(context.Background())

	assert.ErrorContains(t, err, "store down")
}

func TestDueSchedules_AreEnabledAndAtOrPastTheirNextRun(t *testing.T) {
	now := time.Date(2026, 10, 13, 9, 0, 0, 0, time.UTC)
	repo := &fakeRepository{schedules: []model.BotSchedule{
		schedule("overdue", "0 9 * * *", now.Add(-time.Hour)),
		schedule("due-now", "0 9 * * *", now),
		schedule("not-yet", "0 9 * * *", now.Add(time.Second)),
		disabled(schedule("overdue-but-disabled", "0 9 * * *", now.Add(-time.Hour))),
	}}

	due, err := desired.New(repo).DueSchedules(context.Background(), now)

	require.NoError(t, err)
	assert.Equal(t, []string{"overdue", "due-now"}, scheduleIDs(due))
}

func TestDueSchedules_FailWhenTheRepositoryFails(t *testing.T) {
	repo := &fakeRepository{err: errors.New("store down")}

	_, err := desired.New(repo).DueSchedules(context.Background(), time.Now())

	assert.ErrorContains(t, err, "store down")
}

func TestScheduleRan_MovesTheNextRunToTheCronsNextTickAfterTheRun(t *testing.T) {
	ranAt := time.Date(2026, 10, 13, 9, 0, 42, 0, time.UTC)
	repo := &fakeRepository{}

	next, err := desired.New(repo).ScheduleRan(context.Background(), schedule("daily", "0 9 * * *", ranAt), ranAt)

	require.NoError(t, err)
	assert.Equal(t, time.Date(2026, 10, 14, 9, 0, 0, 0, time.UTC), next)
	assert.Equal(t, next, repo.nextRuns["daily"])
}

func TestScheduleRan_LeavesTheNextRunWhenTheCronCannotBeRead(t *testing.T) {
	for name, cron := range map[string]interface{}{
		"no cron":           nil,
		"cron is not text":  42,
		"a seconds field":   "30 0 9 * * *",
		"not a cron at all": "every morning",
	} {
		t.Run(name, func(t *testing.T) {
			repo := &fakeRepository{}
			s := schedule("broken", "", time.Time{})
			s.Config["schedule"] = cron

			_, err := desired.New(repo).ScheduleRan(context.Background(), s, time.Now())

			assert.Error(t, err)
			assert.Empty(t, repo.nextRuns)
		})
	}
}

func TestReschedules_WhenTheCronChangesOrTheScheduleIsEnabledAgain(t *testing.T) {
	tests := []struct {
		name   string
		stored map[string]interface{}
		update map[string]interface{}
		want   bool
	}{
		{"cron changes", map[string]interface{}{"schedule": "0 9 * * *"}, map[string]interface{}{"schedule": "0 10 * * *"}, true},
		{"enabled again", map[string]interface{}{"schedule": "0 9 * * *", "enabled": false}, map[string]interface{}{"schedule": "0 9 * * *", "enabled": true}, true},
		{"enabled again by dropping enabled", map[string]interface{}{"schedule": "0 9 * * *", "enabled": false}, map[string]interface{}{"schedule": "0 9 * * *"}, true},
		{"stored without a cron", map[string]interface{}{}, map[string]interface{}{"schedule": "0 9 * * *"}, true},
		{"other config changes", map[string]interface{}{"schedule": "0 9 * * *", "symbol": "BTC/USD"}, map[string]interface{}{"schedule": "0 9 * * *", "symbol": "ETH/USD"}, false},
		{"disabled", map[string]interface{}{"schedule": "0 9 * * *"}, map[string]interface{}{"schedule": "0 9 * * *", "enabled": false}, false},
		{"stays disabled", map[string]interface{}{"schedule": "0 9 * * *", "enabled": false}, map[string]interface{}{"schedule": "0 9 * * *", "enabled": false}, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			stored := model.BotSchedule{ID: "s", Config: tt.stored}

			assert.Equal(t, tt.want, desired.Reschedules(stored, tt.update))
		})
	}
}

func TestScheduleToBot_IsTheScheduleAsABot(t *testing.T) {
	s := schedule("schedule-1", "0 9 * * *", time.Now())
	s.CustomBotVersion = model.CustomBotVersion{
		Version:   "1.2.0",
		FilePath:  "acme/1.2.0/code.zip",
		CreatedAt: "2026-10-01T00:00:00Z",
		UpdatedAt: "2026-10-02T00:00:00Z",
		Config: model.APIBotConfig{
			Name:        "acme",
			Description: "buys the dip",
			Runtime:     "python3.11",
			Version:     "1.2.0",
			Type:        "scheduled",
			Entrypoints: map[string]string{"bot": "main.py", "query": "query.py"},
		},
	}

	bot := desired.ScheduleToBot(s)

	assert.Equal(t, model.Bot{ID: s.ID, Config: s.Config, CustomBotVersion: s.CustomBotVersion}, bot)
}

func schedule(id, cron string, nextRun time.Time) model.BotSchedule {
	return model.BotSchedule{
		ID:                id,
		NextExecutionTime: nextRun.Unix(),
		Config:            map[string]interface{}{"schedule": cron},
	}
}

func disabled(s model.BotSchedule) model.BotSchedule {
	s.Config["enabled"] = false
	return s
}

func botIDs(bots []model.Bot) []string {
	ids := []string{}
	for _, bot := range bots {
		ids = append(ids, bot.ID)
	}
	return ids
}

func scheduleIDs(schedules []model.BotSchedule) []string {
	ids := []string{}
	for _, s := range schedules {
		ids = append(ids, s.ID)
	}
	return ids
}
