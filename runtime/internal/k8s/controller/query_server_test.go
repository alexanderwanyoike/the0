package controller

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"runtime/internal/model"
	"runtime/internal/query"
)

func TestK8sBotResolver_QueryEntrypoint(t *testing.T) {
	withEntrypoints := func(entrypoints map[string]string) model.CustomBotVersion {
		return model.CustomBotVersion{Config: model.APIBotConfig{Entrypoints: entrypoints}}
	}
	resolver := &k8sBotResolver{
		botRepo: &MockBotRepository{Bots: []model.Bot{
			{ID: "realtime-no-query", CustomBotVersion: withEntrypoints(map[string]string{"bot": "main.py"})},
		}},
		scheduleRepo: &MockBotScheduleRepository{Schedules: []model.BotSchedule{
			{ID: "scheduled-no-query", CustomBotVersion: withEntrypoints(map[string]string{"bot": "main.py"})},
			{ID: "scheduled-no-entrypoints", CustomBotVersion: withEntrypoints(nil)},
			{ID: "scheduled-with-query", CustomBotVersion: withEntrypoints(map[string]string{"bot": "main.py", "query": "query.py"})},
		}},
	}

	for _, botID := range []string{"realtime-no-query", "scheduled-no-query", "scheduled-no-entrypoints"} {
		t.Run("rejects "+botID+" as having no query entrypoint", func(t *testing.T) {
			_, err := resolver.ResolveBot(context.Background(), botID)

			require.ErrorIs(t, err, query.ErrNoQueryEntrypoint)
			assert.Equal(t, "bot has no query entrypoint: "+botID, err.Error())
		})
	}

	t.Run("resolves a scheduled bot with a query entrypoint", func(t *testing.T) {
		_, err := resolver.ResolveBot(context.Background(), "scheduled-with-query")

		assert.NoError(t, err)
	})

	t.Run("reports an unknown bot as not found", func(t *testing.T) {
		_, err := resolver.ResolveBot(context.Background(), "missing")

		require.Error(t, err)
		assert.NotErrorIs(t, err, query.ErrNoQueryEntrypoint)
		assert.Contains(t, err.Error(), "bot not found")
	})
}
