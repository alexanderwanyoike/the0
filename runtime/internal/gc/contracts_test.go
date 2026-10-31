package gc

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"runtime/internal/contracts"
)

// GC finds bots by their artifacts, so a key it cannot read back to its bot
// is never collected, and one it misreads is collected for the wrong bot.
func TestContracts_FindsTheBotOfEveryStorageKey(t *testing.T) {
	var keys struct {
		Logs []struct {
			BotID string `json:"botId"`
			Key   string `json:"key"`
		} `json:"logs"`
		State []struct {
			BotID string `json:"botId"`
			Key   string `json:"key"`
		} `json:"state"`
	}
	contracts.Load(t, "storage-keys.json", &keys)

	minio := &mockMinIOClient{}
	want := map[string]bool{}
	for _, example := range keys.Logs {
		minio.logObjects = append(minio.logObjects, example.Key)
		want[example.BotID] = true
	}
	for _, example := range keys.State {
		minio.stateObjects = append(minio.stateObjects, example.Key)
		want[example.BotID] = true
	}
	gc := NewGarbageCollector(GarbageCollectorOptions{MinIO: minio, LogsBucket: "bot-logs", StateBucket: "bot-state"})

	ids, err := gc.collectMinIOBotIDs(context.Background())

	require.NoError(t, err)
	assert.Equal(t, want, ids)
}
