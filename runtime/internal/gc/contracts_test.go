package gc

import (
	"context"
	"sort"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"runtime/internal/contracts"
)

type storageKey struct {
	BotID string `json:"botId"`
	Key   string `json:"key"`
}

func loadStorageKeys(t *testing.T) (logs, state []storageKey) {
	t.Helper()
	var keys struct {
		Logs  []storageKey `json:"logs"`
		State []storageKey `json:"state"`
	}
	contracts.Load(t, "storage-keys.json", &keys)
	require.NotEmpty(t, keys.Logs)
	require.NotEmpty(t, keys.State)
	return keys.Logs, keys.State
}

func botsOf(keys []storageKey) map[string]bool {
	bots := map[string]bool{}
	for _, key := range keys {
		bots[key.BotID] = true
	}
	return bots
}

// namesOf lists each key once, as a bucket does: several contract examples
// can name the same daily file.
func namesOf(keys []storageKey) []string {
	seen := map[string]bool{}
	var names []string
	for _, key := range keys {
		if !seen[key.Key] {
			seen[key.Key] = true
			names = append(names, key.Key)
		}
	}
	return names
}

func newContractGC(minio *mockMinIOClient) *GarbageCollector {
	return NewGarbageCollector(GarbageCollectorOptions{
		MinIO: minio, Store: &mockBotStore{}, LogsBucket: "bot-logs", StateBucket: "bot-state",
	})
}

// GC finds bots by their artifacts, so a key it cannot read back to its bot
// is never collected, and one it misreads is collected for the wrong bot.
func TestContracts_FindsTheBotOfEveryLogKey(t *testing.T) {
	logs, _ := loadStorageKeys(t)

	ids, err := newContractGC(&mockMinIOClient{logObjects: namesOf(logs)}).collectMinIOBotIDs(context.Background())

	require.NoError(t, err)
	assert.Equal(t, botsOf(logs), ids)
}

func TestContracts_FindsTheBotOfEveryStateKey(t *testing.T) {
	_, state := loadStorageKeys(t)

	ids, err := newContractGC(&mockMinIOClient{stateObjects: namesOf(state)}).collectMinIOBotIDs(context.Background())

	require.NoError(t, err)
	assert.Equal(t, botsOf(state), ids)
}

func TestContracts_DeletesAnOrphanedBotAtTheContractKeys(t *testing.T) {
	logs, state := loadStorageKeys(t)
	minio := &mockMinIOClient{logObjects: namesOf(logs), stateObjects: namesOf(state)}

	_, err := newContractGC(minio).Run(context.Background())

	require.NoError(t, err)
	want := map[string]bool{}
	for _, key := range logs {
		want["bot-logs:"+key.Key] = true
	}
	for _, key := range state {
		want["bot-state:"+key.Key] = true
	}
	wantNames := make([]string, 0, len(want))
	for name := range want {
		wantNames = append(wantNames, name)
	}
	sort.Strings(wantNames)
	got := append([]string(nil), minio.deleted...)
	sort.Strings(got)
	assert.Equal(t, wantNames, got)
}
