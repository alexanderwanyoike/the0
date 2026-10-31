package storage

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"runtime/internal/contracts"
)

func TestContracts_WritesStateWhereTheApiReadsIt(t *testing.T) {
	var keys struct {
		State []struct {
			BotID string `json:"botId"`
			Key   string `json:"key"`
		} `json:"state"`
	}
	contracts.Load(t, "storage-keys.json", &keys)
	require.NotEmpty(t, keys.State)

	for _, example := range keys.State {
		assert.Equal(t, example.Key, (&stateManager{}).objectName(example.BotID))
	}
}
