package miniologger

import (
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"runtime/internal/contracts"
)

func TestContracts_FormatsAChunkAsTheApiReadsIt(t *testing.T) {
	var contract contracts.LogLines
	contracts.Load(t, "log-lines.json", &contract)
	at, err := time.Parse(time.RFC3339Nano, contract.Chunk.At)
	require.NoError(t, err)

	assert.Equal(t, contract.Chunk.Formatted, FormatLogChunk(contract.Chunk.Raw, at))
}

func TestContracts_WritesEachDaysLogsWhereTheApiReadsThem(t *testing.T) {
	var keys contracts.StorageKeys
	contracts.Load(t, "storage-keys.json", &keys)
	require.NotEmpty(t, keys.Logs)

	for _, example := range keys.Logs {
		at, err := time.Parse(time.RFC3339Nano, example.At)
		require.NoError(t, err)
		assert.Equal(t, example.Key, (&minIOLogger{}).GetLogFilePath(example.BotID, at), example.At)
	}
}
