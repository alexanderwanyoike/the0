package miniologger

import (
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"runtime/internal/contracts"
)

func TestContracts_FormatsAChunkAsTheApiReadsIt(t *testing.T) {
	var contract struct {
		Chunk struct {
			At        string `json:"at"`
			Raw       string `json:"raw"`
			Formatted string `json:"formatted"`
		} `json:"chunk"`
	}
	contracts.Load(t, "log-lines.json", &contract)
	at, err := time.Parse(time.RFC3339Nano, contract.Chunk.At)
	require.NoError(t, err)

	assert.Equal(t, contract.Chunk.Formatted, FormatLogChunk(contract.Chunk.Raw, at))
}
