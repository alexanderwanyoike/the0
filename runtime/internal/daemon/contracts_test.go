package daemon

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"runtime/internal/contracts"
)

// The api subscribes to a bot's logs by its ID as is, so a valid ID must
// reach the subject unchanged.
func TestContracts_ValidBotIDsNameTheirLogSubjectUnchanged(t *testing.T) {
	var ids struct {
		Valid   []string `json:"valid"`
		Invalid []string `json:"invalid"`
	}
	contracts.Load(t, "bot-ids.json", &ids)

	for _, id := range ids.Valid {
		assert.Equal(t, id, sanitizeBotID(id), "valid %q", id)
	}
	for _, id := range ids.Invalid {
		assert.NotEqual(t, id, sanitizeBotID(id), "invalid %q", id)
	}
}

func TestContracts_PublishesLogsOnTheSubjectTheApiSubscribesTo(t *testing.T) {
	var contract struct {
		BotID   string `json:"botId"`
		Subject string `json:"subject"`
	}
	contracts.Load(t, "log-lines.json", &contract)

	assert.Equal(t, contract.Subject, logSubject(contract.BotID))
}
