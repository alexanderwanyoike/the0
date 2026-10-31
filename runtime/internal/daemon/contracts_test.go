package daemon

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

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

// subjectMatches applies NATS wildcards: "*" is one token, a trailing ">"
// one or more.
func subjectMatches(pattern, subject string) bool {
	patternTokens, subjectTokens := strings.Split(pattern, "."), strings.Split(subject, ".")
	for i, token := range patternTokens {
		if token == ">" {
			return len(subjectTokens) > i
		}
		if i >= len(subjectTokens) || (token != "*" && token != subjectTokens[i]) {
			return false
		}
	}
	return len(subjectTokens) == len(patternTokens)
}

// A subject outside THE0_BOT_LOGS is never stored for the api to read.
func TestContracts_PublishesLogsOnASubjectTheLogStreamHolds(t *testing.T) {
	var streams map[string][]string
	contracts.Load(t, "events/streams.json", &streams)
	var ids struct {
		Valid []string `json:"valid"`
	}
	contracts.Load(t, "bot-ids.json", &ids)
	require.NotEmpty(t, streams["THE0_BOT_LOGS"])

	for _, id := range ids.Valid {
		subject := logSubject(id)
		held := false
		for _, pattern := range streams["THE0_BOT_LOGS"] {
			held = held || subjectMatches(pattern, subject)
		}
		assert.True(t, held, "%s is outside THE0_BOT_LOGS %v", subject, streams["THE0_BOT_LOGS"])
	}
}
