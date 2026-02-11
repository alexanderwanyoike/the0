package daemon

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"runtime/internal/util"
)

func TestNewNATSPublisher_InvalidURL(t *testing.T) {
	publisher, err := NewNATSPublisher("nats://invalid:99999", &util.DefaultLogger{})
	assert.Error(t, err, "should return error for invalid NATS URL")
	assert.Nil(t, publisher, "publisher should be nil on error")
}

func TestNATSPublisher_Publish_NilConn(t *testing.T) {
	// Verify Publish returns error with nil connection instead of panicking
	p := &NATSPublisher{conn: nil, logger: &util.DefaultLogger{}}
	err := p.Publish("bot-1", "test log line")
	assert.Error(t, err, "Publish should return error with nil connection")
	assert.Contains(t, err.Error(), "NATS connection is closed")
}

func TestNATSPublisher_Close_NilConn(t *testing.T) {
	// Verify Close doesn't panic with nil connection
	p := &NATSPublisher{conn: nil, logger: &util.DefaultLogger{}}
	err := p.Close()
	assert.NoError(t, err, "Close should not error with nil connection")
}

func TestNATSPublisher_ImplementsLogPublisher(t *testing.T) {
	// Compile-time check that NATSPublisher satisfies LogPublisher
	var _ LogPublisher = (*NATSPublisher)(nil)
}

func TestSanitizeBotID(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{"simple-bot", "simple-bot"},
		{"bot_123", "bot_123"},
		{"bot.with.dots", "bot_with_dots"},
		{"bot*wildcard", "bot_wildcard"},
		{"bot>greater", "bot_greater"},
		{"bot with spaces", "bot_with_spaces"},
		{"", "unknown"},
		{"CamelCase123", "CamelCase123"},
	}
	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			assert.Equal(t, tt.expected, sanitizeBotID(tt.input))
		})
	}
}
