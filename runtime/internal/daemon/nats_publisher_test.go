package daemon

import (
	"sync"
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

func TestNATSPublisher_ConcurrentPublishAndClose(t *testing.T) {
	// Verify that concurrent Publish and Close calls don't cause a data race.
	// The publisher starts with a nil conn so no real NATS server is needed.
	p := &NATSPublisher{conn: nil, logger: &util.DefaultLogger{}}

	var wg sync.WaitGroup
	const goroutines = 50

	// Spawn goroutines that call Publish concurrently
	for i := 0; i < goroutines; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			_ = p.Publish("bot-1", "log line")
		}()
	}

	// Spawn goroutines that call Close concurrently
	for i := 0; i < goroutines; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			_ = p.Close()
		}()
	}

	wg.Wait()
}

func TestRedactNATSURL(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected string
	}{
		{"no credentials", "nats://localhost:4222", "nats://localhost:4222"},
		{"with credentials", "nats://user:pass@localhost:4222", "nats://REDACTED:REDACTED@localhost:4222"},
		{"multiple servers", "nats://user:pass@host1:4222,nats://user:pass@host2:4222", "nats://REDACTED:REDACTED@host1:4222,nats://REDACTED:REDACTED@host2:4222"},
		{"malformed with at sign", "user:pass@host:4222", "***@host:4222"},
		{"no at sign", "localhost:4222", "localhost:4222"},
		{"empty string", "", ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			assert.Equal(t, tt.expected, redactNATSURL(tt.input))
		})
	}
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
