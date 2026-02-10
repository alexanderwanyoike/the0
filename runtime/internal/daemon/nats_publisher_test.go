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
