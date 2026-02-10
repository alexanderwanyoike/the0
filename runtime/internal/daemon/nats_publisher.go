package daemon

import (
	"fmt"
	"net/url"
	"time"

	"runtime/internal/util"

	"github.com/nats-io/nats.go"
)

// LogPublisher defines the interface for publishing bot log lines.
type LogPublisher interface {
	Publish(botID string, content string) error
	Close() error
}

// NATSPublisher publishes bot log lines to NATS subjects.
type NATSPublisher struct {
	conn   *nats.Conn
	logger util.Logger
}

// NewNATSPublisher creates a new NATS publisher for log streaming.
func NewNATSPublisher(natsURL string, logger util.Logger) (*NATSPublisher, error) {
	conn, err := nats.Connect(natsURL, nats.MaxReconnects(-1), nats.ReconnectWait(time.Second))
	if err != nil {
		return nil, fmt.Errorf("failed to connect to NATS: %w", err)
	}
	// Redact credentials before logging
	redacted := natsURL
	if u, parseErr := url.Parse(natsURL); parseErr == nil && u.User != nil {
		u.User = url.UserPassword("***", "***")
		redacted = u.String()
	}
	logger.Info("Connected to NATS for log publishing", "url", redacted)
	return &NATSPublisher{conn: conn, logger: logger}, nil
}

// Publish sends log content to the bot's NATS log subject.
func (p *NATSPublisher) Publish(botID string, content string) error {
	if p.conn == nil {
		return fmt.Errorf("NATS connection is closed")
	}
	subject := fmt.Sprintf("the0.bot.logs.%s", botID)
	return p.conn.Publish(subject, []byte(content))
}

// Close closes the NATS connection.
func (p *NATSPublisher) Close() error {
	if p.conn != nil {
		return p.conn.Drain()
	}
	return nil
}
