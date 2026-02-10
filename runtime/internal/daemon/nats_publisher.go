package daemon

import (
	"fmt"
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
	logger.Info("Connected to NATS for log publishing", "url", natsURL)
	return &NATSPublisher{conn: conn, logger: logger}, nil
}

// Publish sends log content to the bot's NATS log subject.
func (p *NATSPublisher) Publish(botID string, content string) error {
	subject := fmt.Sprintf("the0.bot.logs.%s", botID)
	return p.conn.Publish(subject, []byte(content))
}

// Close closes the NATS connection.
func (p *NATSPublisher) Close() error {
	if p.conn != nil {
		p.conn.Close()
	}
	return nil
}
