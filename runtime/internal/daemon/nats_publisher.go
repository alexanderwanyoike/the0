package daemon

import (
	"fmt"
	"net/url"
	"strings"
	"sync"
	"time"

	"runtime/internal/util"

	"github.com/nats-io/nats.go"
)

// LogPublisher defines the interface for publishing bot log content chunks.
type LogPublisher interface {
	Publish(botID string, content string) error
	Close() error
}

// NATSPublisher publishes bot log lines to NATS subjects.
type NATSPublisher struct {
	conn   *nats.Conn
	logger util.Logger
	mu     sync.RWMutex
}

// NewNATSPublisher creates a new NATS publisher for log streaming.
func NewNATSPublisher(natsURL string, logger util.Logger) (*NATSPublisher, error) {
	if logger == nil {
		logger = util.NewLogger()
	}
	conn, err := nats.Connect(natsURL, nats.MaxReconnects(-1), nats.ReconnectWait(time.Second))
	if err != nil {
		return nil, fmt.Errorf("failed to connect to NATS: %w", err)
	}
	logger.Info("Connected to NATS for log publishing", "url", redactNATSURL(natsURL))
	return &NATSPublisher{conn: conn, logger: logger}, nil
}

// redactNATSURL redacts credentials from a NATS URL string,
// handling comma-separated server lists and malformed URLs.
func redactNATSURL(rawURL string) string {
	parts := strings.Split(rawURL, ",")
	for i, part := range parts {
		part = strings.TrimSpace(part)
		if u, err := url.Parse(part); err == nil && u.User != nil {
			u.User = url.UserPassword("REDACTED", "REDACTED")
			parts[i] = u.String()
		} else {
			parts[i] = redactUserInfo(part)
		}
	}
	return strings.Join(parts, ",")
}

// redactUserInfo redacts potential credentials in URL-like strings
// when standard URL parsing fails (e.g., malformed "user:pass@host").
func redactUserInfo(s string) string {
	if at := strings.Index(s, "@"); at != -1 {
		return "***@" + s[at+1:]
	}
	return s
}

// sanitizeBotID replaces NATS-unsafe characters (., *, >, whitespace) with underscores.
func sanitizeBotID(botID string) string {
	if botID == "" {
		return "unknown"
	}
	return strings.Map(func(r rune) rune {
		if (r >= 'a' && r <= 'z') || (r >= 'A' && r <= 'Z') ||
			(r >= '0' && r <= '9') || r == '-' || r == '_' {
			return r
		}
		return '_'
	}, botID)
}

// Publish sends log content to the bot's NATS log subject.
// It is safe to call concurrently with Close.
func (p *NATSPublisher) Publish(botID string, content string) error {
	p.mu.RLock()
	defer p.mu.RUnlock()
	if p.conn == nil {
		return fmt.Errorf("NATS connection is closed")
	}
	subject := fmt.Sprintf("the0.bot.logs.%s", sanitizeBotID(botID))
	return p.conn.Publish(subject, []byte(content))
}

// Close drains and closes the NATS connection.
// It is safe to call concurrently with Publish.
func (p *NATSPublisher) Close() error {
	p.mu.Lock()
	defer p.mu.Unlock()
	if p.conn != nil {
		err := p.conn.Drain()
		p.conn = nil
		return err
	}
	return nil
}
