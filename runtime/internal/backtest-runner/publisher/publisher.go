package publisher

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"runtime/internal/util"

	"github.com/nats-io/nats.go"
)

type BacktestCompletedEvent struct {
	BacktestId string `json:"backtest_id"`
	Success    bool   `json:"success"`
	Error      string `json:"error,omitempty"`
}

type Publisher interface {
	PublishBacktestCompleted(ctx context.Context, event BacktestCompletedEvent) error
	Close() error
}

// NATSPublisher is a struct that implements the Publisher interface
type NATSPublisher struct {
	client *nats.Conn
	logger util.Logger
}

type NATSPublisherConfig struct {
	Logger util.Logger
}

func DefaultPublisherConfig() NATSPublisherConfig {
	return NATSPublisherConfig{
		Logger: &util.DefaultLogger{},
	}
}

const (
	SubjectName = "the0.backtest.completed"
)

// NewNATSPublisher creates a new NATS publisher
func NewNATSPublisher(
	config NATSPublisherConfig,
) (*NATSPublisher, error) {
	natsUrl := os.Getenv("NATS_URL")
	if natsUrl == "" {
		return nil, fmt.Errorf("NATS_URL environment variable is not set")
	}

	client, err := nats.Connect(natsUrl)
	if err != nil {
		return nil, fmt.Errorf("failed to create NATS client: %v", err)
	}

	return &NATSPublisher{
		client: client,
		logger: config.Logger,
	}, nil
}

// PublishBacktestCompleted publishes a backtest completion event to the NATS subject
func (p *NATSPublisher) PublishBacktestCompleted(
	ctx context.Context,
	event BacktestCompletedEvent,
) error {

	data, err := json.Marshal(event)
	if err != nil {
		return fmt.Errorf("failed to marshal event: %v", err)
	}

	// Publish message to NATS subject
	err = p.client.Publish(SubjectName, data)
	if err != nil {
		return fmt.Errorf("failed to publish message: %v", err)
	}

	p.logger.Info("Published message", "subject", SubjectName, "backtest_id", event.BacktestId)
	return nil
}

// Close closes the NATS client
func (p *NATSPublisher) Close() error {
	if p.client != nil {
		p.client.Close()
	}
	return nil
}

// MockPublisher is a mock implementation of the Publisher interface
type MockPublisher struct {
	PublishedEvents []BacktestCompletedEvent
}

func NewMockPublisher() *MockPublisher {
	return &MockPublisher{
		PublishedEvents: make([]BacktestCompletedEvent, 0),
	}
}

func (m *MockPublisher) PublishBacktestCompleted(
	ctx context.Context,
	event BacktestCompletedEvent,
) error {
	m.PublishedEvents = append(m.PublishedEvents, event)
	return nil
}

func (m *MockPublisher) Close() error {
	return nil
}
