package docker

import (
	"context"
	"fmt"
	"time"
)

// MockConfigLoader provides a mock configuration for testing.
type MockConfigLoader struct {
	Config *DockerRunnerConfig
	Err    error
}

// LoadConfig implements ConfigLoader interface.
func (m *MockConfigLoader) LoadConfig() (*DockerRunnerConfig, error) {
	if m.Err != nil {
		return nil, m.Err
	}
	if m.Config != nil {
		return m.Config, nil
	}
	// Return default test config
	return &DockerRunnerConfig{
		MinIOEndpoint:          "localhost:9000",
		MinIOContainerEndpoint: "", // Uses MinIOEndpoint if empty
		MinIOAccessKeyID:       "minioadmin",
		MinIOSecretAccessKey:   "minioadmin",
		MinIOUseSSL:            false,
		MinIOCodeBucket:        "custom-bots",
		MinioResultsBucket:     "backtests",
		MinioLogsBucket:        "bot-logs",
		MinioStateBucket:       "bot-state",
		MaxStateSizeBytes:      8 * 1024 * 1024 * 1024, // 8GB
		MaxStateFileSizeBytes:  10 * 1024 * 1024,       // 10MB
		TempDir:                "/tmp",
		MemoryLimitMB:          512 * 1024 * 1024, // 512MB
		CPUShares:              512,
		DockerNetwork:          "",
	}, nil
}

// NewMockConfig creates a DockerRunnerConfig for testing.
func NewMockConfig() *DockerRunnerConfig {
	return &DockerRunnerConfig{
		MinIOEndpoint:          "localhost:9000",
		MinIOContainerEndpoint: "", // Uses MinIOEndpoint if empty
		MinIOAccessKeyID:       "minioadmin",
		MinIOSecretAccessKey:   "minioadmin",
		MinIOUseSSL:            false,
		MinIOCodeBucket:        "custom-bots",
		MinioResultsBucket:     "backtests",
		MinioLogsBucket:        "bot-logs",
		MinioStateBucket:       "bot-state",
		MaxStateSizeBytes:      8 * 1024 * 1024 * 1024, // 8GB
		MaxStateFileSizeBytes:  10 * 1024 * 1024,       // 10MB
		TempDir:                "/tmp",
		MemoryLimitMB:          512 * 1024 * 1024, // 512MB
		CPUShares:              512,
		DockerNetwork:          "",
	}
}

// WaitFor waits for a condition to become true or times out.
// condition: function that returns true when the condition is met
// timeout: maximum duration to wait
// interval: how often to check the condition
// Returns error if timeout is reached before condition becomes true.
func WaitFor(condition func() bool, timeout time.Duration, interval time.Duration) error {
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()

	ticker := time.NewTicker(interval)
	defer ticker.Stop()

	// Check immediately first
	if condition() {
		return nil
	}

	for {
		select {
		case <-ctx.Done():
			return fmt.Errorf("timeout waiting for condition")
		case <-ticker.C:
			if condition() {
				return nil
			}
		}
	}
}
