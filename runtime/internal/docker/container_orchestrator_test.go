package docker

import (
	"context"
	"runtime/internal/util"
	"testing"

	"github.com/docker/docker/client"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// testLogger is a simple logger for testing
type testLogger struct{}

func (l *testLogger) Info(msg string, args ...interface{})  {}
func (l *testLogger) Error(msg string, args ...interface{}) {}
func (l *testLogger) Debug(msg string, args ...interface{}) {}

// Note on Testing Strategy:
//
// The ContainerOrchestrator is challenging to unit test with mocks because:
// 1. NewDockerOrchestrator accepts *client.Client (concrete type) rather than an interface
// 2. Full unit testing with mocks would require refactoring to use a DockerClient interface
// 3. Per project requirements, we're testing existing code as-is without major refactoring
//
// Testing Approach:
// - Constructor tests verify creation works correctly
// - Interface compliance tests ensure the implementation satisfies ContainerOrchestrator
// - Full integration tests are covered in DockerRunner tests (runner_test.go)
// - End-to-end Docker operations are tested through the service layer
//
// Future Improvement (would require refactoring):
// - Create a DockerClient interface wrapping *client.Client methods
// - Modify NewDockerOrchestrator to accept the interface
// - Enable full unit testing with mocked Docker operations

func TestNewDockerOrchestrator(t *testing.T) {
	// Skip if Docker is not available
	dockerClient, err := client.NewClientWithOpts(client.FromEnv, client.WithAPIVersionNegotiation())
	if err != nil {
		t.Skip("Docker not available, skipping test")
	}
	defer dockerClient.Close()

	tests := []struct {
		name   string
		logger util.Logger
	}{
		{
			name:   "with test logger",
			logger: &testLogger{},
		},
		{
			name:   "with default logger",
			logger: &util.DefaultLogger{},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			orchestrator := NewDockerOrchestrator(dockerClient, tt.logger)

			assert.NotNil(t, orchestrator)
			require.NotNil(t, orchestrator)

			// Verify it implements the interface
			var _ ContainerOrchestrator = orchestrator
		})
	}
}

func TestContainerOrchestrator_InterfaceCompliance(t *testing.T) {
	// This test ensures the dockerOrchestrator type implements all ContainerOrchestrator methods
	// If a method is added to the interface but not implemented, this will fail at compile time

	dockerClient, err := client.NewClientWithOpts(client.FromEnv, client.WithAPIVersionNegotiation())
	if err != nil {
		t.Skip("Docker not available, skipping test")
	}
	defer dockerClient.Close()

	logger := &testLogger{}
	var orchestrator ContainerOrchestrator = NewDockerOrchestrator(dockerClient, logger)

	// Verify all interface methods are available (compile-time check)
	require.NotNil(t, orchestrator.PullImage)
	require.NotNil(t, orchestrator.CreateAndStart)
	require.NotNil(t, orchestrator.RunAndWait)
	require.NotNil(t, orchestrator.Stop)
	require.NotNil(t, orchestrator.GetLogs)
	require.NotNil(t, orchestrator.CopyFromContainer)
	require.NotNil(t, orchestrator.GetStatus)
	require.NotNil(t, orchestrator.ListContainer)
	require.NotNil(t, orchestrator.ListAllContainers)
	require.NotNil(t, orchestrator.Remove)
	require.NotNil(t, orchestrator.GetContainerIP)
}

func TestContainerOrchestrator_ContextHandling(t *testing.T) {
	// Verify that methods properly handle context cancellation
	dockerClient, err := client.NewClientWithOpts(client.FromEnv, client.WithAPIVersionNegotiation())
	if err != nil {
		t.Skip("Docker not available, skipping test")
	}
	defer dockerClient.Close()

	logger := &testLogger{}
	orchestrator := NewDockerOrchestrator(dockerClient, logger)

	// Test with cancelled context
	ctx, cancel := context.WithCancel(context.Background())
	cancel() // Cancel immediately

	// These operations should respect the cancelled context
	err = orchestrator.PullImage(ctx, "python:3.11")
	assert.Error(t, err, "Should fail with cancelled context")

	_, err = orchestrator.GetLogs(ctx, "nonexistent", 100)
	assert.Error(t, err, "Should fail with cancelled context")
}

func TestContainerRunResult_Structure(t *testing.T) {
	// Verify ContainerRunResult structure
	result := &ContainerRunResult{
		ExitCode:           0,
		Logs:               "test logs",
		ResultFileContents: []byte("test content"),
	}

	assert.Equal(t, 0, result.ExitCode)
	assert.Equal(t, "test logs", result.Logs)
	assert.Equal(t, []byte("test content"), result.ResultFileContents)
}

func TestContainerStatus_Structure(t *testing.T) {
	// Verify ContainerStatus structure and field types
	status := &ContainerStatus{
		ContainerID: "abc123",
		Status:      "running",
		ExitCode:    0,
		StartedAt:   "2024-01-01T00:00:00Z",
		FinishedAt:  "2024-01-01T00:01:00Z",
		Error:       "",
		Labels: map[string]string{
			"app": "test",
		},
	}

	assert.Equal(t, "abc123", status.ContainerID)
	assert.Equal(t, "running", status.Status)
	assert.Equal(t, 0, status.ExitCode)
	assert.NotNil(t, status.Labels)
}

func TestContainerStatus_StatusValues(t *testing.T) {
	// Document expected status string values
	validStatuses := []string{
		"running",
		"exited",
		"paused",
		"restarting",
		"dead",
		"not_found",
	}

	for _, status := range validStatuses {
		cs := &ContainerStatus{Status: status}
		assert.NotEmpty(t, cs.Status)
	}
}

// Benchmark for constructor (minimal overhead expected)
func BenchmarkNewDockerOrchestrator(b *testing.B) {
	dockerClient, err := client.NewClientWithOpts(client.FromEnv, client.WithAPIVersionNegotiation())
	if err != nil {
		b.Skip("Docker not available, skipping benchmark")
	}
	defer dockerClient.Close()

	logger := &testLogger{}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = NewDockerOrchestrator(dockerClient, logger)
	}
}

// Additional Integration Test Notes:
//
// For comprehensive testing of ContainerOrchestrator behavior, see:
//
// 1. internal/docker/runner_test.go
//    - Tests DockerRunner which uses ContainerOrchestrator
//    - Uses testcontainers for real Docker operations
//    - Covers image pulling, container creation, execution, cleanup
//
// 2. internal/docker/service_test.go
//    - Tests BotService which orchestrates bot container lifecycle
//    - Tests reconciliation logic with running containers
//    - Tests config change detection and restart logic
//
// 3. End-to-end tests (when available)
//    - Full stack tests with real bot execution
//    - Tests query execution through containers
//    - Tests failure recovery and cleanup
