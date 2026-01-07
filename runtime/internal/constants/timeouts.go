package constants

import "time"

// Service timeouts and intervals
const (
	// ReconciliationTimeout is the max time for a full reconciliation cycle
	ReconciliationTimeout = 2 * time.Minute

	// ShutdownTimeout is the max time to wait for graceful shutdown
	ShutdownTimeout = 2 * time.Minute

	// ContainerStopTimeout is the max time to wait for a container to stop gracefully
	ContainerStopTimeout = 30 * time.Second

	// DefaultReconcileInterval is the default interval between reconciliation loops
	DefaultReconcileInterval = 30 * time.Second

	// DefaultCheckInterval is the default interval for schedule checks
	DefaultCheckInterval = 10 * time.Second

	// MongoConnectTimeout is the max time to wait for MongoDB connection
	MongoConnectTimeout = 10 * time.Second

	// MongoDisconnectTimeout is the max time to wait for MongoDB disconnection
	MongoDisconnectTimeout = 10 * time.Second

	// ExecutionWaitTimeout is the max time to wait for in-flight executions during shutdown
	ExecutionWaitTimeout = 30 * time.Second
)
