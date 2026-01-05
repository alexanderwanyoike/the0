package controller

import (
	"runtime/internal/query"
	"testing"

	"github.com/stretchr/testify/assert"
)

// Note: K8sQueryHandler realtime query execution is delegated to query.RealtimeExecutor
// which is tested in the query package. The K8s handler tests focus on K8s-specific
// functionality like Job creation for scheduled queries.
//
// Integration tests for the full K8s query flow require a Kubernetes cluster
// and should be run as part of e2e testing.

func TestK8sQueryHandler_Config(t *testing.T) {
	handler := NewK8sQueryHandler(K8sQueryHandlerConfig{
		Namespace: "test-namespace",
	})

	// Verify handler was created with correct namespace
	assert.Equal(t, "test-namespace", handler.namespace)
	assert.NotNil(t, handler.realtimeExecutor)
	assert.NotNil(t, handler.logger)
}

func TestK8sQueryHandler_DefaultConfig(t *testing.T) {
	handler := NewK8sQueryHandler(K8sQueryHandlerConfig{})

	// Verify defaults are applied
	assert.Equal(t, "the0", handler.namespace)
	assert.NotNil(t, handler.realtimeExecutor)
	assert.NotNil(t, handler.logger)
}

func TestQueryRequest_Defaults(t *testing.T) {
	// Verify default timeout constant is accessible
	assert.Equal(t, 30, query.DefaultTimeout)
	assert.Equal(t, 9476, query.DefaultQueryPort)
}
