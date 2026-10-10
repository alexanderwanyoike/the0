package controller

import (
	"context"
	"net/http"
	"runtime/internal/k8s/podgen"
	"runtime/internal/model"
	"runtime/internal/query"
	"runtime/internal/util"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
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

func TestK8sQueryHandler_ScheduledQueryWithoutQueryEntrypoint(t *testing.T) {
	handler := &K8sQueryHandler{
		namespace: "the0",
		podGenerator: podgen.NewPodGenerator(podgen.PodGeneratorConfig{
			Namespace:      "the0",
			MinIOEndpoint:  "minio:9000",
			MinIOAccessKey: "key",
			MinIOSecretKey: "secret",
			MinIOBucket:    "bots",
			RuntimeImage:   "the0/runtime:latest",
		}),
		logger: &util.DefaultLogger{},
	}
	bot := model.Bot{
		ID: "no-query",
		CustomBotVersion: model.CustomBotVersion{
			Version:  "1.0.0",
			FilePath: "bot/1.0.0",
			Config: model.APIBotConfig{
				Name:        "bot",
				Runtime:     "python3.11",
				Entrypoints: map[string]string{"bot": "main.py"},
			},
		},
	}

	response, err := handler.ExecuteQuery(context.Background(), query.Request{BotID: "no-query", QueryPath: "/health"}, bot, "")
	require.NoError(t, err)

	assert.Equal(t, http.StatusUnprocessableEntity, response.HTTPStatus())
	assert.Equal(t, "bot has no query entrypoint: no-query", response.Error)
}
