package controller

import (
	"context"
	"runtime/internal/query"
	"runtime/internal/util"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	corev1 "k8s.io/api/core/v1"
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"
	"k8s.io/client-go/kubernetes/fake"
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

type stubQueryResultManager struct {
	result     []byte
	deletedKey string
}

func (s *stubQueryResultManager) Upload(ctx context.Context, key string, data []byte) error {
	return nil
}

func (s *stubQueryResultManager) Download(ctx context.Context, key string) ([]byte, error) {
	if s.result == nil {
		return nil, assert.AnError
	}
	return s.result, nil
}

func (s *stubQueryResultManager) Delete(ctx context.Context, key string) error {
	s.deletedKey = key
	return nil
}

func TestK8sQueryHandler_JobResponse(t *testing.T) {
	const jobName = "query-bot-1"
	const resultKey = "bot/1/result.json"
	jobPod := &corev1.Pod{ObjectMeta: metav1.ObjectMeta{
		Name:      jobName + "-abcde",
		Namespace: "the0",
		Labels:    map[string]string{"job-name": jobName},
	}}
	newHandler := func(results *stubQueryResultManager) *K8sQueryHandler {
		return &K8sQueryHandler{
			clientset:     fake.NewSimpleClientset(jobPod),
			namespace:     "the0",
			resultManager: results,
			logger:        &util.DefaultLogger{},
		}
	}

	t.Run("failed job returns the bot's own error and its available paths", func(t *testing.T) {
		results := &stubQueryResultManager{
			result: []byte(`{"status":"error","error":"No handler for path: /status","available":["/today","/history"]}`),
		}

		response := newHandler(results).jobResponse(context.Background(), jobName, resultKey, true, time.Now())

		assert.Equal(t, "error", response.Status)
		assert.Equal(t, "No handler for path: /status (available: /today, /history)", response.Error)
		assert.Equal(t, resultKey, results.deletedKey)
	})

	t.Run("failed job without a result reports the pod logs", func(t *testing.T) {
		response := newHandler(&stubQueryResultManager{}).jobResponse(context.Background(), jobName, resultKey, true, time.Now())

		assert.Equal(t, "error", response.Status)
		assert.Equal(t, "query job failed: fake logs", response.Error)
	})

	t.Run("succeeded job returns the result", func(t *testing.T) {
		results := &stubQueryResultManager{result: []byte(`{"status":"ok","data":{"healthy":true}}`)}

		response := newHandler(results).jobResponse(context.Background(), jobName, resultKey, false, time.Now())

		assert.Equal(t, "ok", response.Status)
		assert.JSONEq(t, `{"healthy":true}`, string(response.Data))
		assert.Equal(t, resultKey, results.deletedKey)
	})
}
