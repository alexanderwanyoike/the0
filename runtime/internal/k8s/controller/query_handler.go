// Package controller provides query execution for bot pods in Kubernetes.
package controller

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"time"

	"github.com/minio/minio-go/v7"
	batchv1 "k8s.io/api/batch/v1"
	corev1 "k8s.io/api/core/v1"
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"
	"k8s.io/client-go/kubernetes"

	"runtime/internal/k8s/podgen"
	"runtime/internal/model"
	"runtime/internal/query"
	"runtime/internal/runtime/storage"
	"runtime/internal/util"
)

// K8sQueryHandler executes queries against bot pods in Kubernetes.
// For scheduled bots, it creates ephemeral Jobs with QUERY_PATH set.
// For realtime bots, it proxies to the bot's query HTTP server on port 9476.
type K8sQueryHandler struct {
	clientset        kubernetes.Interface
	namespace        string
	podGenerator     *podgen.PodGenerator
	realtimeExecutor *query.RealtimeExecutor
	resultManager    storage.QueryResultManager
	logger           util.Logger
}

// K8sQueryHandlerConfig contains configuration for the K8sQueryHandler.
type K8sQueryHandlerConfig struct {
	Clientset    *kubernetes.Clientset
	Namespace    string
	PodGenerator *podgen.PodGenerator
	MinioClient  *minio.Client
	StorageConfig *storage.Config
	Logger       util.Logger
}

// NewK8sQueryHandler creates a new K8sQueryHandler instance.
func NewK8sQueryHandler(config K8sQueryHandlerConfig) *K8sQueryHandler {
	if config.Logger == nil {
		config.Logger = &util.DefaultLogger{}
	}
	if config.Namespace == "" {
		config.Namespace = "the0"
	}
	return &K8sQueryHandler{
		clientset:        config.Clientset,
		namespace:        config.Namespace,
		podGenerator:     config.PodGenerator,
		realtimeExecutor: query.NewRealtimeExecutor(query.DefaultQueryPort, config.Logger),
		resultManager:    storage.NewQueryResultManager(config.MinioClient, config.StorageConfig, config.Logger),
		logger:           config.Logger,
	}
}

// ExecuteQuery executes a query against a bot pod.
// For scheduled bots: Creates a Job with QUERY_PATH and QUERY_PARAMS set.
// For realtime bots: Proxies the request to the pod's query HTTP server.
func (h *K8sQueryHandler) ExecuteQuery(ctx context.Context, req query.Request, bot model.Bot, podIP string) (*query.Response, error) {
	start := time.Now()

	// Set defaults
	if req.TimeoutSec <= 0 {
		req.TimeoutSec = query.DefaultTimeout
	}

	// Create timeout context
	queryCtx, cancel := context.WithTimeout(ctx, time.Duration(req.TimeoutSec)*time.Second)
	defer cancel()

	// Determine if this is a realtime bot (has a running pod with IP)
	isRealtime := podIP != ""

	if isRealtime {
		return h.realtimeExecutor.Execute(queryCtx, req, podIP), nil
	}

	// For scheduled bots, create an ephemeral Job
	return h.executeScheduledQuery(queryCtx, req, bot, start)
}

// executeScheduledQuery creates a Kubernetes Job to execute the query.
func (h *K8sQueryHandler) executeScheduledQuery(ctx context.Context, req query.Request, bot model.Bot, start time.Time) (*query.Response, error) {
	h.logger.Info("Executing scheduled query via K8s Job: bot=%s path=%s", req.BotID, req.QueryPath)

	// Generate a unique key for storing the result in MinIO
	resultKey := fmt.Sprintf("%s/%d/result.json", bot.ID, time.Now().UnixNano())

	// Generate pod spec for the query
	pod, err := h.podGenerator.GenerateQueryPod(bot, req.QueryPath, req.Params)
	if err != nil {
		return query.ErrorResponse(fmt.Sprintf("failed to generate query pod spec: %v", err), start), nil
	}

	// Add QUERY_RESULT_KEY env var to the bot container
	for i := range pod.Spec.Containers {
		if pod.Spec.Containers[i].Name == "bot" {
			pod.Spec.Containers[i].Env = append(pod.Spec.Containers[i].Env,
				corev1.EnvVar{Name: "QUERY_RESULT_KEY", Value: resultKey})
			break
		}
	}

	// Create a Job from the pod spec
	jobName := fmt.Sprintf("query-%s-%d", bot.ID, time.Now().UnixNano())
	if len(jobName) > 63 {
		jobName = jobName[:63] // K8s name limit
	}

	backoffLimit := int32(0)
	ttlSeconds := int32(300) // Clean up after 5 minutes

	job := &batchv1.Job{
		ObjectMeta: metav1.ObjectMeta{
			Name:      jobName,
			Namespace: h.namespace,
			Labels: map[string]string{
				podgen.LabelManagedBy: "the0-query-handler",
				podgen.LabelBotID:     bot.ID,
				"the0.app/query":      "true",
			},
		},
		Spec: batchv1.JobSpec{
			BackoffLimit:            &backoffLimit,
			TTLSecondsAfterFinished: &ttlSeconds,
			Template: corev1.PodTemplateSpec{
				ObjectMeta: pod.ObjectMeta,
				Spec:       pod.Spec,
			},
		},
	}

	// Create the Job
	createdJob, err := h.clientset.BatchV1().Jobs(h.namespace).Create(ctx, job, metav1.CreateOptions{})
	if err != nil {
		return query.ErrorResponse(fmt.Sprintf("failed to create query job: %v", err), start), nil
	}

	failed, err := h.waitForJob(ctx, createdJob.Name)
	if err != nil {
		return query.ErrorResponse(fmt.Sprintf("query job failed: %v", err), start), nil
	}

	return h.jobResponse(ctx, createdJob.Name, resultKey, failed, start), nil
}

// waitForJob waits for a Job to finish and reports whether it failed.
func (h *K8sQueryHandler) waitForJob(ctx context.Context, jobName string) (bool, error) {
	ticker := time.NewTicker(500 * time.Millisecond)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			return false, ctx.Err()
		case <-ticker.C:
			job, err := h.clientset.BatchV1().Jobs(h.namespace).Get(ctx, jobName, metav1.GetOptions{})
			if err != nil {
				return false, fmt.Errorf("failed to get job status: %w", err)
			}
			if job.Status.Succeeded > 0 {
				return false, nil
			}
			if job.Status.Failed > 0 {
				return true, nil
			}
		}
	}
}

// jobResponse builds the query response for a finished Job from the result it
// left in MinIO, falling back to the pod logs.
func (h *K8sQueryHandler) jobResponse(ctx context.Context, jobName, resultKey string, failed bool, start time.Time) *query.Response {
	result, err := h.takeResult(ctx, resultKey)
	if failed {
		return query.FailedRunResponse(result, func() string {
			logs, _ := h.getJobPodLogs(ctx, jobName)
			return fmt.Sprintf("query job failed: %s", logs)
		}, start)
	}
	if err != nil {
		logs, _ := h.getJobPodLogs(ctx, jobName)
		return query.ErrorResponse(fmt.Sprintf("query job failed: failed to download result from MinIO: %v\nLogs: %s", err, logs), start)
	}

	return query.ParseQueryOutput(result, start)
}

// takeResult downloads a query result from MinIO and deletes it.
func (h *K8sQueryHandler) takeResult(ctx context.Context, resultKey string) ([]byte, error) {
	data, err := h.resultManager.Download(ctx, resultKey)
	if err != nil {
		return nil, err
	}

	if delErr := h.resultManager.Delete(ctx, resultKey); delErr != nil {
		h.logger.Info("Warning: failed to delete query result from MinIO: %v", delErr)
	}

	return data, nil
}

// getJobPodLogs retrieves logs from the pod created by a Job (fallback for errors).
func (h *K8sQueryHandler) getJobPodLogs(ctx context.Context, jobName string) (string, error) {
	// List pods with job-name label
	pods, err := h.clientset.CoreV1().Pods(h.namespace).List(ctx, metav1.ListOptions{
		LabelSelector: fmt.Sprintf("job-name=%s", jobName),
	})
	if err != nil {
		return "", fmt.Errorf("failed to list job pods: %w", err)
	}

	if len(pods.Items) == 0 {
		return "", fmt.Errorf("no pods found for job %s", jobName)
	}

	// Get logs from the first pod's main container
	pod := pods.Items[0]
	req := h.clientset.CoreV1().Pods(h.namespace).GetLogs(pod.Name, &corev1.PodLogOptions{
		Container: "bot", // Main container name from pod generator
	})

	stream, err := req.Stream(ctx)
	if err != nil {
		return "", fmt.Errorf("failed to get pod logs: %w", err)
	}
	defer stream.Close()

	buf := new(bytes.Buffer)
	_, err = io.Copy(buf, stream)
	if err != nil {
		return "", fmt.Errorf("failed to read pod logs: %w", err)
	}

	return buf.String(), nil
}
