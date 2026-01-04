// Package controller provides query execution for bot pods in Kubernetes.
package controller

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"time"

	batchv1 "k8s.io/api/batch/v1"
	corev1 "k8s.io/api/core/v1"
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"
	"k8s.io/client-go/kubernetes"

	"runtime/internal/k8s/podgen"
	"runtime/internal/model"
	"runtime/internal/util"
)

// QueryRequest represents a query to execute against a bot.
type QueryRequest struct {
	BotID      string                 `json:"bot_id"`
	QueryPath  string                 `json:"query_path"`
	Params     map[string]interface{} `json:"params,omitempty"`
	TimeoutSec int                    `json:"timeout_sec,omitempty"` // Default: 30 seconds
}

// QueryResponse represents the result of a query execution.
type QueryResponse struct {
	Status    string          `json:"status"`              // "ok" or "error"
	Data      json.RawMessage `json:"data,omitempty"`      // Query result data
	Error     string          `json:"error,omitempty"`     // Error message if status is "error"
	Duration  time.Duration   `json:"duration"`            // Execution time
	Timestamp time.Time       `json:"timestamp"`           // When the query was executed
}

// K8sQueryHandler executes queries against bot pods in Kubernetes.
// For scheduled bots, it creates ephemeral Jobs with QUERY_PATH set.
// For realtime bots, it proxies to the bot's query HTTP server on port 9476.
type K8sQueryHandler struct {
	clientset    *kubernetes.Clientset
	namespace    string
	podGenerator *podgen.PodGenerator
	logger       util.Logger
	queryPort    int
}

// K8sQueryHandlerConfig contains configuration for the K8sQueryHandler.
type K8sQueryHandlerConfig struct {
	Clientset    *kubernetes.Clientset
	Namespace    string
	PodGenerator *podgen.PodGenerator
	Logger       util.Logger
	// QueryPort is the port where bot query servers listen (default: 9476)
	QueryPort    int
}

// NewK8sQueryHandler creates a new K8sQueryHandler instance.
func NewK8sQueryHandler(config K8sQueryHandlerConfig) *K8sQueryHandler {
	if config.Logger == nil {
		config.Logger = &util.DefaultLogger{}
	}
	if config.Namespace == "" {
		config.Namespace = "the0"
	}
	if config.QueryPort == 0 {
		config.QueryPort = 9476 // Default SDK query server port
	}
	return &K8sQueryHandler{
		clientset:    config.Clientset,
		namespace:    config.Namespace,
		podGenerator: config.PodGenerator,
		logger:       config.Logger,
		queryPort:    config.QueryPort,
	}
}

// ExecuteQuery executes a query against a bot pod.
// For scheduled bots: Creates a Job with QUERY_PATH and QUERY_PARAMS set.
// For realtime bots: Proxies the request to the pod's query HTTP server.
func (h *K8sQueryHandler) ExecuteQuery(ctx context.Context, req QueryRequest, bot model.Bot, podIP string) (*QueryResponse, error) {
	start := time.Now()

	// Set defaults
	if req.TimeoutSec <= 0 {
		req.TimeoutSec = 30
	}

	// Create timeout context
	queryCtx, cancel := context.WithTimeout(ctx, time.Duration(req.TimeoutSec)*time.Second)
	defer cancel()

	// Determine if this is a realtime bot (has a running pod with IP)
	isRealtime := podIP != ""

	if isRealtime {
		return h.executeRealtimeQuery(queryCtx, req, podIP, start)
	}

	// For scheduled bots, create an ephemeral Job
	return h.executeScheduledQuery(queryCtx, req, bot, start)
}

// executeScheduledQuery creates a Kubernetes Job to execute the query.
func (h *K8sQueryHandler) executeScheduledQuery(ctx context.Context, req QueryRequest, bot model.Bot, start time.Time) (*QueryResponse, error) {
	h.logger.Info("Executing scheduled query via K8s Job: bot=%s path=%s", req.BotID, req.QueryPath)

	// Generate pod spec for the query
	pod, err := h.podGenerator.GenerateQueryPod(bot, req.QueryPath, req.Params)
	if err != nil {
		return &QueryResponse{
			Status:    "error",
			Error:     fmt.Sprintf("failed to generate query pod spec: %v", err),
			Duration:  time.Since(start),
			Timestamp: time.Now(),
		}, nil
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
				"the0.dev/query":      "true",
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
		return &QueryResponse{
			Status:    "error",
			Error:     fmt.Sprintf("failed to create query job: %v", err),
			Duration:  time.Since(start),
			Timestamp: time.Now(),
		}, nil
	}

	// Wait for Job completion
	output, err := h.waitForJobCompletion(ctx, createdJob.Name)
	if err != nil {
		return &QueryResponse{
			Status:    "error",
			Error:     fmt.Sprintf("query job failed: %v", err),
			Duration:  time.Since(start),
			Timestamp: time.Now(),
		}, nil
	}

	// Parse the output as JSON
	response := &QueryResponse{
		Duration:  time.Since(start),
		Timestamp: time.Now(),
	}

	var queryOutput struct {
		Status string          `json:"status"`
		Data   json.RawMessage `json:"data"`
		Error  string          `json:"error"`
	}

	if err := json.Unmarshal([]byte(output), &queryOutput); err != nil {
		response.Status = "error"
		response.Error = fmt.Sprintf("failed to parse query response: %v (output: %s)", err, output)
		return response, nil
	}

	response.Status = queryOutput.Status
	response.Data = queryOutput.Data
	response.Error = queryOutput.Error

	return response, nil
}

// waitForJobCompletion waits for a Job to complete and returns its output.
func (h *K8sQueryHandler) waitForJobCompletion(ctx context.Context, jobName string) (string, error) {
	ticker := time.NewTicker(500 * time.Millisecond)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			return "", ctx.Err()
		case <-ticker.C:
			job, err := h.clientset.BatchV1().Jobs(h.namespace).Get(ctx, jobName, metav1.GetOptions{})
			if err != nil {
				return "", fmt.Errorf("failed to get job status: %w", err)
			}

			// Check if job completed
			if job.Status.Succeeded > 0 {
				// Get pod logs
				return h.getJobPodLogs(ctx, jobName)
			}

			// Check if job failed
			if job.Status.Failed > 0 {
				// Get pod logs for error details
				logs, _ := h.getJobPodLogs(ctx, jobName)
				return "", fmt.Errorf("job failed: %s", logs)
			}
		}
	}
}

// getJobPodLogs retrieves logs from the pod created by a Job.
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

// executeRealtimeQuery proxies the query to the bot pod's HTTP server.
func (h *K8sQueryHandler) executeRealtimeQuery(ctx context.Context, req QueryRequest, podIP string, start time.Time) (*QueryResponse, error) {
	h.logger.Info("Executing realtime query via pod IP: bot=%s path=%s ip=%s", req.BotID, req.QueryPath, podIP)

	// Build the request URL
	url := fmt.Sprintf("http://%s:%d%s", podIP, h.queryPort, req.QueryPath)

	// Prepare request body with params
	var reqBody io.Reader
	if len(req.Params) > 0 {
		paramsJSON, err := json.Marshal(req.Params)
		if err != nil {
			return &QueryResponse{
				Status:    "error",
				Error:     fmt.Sprintf("failed to marshal query params: %v", err),
				Duration:  time.Since(start),
				Timestamp: time.Now(),
			}, nil
		}
		reqBody = bytes.NewReader(paramsJSON)
	}

	// Create HTTP request
	httpReq, err := http.NewRequestWithContext(ctx, http.MethodPost, url, reqBody)
	if err != nil {
		return &QueryResponse{
			Status:    "error",
			Error:     fmt.Sprintf("failed to create HTTP request: %v", err),
			Duration:  time.Since(start),
			Timestamp: time.Now(),
		}, nil
	}
	httpReq.Header.Set("Content-Type", "application/json")

	// Execute request
	client := &http.Client{
		Timeout: time.Duration(req.TimeoutSec) * time.Second,
	}
	resp, err := client.Do(httpReq)
	if err != nil {
		return &QueryResponse{
			Status:    "error",
			Error:     fmt.Sprintf("failed to execute query request: %v", err),
			Duration:  time.Since(start),
			Timestamp: time.Now(),
		}, nil
	}
	defer resp.Body.Close()

	// Read response body
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return &QueryResponse{
			Status:    "error",
			Error:     fmt.Sprintf("failed to read query response: %v", err),
			Duration:  time.Since(start),
			Timestamp: time.Now(),
		}, nil
	}

	// Parse response JSON
	var output struct {
		Status string          `json:"status"`
		Data   json.RawMessage `json:"data"`
		Error  string          `json:"error"`
	}

	if err := json.Unmarshal(body, &output); err != nil {
		return &QueryResponse{
			Status:    "error",
			Error:     fmt.Sprintf("failed to parse query response: %v (body: %s)", err, string(body)),
			Duration:  time.Since(start),
			Timestamp: time.Now(),
		}, nil
	}

	return &QueryResponse{
		Status:    output.Status,
		Data:      output.Data,
		Error:     output.Error,
		Duration:  time.Since(start),
		Timestamp: time.Now(),
	}, nil
}
