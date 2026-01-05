// Package controller provides an HTTP server for bot query execution in Kubernetes.
package controller

import (
	"context"
	"fmt"

	corev1 "k8s.io/api/core/v1"
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"
	"k8s.io/client-go/kubernetes"

	"runtime/internal/k8s/podgen"
	"runtime/internal/model"
	"runtime/internal/query"
	"runtime/internal/util"
)

// K8sQueryServer wraps the shared query.Server with K8s-specific bot resolution.
type K8sQueryServer struct {
	server    *query.Server
	handler   *K8sQueryHandler
	botRepo   BotRepository
	clientset *kubernetes.Clientset
	namespace string
	logger    util.Logger
}

// K8sQueryServerConfig contains configuration for the K8sQueryServer.
type K8sQueryServerConfig struct {
	Port      int
	Handler   *K8sQueryHandler
	BotRepo   BotRepository
	Clientset *kubernetes.Clientset
	Namespace string
	Logger    util.Logger
}

// k8sQueryExecutor adapts K8sQueryHandler to the query.Executor interface.
type k8sQueryExecutor struct {
	handler   *K8sQueryHandler
	botRepo   BotRepository
	clientset *kubernetes.Clientset
	namespace string
	logger    util.Logger
}

func (e *k8sQueryExecutor) ExecuteQuery(ctx context.Context, req query.Request, targetIP string) *query.Response {
	// Get bot from repository
	bots, err := e.botRepo.FindAllEnabled(ctx)
	if err != nil {
		return &query.Response{Status: "error", Error: fmt.Sprintf("failed to query bots: %v", err)}
	}

	// Find the specific bot
	var bot *model.Bot
	for i := range bots {
		if bots[i].ID == req.BotID {
			bot = &bots[i]
			break
		}
	}

	if bot == nil {
		return &query.Response{Status: "error", Error: fmt.Sprintf("bot not found: %s", req.BotID)}
	}

	// Get pod IP for realtime bots
	podIP := e.getBotPodIP(ctx, req.BotID)

	resp, _ := e.handler.ExecuteQuery(ctx, req, *bot, podIP)
	return resp
}

func (e *k8sQueryExecutor) getBotPodIP(ctx context.Context, botID string) string {
	pods, err := e.clientset.CoreV1().Pods(e.namespace).List(ctx, metav1.ListOptions{
		LabelSelector: fmt.Sprintf("%s=%s", podgen.LabelBotID, botID),
	})
	if err != nil {
		e.logger.Error("Failed to list pods for bot %s: %v", botID, err)
		return ""
	}

	for _, pod := range pods.Items {
		if pod.Status.Phase == corev1.PodRunning && pod.Status.PodIP != "" {
			return pod.Status.PodIP
		}
	}
	return ""
}

// k8sBotResolver adapts BotRepository to the query.BotResolver interface.
type k8sBotResolver struct {
	botRepo BotRepository
}

func (r *k8sBotResolver) ResolveBot(ctx context.Context, botID string) (string, error) {
	bots, err := r.botRepo.FindAllEnabled(ctx)
	if err != nil {
		return "", fmt.Errorf("failed to query bots: %v", err)
	}

	for _, bot := range bots {
		if bot.ID == botID {
			// Check if bot has query entrypoint
			if bot.CustomBotVersion.Config.Entrypoints == nil {
				return "", fmt.Errorf("bot %s does not have entrypoints configured", botID)
			}
			if _, hasQuery := bot.CustomBotVersion.Config.Entrypoints["query"]; !hasQuery {
				return "", fmt.Errorf("bot %s does not have a query entrypoint", botID)
			}
			// Return empty - executor will handle pod IP lookup
			return "", nil
		}
	}

	return "", fmt.Errorf("bot not found: %s", botID)
}

// NewK8sQueryServer creates a new K8sQueryServer instance.
func NewK8sQueryServer(config K8sQueryServerConfig) *K8sQueryServer {
	if config.Logger == nil {
		config.Logger = &util.DefaultLogger{}
	}
	if config.Namespace == "" {
		config.Namespace = "the0"
	}

	executor := &k8sQueryExecutor{
		handler:   config.Handler,
		botRepo:   config.BotRepo,
		clientset: config.Clientset,
		namespace: config.Namespace,
		logger:    config.Logger,
	}

	resolver := &k8sBotResolver{
		botRepo: config.BotRepo,
	}

	server := query.NewServer(query.ServerConfig{
		Port:     config.Port,
		Resolver: resolver,
		Executor: executor,
		Logger:   config.Logger,
	})

	return &K8sQueryServer{
		server:    server,
		handler:   config.Handler,
		botRepo:   config.BotRepo,
		clientset: config.Clientset,
		namespace: config.Namespace,
		logger:    config.Logger,
	}
}

// Start begins serving HTTP requests.
func (s *K8sQueryServer) Start() error {
	return s.server.Start()
}

// Stop gracefully shuts down the server.
func (s *K8sQueryServer) Stop(ctx context.Context) error {
	return s.server.Stop(ctx)
}
