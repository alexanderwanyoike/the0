// Package docker provides query server for bot containers.
package docker

import (
	"context"
	"fmt"

	"runtime/internal/model"
	"runtime/internal/query"
	"runtime/internal/util"
)

// BotResolver resolves a bot ID to its executable configuration.
type BotResolver interface {
	// GetBot returns the bot configuration for a given bot ID.
	GetBot(ctx context.Context, botID string) (*model.Bot, error)
	// GetContainerID returns the container ID for a running bot.
	GetContainerID(ctx context.Context, botID string) (string, bool)
}

// QueryServer wraps the shared query.Server with Docker-specific bot resolution.
type QueryServer struct {
	server      *query.Server
	handler     *QueryHandler
	botResolver BotResolver
	logger      util.Logger
}

// QueryServerConfig contains configuration for the QueryServer.
type QueryServerConfig struct {
	Port        int
	Handler     *QueryHandler
	BotResolver BotResolver
	Logger      util.Logger
}

// dockerQueryExecutor adapts QueryHandler to the query.Executor interface.
type dockerQueryExecutor struct {
	handler     *QueryHandler
	botResolver BotResolver
}

func (e *dockerQueryExecutor) ExecuteQuery(ctx context.Context, req query.Request, targetIP string) *query.Response {
	// Get bot info to build executable
	bot, err := e.botResolver.GetBot(ctx, req.BotID)
	if err != nil {
		return &query.Response{Status: "error", Error: fmt.Sprintf("bot not found: %s", req.BotID)}
	}

	// Check if bot is running
	containerID, isRunning := e.botResolver.GetContainerID(ctx, req.BotID)

	// Convert bot to executable
	executable := botToExecutable(bot, isRunning, containerID)
	if executable == nil {
		return &query.Response{Status: "error", Error: fmt.Sprintf("bot %s does not have a query entrypoint", req.BotID)}
	}

	resp, _ := e.handler.ExecuteQuery(ctx, req, *executable, containerID)
	return resp
}

// dockerBotResolver adapts BotResolver to the query.BotResolver interface.
type dockerBotResolver struct {
	resolver BotResolver
}

func (r *dockerBotResolver) ResolveBot(ctx context.Context, botID string) (string, error) {
	bot, err := r.resolver.GetBot(ctx, botID)
	if err != nil {
		return "", fmt.Errorf("bot not found: %s", botID)
	}

	// Check if bot has query entrypoint
	if bot.CustomBotVersion.Config.Entrypoints == nil {
		return "", fmt.Errorf("bot %s does not have entrypoints configured", botID)
	}
	if _, hasQuery := bot.CustomBotVersion.Config.Entrypoints["query"]; !hasQuery {
		return "", fmt.Errorf("bot %s does not have a query entrypoint", botID)
	}

	// Return empty string for target IP - executor will handle resolution
	return "", nil
}

// NewQueryServer creates a new QueryServer instance.
func NewQueryServer(config QueryServerConfig) *QueryServer {
	if config.Logger == nil {
		config.Logger = &util.DefaultLogger{}
	}

	executor := &dockerQueryExecutor{
		handler:     config.Handler,
		botResolver: config.BotResolver,
	}

	resolver := &dockerBotResolver{
		resolver: config.BotResolver,
	}

	server := query.NewServer(query.ServerConfig{
		Port:     config.Port,
		Resolver: resolver,
		Executor: executor,
		Logger:   config.Logger,
	})

	return &QueryServer{
		server:      server,
		handler:     config.Handler,
		botResolver: config.BotResolver,
		logger:      config.Logger,
	}
}

// Start begins serving HTTP requests.
func (s *QueryServer) Start() error {
	return s.server.Start()
}

// Stop gracefully shuts down the server.
func (s *QueryServer) Stop(ctx context.Context) error {
	return s.server.Stop(ctx)
}

// botToExecutable converts a Bot model to an Executable for query execution.
// Returns nil if the bot has no query entrypoint defined.
func botToExecutable(bot *model.Bot, isRunning bool, containerID string) *model.Executable {
	if bot.CustomBotVersion.Config.Entrypoints == nil {
		return nil
	}

	// Check if query entrypoint exists
	if _, hasQuery := bot.CustomBotVersion.Config.Entrypoints["query"]; !hasQuery {
		return nil
	}

	entrypointFiles := make(map[string]string)
	for k, v := range bot.CustomBotVersion.Config.Entrypoints {
		entrypointFiles[k] = v
	}

	return &model.Executable{
		ID:              bot.ID,
		Runtime:         bot.CustomBotVersion.Config.Runtime,
		Entrypoint:      "query",
		EntrypointFiles: entrypointFiles,
		Config:          bot.Config,
		FilePath:        bot.CustomBotVersion.FilePath,
		IsLongRunning:   isRunning,
	}
}
