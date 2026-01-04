// Package daemon provides bot container lifecycle management for both Docker and K8s.
// The daemon runs as init container/entrypoint prefix (for setup) and sidecar/background
// process (for periodic state and log synchronization).
package daemon

import (
	"context"
	"fmt"
	"os"
	"path/filepath"

	"runtime/internal/entrypoints"
	"runtime/internal/runtime/storage"
	"runtime/internal/util"
)

// InitOptions configures the init command.
type InitOptions struct {
	BotID      string
	CodePath   string // Where to extract code (default: /bot)
	StatePath  string // Where to extract state (default: /state)
	CodeFile   string // MinIO object path for code (e.g., "my-bot/v1.0.0/code.zip")
	Runtime    string // Runtime for entrypoint generation (e.g., "python3.11")
	Entrypoint string // Entrypoint file (e.g., "main.py")
}

// Init downloads bot code and state, preparing the container for execution.
// Runs as init container (K8s) or entrypoint prefix (Docker) before the bot starts.
func Init(ctx context.Context, opts InitOptions) error {
	logger := &util.DefaultLogger{}
	logger.Info("Daemon init starting", "bot_id", opts.BotID)

	// Load config
	cfg, err := storage.LoadConfigFromEnv()
	if err != nil {
		return fmt.Errorf("failed to load config: %w", err)
	}

	// Create MinIO client
	minioClient, err := storage.NewMinioClient(cfg)
	if err != nil {
		return fmt.Errorf("failed to create MinIO client: %w", err)
	}

	// Set defaults
	if opts.CodePath == "" {
		opts.CodePath = "/bot"
	}
	if opts.StatePath == "" {
		opts.StatePath = "/state"
	}

	// Download and extract code
	if opts.CodeFile != "" {
		logger.Info("Downloading bot code", "bot_id", opts.BotID, "file", opts.CodeFile)
		codeManager := storage.NewCodeManager(minioClient, cfg, logger)
		if err := codeManager.DownloadAndExtract(ctx, opts.CodeFile, opts.CodePath); err != nil {
			return fmt.Errorf("failed to download code: %w", err)
		}
	}

	// Generate entrypoint script if runtime and entrypoint are specified
	if opts.Runtime != "" && opts.Entrypoint != "" {
		logger.Info("Generating entrypoint script", "runtime", opts.Runtime, "entrypoint", opts.Entrypoint)
		script, err := entrypoints.GenerateEntrypoint(opts.Runtime, entrypoints.GeneratorOptions{
			EntryPointType: "bot",
			Entrypoint:     opts.Entrypoint,
			WorkDir:        opts.CodePath,
		})
		if err != nil {
			return fmt.Errorf("failed to generate entrypoint: %w", err)
		}

		scriptPath := filepath.Join(opts.CodePath, "entrypoint.sh")
		if err := os.WriteFile(scriptPath, []byte(script), 0755); err != nil {
			return fmt.Errorf("failed to write entrypoint script: %w", err)
		}
		logger.Info("Created entrypoint script", "path", scriptPath)
	}

	// Download state
	logger.Info("Downloading state", "bot_id", opts.BotID)
	if err := os.MkdirAll(opts.StatePath, 0755); err != nil {
		return fmt.Errorf("failed to create state directory: %w", err)
	}

	stateManager := storage.NewStateManager(minioClient, cfg, logger)
	if err := stateManager.DownloadState(ctx, opts.BotID, opts.StatePath); err != nil {
		// State may not exist for first run - log but don't fail
		logger.Info("No existing state (first run)", "bot_id", opts.BotID)
	}

	logger.Info("Daemon init complete", "bot_id", opts.BotID)
	return nil
}
