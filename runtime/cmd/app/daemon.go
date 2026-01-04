package main

import (
	"context"
	"time"

	"github.com/spf13/cobra"

	"runtime/internal/k8s/daemon"
)

var daemonCmd = &cobra.Command{
	Use:   "daemon",
	Short: "K8s pod daemon commands",
	Long:  `Commands for initializing and syncing bot state in Kubernetes pods.`,
}

var daemonInitCmd = &cobra.Command{
	Use:   "init",
	Short: "Initialize bot environment (download code and state)",
	Long: `Downloads bot code and state from MinIO to prepare the pod environment.
Used as an init container in Kubernetes.

Required environment variables:
  MINIO_ENDPOINT, MINIO_ACCESS_KEY, MINIO_SECRET_KEY

Optional environment variables:
  MINIO_CODE_BUCKET (default: custom-bots)
  MINIO_STATE_BUCKET (default: bot-state)`,
	RunE: runDaemonInit,
}

var daemonSyncCmd = &cobra.Command{
	Use:   "sync",
	Short: "Sync bot state and logs to MinIO",
	Long: `Syncs state and logs directories to MinIO.
State is only uploaded when it has actually changed.
Logs are uploaded incrementally (new content only).

Modes:
  - Continuous (default): Runs as a sidecar, syncing periodically
  - Once (--once): Syncs once and exits, for scheduled bots

Required environment variables:
  MINIO_ENDPOINT, MINIO_ACCESS_KEY, MINIO_SECRET_KEY

Optional environment variables:
  MINIO_STATE_BUCKET (default: bot-state)`,
	RunE: runDaemonSync,
}

var (
	daemonBotID        string
	daemonCodePath     string
	daemonStatePath    string
	daemonLogsPath     string
	daemonCodeFile     string
	daemonRuntime      string
	daemonEntrypoint   string
	daemonSyncInterval time.Duration
	daemonSyncOnce     bool
)

func init() {
	// Init command flags
	daemonInitCmd.Flags().StringVar(&daemonBotID, "bot-id", "", "Bot ID (required)")
	daemonInitCmd.Flags().StringVar(&daemonCodePath, "code-path", "/bot", "Path to extract code")
	daemonInitCmd.Flags().StringVar(&daemonStatePath, "state-path", "/state", "Path for state directory")
	daemonInitCmd.Flags().StringVar(&daemonCodeFile, "code-file", "", "MinIO object path for code (e.g., my-bot/v1.0.0/code.zip)")
	daemonInitCmd.Flags().StringVar(&daemonRuntime, "runtime", "", "Runtime for entrypoint (e.g., python3.11)")
	daemonInitCmd.Flags().StringVar(&daemonEntrypoint, "entrypoint", "", "Entrypoint file (e.g., main.py)")
	daemonInitCmd.MarkFlagRequired("bot-id")

	// Sync command flags
	daemonSyncCmd.Flags().StringVar(&daemonBotID, "bot-id", "", "Bot ID (required)")
	daemonSyncCmd.Flags().StringVar(&daemonStatePath, "state-path", "/state", "Path for state directory")
	daemonSyncCmd.Flags().StringVar(&daemonLogsPath, "logs-path", "/var/the0/logs", "Path for logs directory")
	daemonSyncCmd.Flags().DurationVar(&daemonSyncInterval, "interval", 60*time.Second, "Sync interval")
	daemonSyncCmd.Flags().BoolVar(&daemonSyncOnce, "once", false, "Sync once and exit (for scheduled bots)")
	daemonSyncCmd.MarkFlagRequired("bot-id")

	daemonCmd.AddCommand(daemonInitCmd)
	daemonCmd.AddCommand(daemonSyncCmd)
	rootCmd.AddCommand(daemonCmd)
}

func runDaemonInit(cmd *cobra.Command, args []string) error {
	return daemon.Init(context.Background(), daemon.InitOptions{
		BotID:      daemonBotID,
		CodePath:   daemonCodePath,
		StatePath:  daemonStatePath,
		CodeFile:   daemonCodeFile,
		Runtime:    daemonRuntime,
		Entrypoint: daemonEntrypoint,
	})
}

func runDaemonSync(cmd *cobra.Command, args []string) error {
	return daemon.Sync(context.Background(), daemon.SyncOptions{
		BotID:     daemonBotID,
		StatePath: daemonStatePath,
		LogsPath:  daemonLogsPath,
		Interval:  daemonSyncInterval,
		Once:      daemonSyncOnce,
	})
}
