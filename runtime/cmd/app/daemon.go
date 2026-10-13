package main

import (
	"context"
	"time"

	"github.com/spf13/cobra"

	"runtime/internal/daemon"
)

var daemonCmd = &cobra.Command{
	Use:   "daemon",
	Short: "Bot container daemon commands",
	Long:  `Commands for syncing bot state and logs in containers (Docker and K8s).`,
}

var daemonSyncCmd = &cobra.Command{
	Use:   "sync",
	Short: "Sync bot state and logs to MinIO",
	Long: `Syncs state and logs directories to MinIO.
State is only uploaded when it has actually changed.
Logs are uploaded incrementally (new content only).

Required environment variables:
  MINIO_ENDPOINT, MINIO_ACCESS_KEY, MINIO_SECRET_KEY

Optional environment variables:
  MINIO_STATE_BUCKET (default: bot-state)`,
	RunE: runDaemonSync,
}

var (
	daemonBotID         string
	daemonStatePath     string
	daemonLogsPath      string
	daemonSyncInterval  time.Duration
	daemonReadinessPort int
)

func init() {
	// Sync command flags
	daemonSyncCmd.Flags().StringVar(&daemonBotID, "bot-id", "", "Bot ID (required)")
	daemonSyncCmd.Flags().StringVar(&daemonStatePath, "state-path", "/state", "Path for state directory")
	daemonSyncCmd.Flags().StringVar(&daemonLogsPath, "logs-path", "/var/the0/logs", "Path for logs directory")
	daemonSyncCmd.Flags().DurationVar(&daemonSyncInterval, "interval", 60*time.Second, "Sync interval")
	daemonSyncCmd.Flags().IntVar(&daemonReadinessPort, "readiness-port", 0, "Port for health/readiness HTTP server (K8s sidecar mode)")
	daemonSyncCmd.MarkFlagRequired("bot-id")

	daemonCmd.AddCommand(daemonSyncCmd)
	rootCmd.AddCommand(daemonCmd)
}

func runDaemonSync(cmd *cobra.Command, args []string) error {
	return daemon.Sync(context.Background(), daemon.SyncOptions{
		BotID:         daemonBotID,
		StatePath:     daemonStatePath,
		LogsPath:      daemonLogsPath,
		Interval:      daemonSyncInterval,
		ReadinessPort: daemonReadinessPort,
	})
}
