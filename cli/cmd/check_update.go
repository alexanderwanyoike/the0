package cmd

import (
	"context"
	"fmt"
	"os"
	"time"

	"github.com/spf13/cobra"
	"the0/internal"
)

// VERSION will be set by the main package
var VERSION = "1.0.0" // Default fallback

// SetVersion sets the version for the update commands
func SetVersion(version string) {
	VERSION = version
}

func NewCheckUpdateCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "check-update",
		Short: "Check for available updates",
		Long:  "Check if a newer version of the CLI is available without installing it.",
		RunE:  checkUpdateRun,
	}

	cmd.Flags().Duration("timeout", 10*time.Second, "Request timeout")

	return cmd
}

func checkUpdateRun(cmd *cobra.Command, args []string) error {
	// Get flags
	timeout, _ := cmd.Flags().GetDuration("timeout")

	// Get channel from environment variable
	updateChannel := internal.GetUpdateChannel()

	// Create updater
	updater := internal.NewUpdater(internal.UpdateConfig{
		Channel:        updateChannel,
		CurrentVersion: VERSION,
		CheckTimeout:   timeout,
	})

	fmt.Printf("🔍 Checking for updates...\n")

	// Check for updates
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()

	updateAvailable, latestVersion, err := updater.CheckForUpdates(ctx)
	if err != nil {
		if ctx.Err() == context.DeadlineExceeded {
			fmt.Fprintf(os.Stderr, "⏰ Update check timed out after %v\n", timeout)
			fmt.Fprintf(os.Stderr, "   Try increasing the timeout with --timeout flag\n")
			return nil
		}
		return fmt.Errorf("failed to check for updates: %w", err)
	}

	if !updateAvailable {
		fmt.Printf("✅ You're running the latest version: %s\n", internal.NormalizeVersion(VERSION))
		return nil
	}

	// Show update information
	fmt.Printf("⚠️  Update available!\n")
	fmt.Printf("   Current: %s\n", internal.NormalizeVersion(VERSION))
	fmt.Printf("   Latest:  %s\n", internal.NormalizeVersion(latestVersion))
	fmt.Printf("\n💡 Run 'the0 self-update' to update now.\n")

	return nil
}
