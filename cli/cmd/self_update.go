package cmd

import (
	"context"
	"fmt"
	"os"
	"time"

	"github.com/spf13/cobra"
	"the0/internal"
)

// Use the VERSION variable from check_update.go (same package)

func NewSelfUpdateCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "self-update",
		Short: "Update the CLI to the latest version",
		Long:  "Download and install the latest version of the CLI binary.",
		RunE:  selfUpdateRun,
	}

	cmd.Flags().Duration("timeout", 5*time.Minute, "Download timeout")
	cmd.Flags().Bool("check-only", false, "Only check for updates, don't install")
	cmd.Flags().Bool("force", false, "Force update even if already on latest version")
	cmd.Flags().Bool("yes", false, "Skip confirmation prompt")

	return cmd
}

func selfUpdateRun(cmd *cobra.Command, args []string) error {
	// Get flags
	timeout, _ := cmd.Flags().GetDuration("timeout")
	checkOnly, _ := cmd.Flags().GetBool("check-only")
	force, _ := cmd.Flags().GetBool("force")
	yes, _ := cmd.Flags().GetBool("yes")

	// Get channel from environment variable
	updateChannel := internal.GetUpdateChannel()

	// Create updater
	updater := internal.NewUpdater(internal.UpdateConfig{
		Channel:         updateChannel,
		CurrentVersion:  VERSION,
		CheckTimeout:    10 * time.Second,
		DownloadTimeout: timeout,
	})

	fmt.Printf("🔍 Checking for updates...\n")

	// Check for updates
	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()

	updateAvailable, latestVersion, err := updater.CheckForUpdates(ctx)
	if err != nil {
		if ctx.Err() == context.DeadlineExceeded {
			fmt.Fprintf(os.Stderr, "⏰ Update check timed out\n")
			return nil
		}
		return fmt.Errorf("failed to check for updates: %w", err)
	}

	if !updateAvailable && !force {
		fmt.Printf("✅ You're already running the latest version: %s\n", internal.NormalizeVersion(VERSION))
		return nil
	}

	// Show update information
	if updateAvailable {
		fmt.Printf("⚠️  Update available!\n")
		fmt.Printf("   Current: %s\n", internal.NormalizeVersion(VERSION))
		fmt.Printf("   Latest:  %s\n", internal.NormalizeVersion(latestVersion))
	} else if force {
		fmt.Printf("🔄 Forcing update to latest version: %s\n", internal.NormalizeVersion(latestVersion))
	}

	// If check-only, exit here
	if checkOnly {
		if updateAvailable {
			fmt.Printf("\n💡 Run 'the0 self-update' to install the update.\n")
		}
		return nil
	}

	// Confirmation prompt (unless --yes is used)
	if !yes {
		fmt.Printf("\n❓ Do you want to update now? [y/N]: ")
		var response string
		fmt.Scanln(&response)

		if response != "y" && response != "Y" && response != "yes" && response != "Yes" {
			fmt.Println("Update cancelled.")
			return nil
		}
	}

	// Perform the update
	fmt.Printf("\n🚀 Starting update process...\n")

	// Progress callback
	var lastPercent int
	progressCallback := func(written, total int64) {
		if total > 0 {
			percent := int((written * 100) / total)
			if percent != lastPercent && percent%5 == 0 {
				fmt.Printf("📦 Downloading: %d%% (%d/%d bytes)\n", percent, written, total)
				lastPercent = percent
			}
		}
	}

	// Use extended timeout for the update process
	updateCtx, updateCancel := context.WithTimeout(context.Background(), timeout)
	defer updateCancel()

	err = updater.SelfUpdate(updateCtx, progressCallback)
	if err != nil {
		if updateCtx.Err() == context.DeadlineExceeded {
			fmt.Fprintf(os.Stderr, "⏰ Update timed out after %v\n", timeout)
			fmt.Fprintf(os.Stderr, "   Try increasing the timeout with --timeout flag\n")
			return nil
		}
		return fmt.Errorf("update failed: %w", err)
	}

	fmt.Printf("\n🎉 Update completed successfully!\n")
	fmt.Printf("   New version: %s\n", internal.NormalizeVersion(latestVersion))
	fmt.Printf("\n💡 The update will take effect the next time you run the CLI.\n")

	return nil
}
