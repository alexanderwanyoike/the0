package main

import (
	"context"
	"fmt"
	"os"
	"time"

	"github.com/spf13/cobra"
	"the0/cmd"
	"the0/internal"
)

var (
	VERSION = "0.0.0-dev" // Default for development, overridden by CI via ldflags
)

func main() {
	// Set version for update commands
	cmd.SetVersion(VERSION)

	var rootCmd = &cobra.Command{
		Use:   "the0",
		Short: "the0 CLI - Deploy and manage your trading bots",
		Long: `
 _____ _          ___  
|_   _| |_  ___  / _ \ 
  | | | ' \/ -_)| (_) |
  |_| |_||_\___| \___/ 
                       
the0 CLI - Terminal-based trading bot management`,
		Version: VERSION,
	}

	// Add command groups
	rootCmd.AddCommand(cmd.NewCustomBotCmd())
	rootCmd.AddCommand(cmd.NewBotCmd())
	rootCmd.AddCommand(cmd.NewUserBotCmd())
	rootCmd.AddCommand(cmd.NewAuthCmd())
	rootCmd.AddCommand(cmd.NewCheckUpdateCmd())
	rootCmd.AddCommand(cmd.NewSelfUpdateCmd())

	// Check for updates before executing commands
	// Skip if quiet mode or if we're running a help command
	if os.Getenv("THE0_QUIET") == "" && !(len(os.Args) > 1 && (os.Args[1] == "help" || os.Args[1] == "--help" || os.Args[1] == "-h")) {
		// Skip if we're running update commands to avoid recursion
		if !(len(os.Args) > 1 && (os.Args[1] == "check-update" || os.Args[1] == "self-update")) {
			updater := internal.NewUpdater(internal.UpdateConfig{
				Channel:        internal.GetUpdateChannel(),
				CurrentVersion: VERSION,
				CheckTimeout:   2 * time.Second, // Fast timeout for startup
			})

			ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
			defer cancel()

			updateAvailable, latestVersion, err := updater.CheckForUpdates(ctx)
			if err == nil && updateAvailable {
				fmt.Fprintf(os.Stderr, "⚠️  A new version of the0 CLI is available!\n")
				fmt.Fprintf(os.Stderr, "   Current: %s\n", internal.NormalizeVersion(VERSION))
				fmt.Fprintf(os.Stderr, "   Latest:  %s\n", internal.NormalizeVersion(latestVersion))
				fmt.Fprintf(os.Stderr, "\n   Run 'the0 self-update' to update or 'the0 check-update --help' for options.\n\n")
			}
		}
	}

	if err := rootCmd.Execute(); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}
