package main

import (
	"fmt"
	"os"
	"time"

	"github.com/spf13/cobra"
	"the0/cmd"
	"the0/internal"
	"the0/internal/logger"
)

// VERSION is overridden by -ldflags at build time
var VERSION = "1.3.1"

var (
	verbose bool
)

func main() {
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
		PersistentPreRun: func(cmd *cobra.Command, args []string) {
			// Initialize global logger with verbose setting
			logger.SetGlobal(logger.New(logger.Config{
				Verbose:   verbose,
				NoSpinner: os.Getenv("THE0_NO_SPINNER") != "",
			}))
		},
	}

	// Add global verbose flag
	rootCmd.PersistentFlags().BoolVarP(&verbose, "verbose", "v", false, "Enable verbose output")

	// Add command groups
	rootCmd.AddCommand(cmd.NewCustomBotCmd())
	rootCmd.AddCommand(cmd.NewBotCmd())
	rootCmd.AddCommand(cmd.NewAuthCmd())
	rootCmd.AddCommand(cmd.NewUpdateCmd(VERSION))

	// Startup version check (non-blocking, cache-based)
	startupVersionCheck()

	if err := rootCmd.Execute(); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}

// startupVersionCheck prints a notice if a newer version is cached, and
// refreshes the cache in the background if stale.
func startupVersionCheck() {
	// Skip check for update and help commands
	if len(os.Args) > 1 {
		sub := os.Args[1]
		if sub == "update" || sub == "help" || sub == "--help" || sub == "-h" || sub == "--version" || sub == "-v" {
			return
		}
	}

	cache, err := internal.ReadVersionCache()
	if err != nil {
		// No cache — fire-and-forget background refresh so the next run has a
		// cached result. The goroutine is intentionally not joined; if the
		// process exits before the request completes, the check is simply
		// skipped and retried on the next invocation.
		go backgroundVersionCheck()
		return
	}

	if internal.IsCacheStale(cache) {
		// Stale cache — fire-and-forget background refresh (same rationale
		// as above: best-effort, no need to block the CLI on a network call).
		go backgroundVersionCheck()
		return
	}

	// Cache is fresh — check if update is available
	isNewer, err := internal.CompareVersions(VERSION, cache.LatestVersion)
	if err != nil {
		return
	}

	if isNewer {
		fmt.Fprintf(os.Stderr,
			"A new version of the0 CLI is available: %s (current: %s). Run 'the0 update' to upgrade.\n",
			cache.LatestVersion, internal.NormalizeVersion(VERSION))
	}
}

func backgroundVersionCheck() {
	updater := internal.NewUpdater(VERSION)
	release, err := updater.CheckLatestRelease()
	if err != nil {
		return
	}

	latestVersion := internal.ExtractVersionFromTag(release.TagName)
	_ = internal.WriteVersionCache(&internal.VersionCache{
		LastCheck:     time.Now(),
		LatestVersion: latestVersion,
	})
}
