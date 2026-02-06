package main

import (
	"fmt"
	"os"

	"github.com/spf13/cobra"
	"the0/cmd"
	"the0/internal/logger"
)

// Version is the current version of the CLI
const VERSION = "1.3.1"

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
	rootCmd.AddCommand(cmd.NewLocalCmd())

	if err := rootCmd.Execute(); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}
