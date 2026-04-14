package cmd

import (
	"bufio"
	"fmt"
	"os"
	"strings"
	"text/tabwriter"
	"time"

	"github.com/spf13/cobra"
	"the0/internal"
	"the0/internal/logger"
)

func NewEnvCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "env",
		Short: "Manage named API environments (local, prod, ...)",
		Long: `Manage named environments for the the0 CLI.

Each environment stores its own API URL and API key. Switch between them
instantly with 'the0 env use <name>' or override per-command with --env.

Examples:
  the0 env add local --url http://localhost:3000
  the0 env add prod  --url https://api.the0.app --api-key the0_xxx
  the0 env use prod
  the0 env list
  the0 bot list --env prod`,
	}

	cmd.AddCommand(newEnvAddCmd())
	cmd.AddCommand(newEnvUseCmd())
	cmd.AddCommand(newEnvListCmd())
	cmd.AddCommand(newEnvRemoveCmd())
	cmd.AddCommand(newEnvCurrentCmd())
	return cmd
}

func newEnvAddCmd() *cobra.Command {
	var url, apiKey string
	cmd := &cobra.Command{
		Use:   "add <name>",
		Short: "Add a new environment",
		Args:  cobra.ExactArgs(1),
		Run: func(cmd *cobra.Command, args []string) {
			name := args[0]
			if url == "" {
				logger.Error("--url is required")
				os.Exit(1)
			}
			if apiKey == "" {
				key, err := promptForAPIKey()
				if err != nil {
					logger.Error("%v", err)
					os.Exit(1)
				}
				apiKey = key
			}

			if err := addEnvironment(name, url, apiKey); err != nil {
				logger.Error("%v", err)
				os.Exit(1)
			}
			logger.Success("Environment %q added", name)
		},
	}
	cmd.Flags().StringVar(&url, "url", "", "API base URL (e.g. http://localhost:3000)")
	cmd.Flags().StringVar(&apiKey, "api-key", "", "API key (prompted if omitted)")
	return cmd
}

func newEnvUseCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "use <name>",
		Short: "Switch the active environment",
		Args:  cobra.ExactArgs(1),
		Run: func(cmd *cobra.Command, args []string) {
			envs, err := internal.LoadEnvironments()
			if err != nil {
				logger.Error("%v", err)
				os.Exit(1)
			}
			if err := envs.Use(args[0]); err != nil {
				logger.Error("%v", err)
				os.Exit(1)
			}
			if err := internal.SaveEnvironments(envs); err != nil {
				logger.Error("%v", err)
				os.Exit(1)
			}
			logger.Success("Active environment: %s", args[0])
		},
	}
}

func newEnvListCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "list",
		Short: "List all environments",
		Run: func(cmd *cobra.Command, args []string) {
			envs, err := internal.LoadEnvironments()
			if err != nil {
				logger.Error("%v", err)
				os.Exit(1)
			}
			if len(envs.Environments) == 0 {
				logger.Print("No environments defined. Run `the0 env add <name> --url <url>` to create one.")
				return
			}
			w := tabwriter.NewWriter(os.Stdout, 0, 0, 2, ' ', 0)
			fmt.Fprintln(w, "ACTIVE\tNAME\tURL")
			for name, env := range envs.Environments {
				marker := " "
				if name == envs.Active {
					marker = "*"
				}
				fmt.Fprintf(w, "%s\t%s\t%s\n", marker, name, env.URL)
			}
			_ = w.Flush()
		},
	}
}

func newEnvRemoveCmd() *cobra.Command {
	return &cobra.Command{
		Use:     "remove <name>",
		Aliases: []string{"rm"},
		Short:   "Remove an environment",
		Args:    cobra.ExactArgs(1),
		Run: func(cmd *cobra.Command, args []string) {
			envs, err := internal.LoadEnvironments()
			if err != nil {
				logger.Error("%v", err)
				os.Exit(1)
			}
			wasActive := envs.Active == args[0]
			if err := envs.Remove(args[0]); err != nil {
				logger.Error("%v", err)
				os.Exit(1)
			}
			if err := internal.SaveEnvironments(envs); err != nil {
				logger.Error("%v", err)
				os.Exit(1)
			}
			logger.Success("Environment %q removed", args[0])
			if wasActive {
				logger.Info("No active environment; run `the0 env use <name>` to pick one.")
			}
		},
	}
}

func newEnvCurrentCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "current",
		Short: "Show the active environment",
		Run: func(cmd *cobra.Command, args []string) {
			envs, err := internal.LoadEnvironments()
			if err != nil {
				logger.Error("%v", err)
				os.Exit(1)
			}
			if envs.Active == "" {
				logger.Print("No active environment. Legacy fallback in use (URL: %s).", internal.GetAPIBaseURL())
				return
			}
			env, ok := envs.Environments[envs.Active]
			if !ok {
				logger.Error("active environment %q is not defined", envs.Active)
				os.Exit(1)
			}
			logger.Print("Active: %s", envs.Active)
			logger.Print("URL:    %s", env.URL)
		},
	}
}

// addEnvironment validates the API key against the URL and, on success,
// persists the environment. If no environments are defined yet, it first
// migrates any legacy auth.json so the existing login is preserved.
func addEnvironment(name, url, apiKey string) error {
	url = strings.TrimRight(url, "/")
	apiClient := internal.NewAPIClient(url)
	if err := apiClient.TestAPIKey(&internal.Auth{APIKey: apiKey}); err != nil {
		return fmt.Errorf("API key validation failed: %w", err)
	}

	envs, err := internal.LoadEnvironments()
	if err != nil {
		return err
	}
	// First env ever: migrate legacy auth so users aren't logged out.
	if len(envs.Environments) == 0 {
		migrated, err := internal.MigrateLegacyAuth()
		if err != nil {
			return err
		}
		envs = migrated
	}

	if err := envs.Add(name, internal.Environment{
		URL:       url,
		APIKey:    apiKey,
		CreatedAt: time.Now().UTC(),
	}); err != nil {
		return err
	}
	if envs.Active == "" {
		envs.Active = name
	}
	return internal.SaveEnvironments(envs)
}

func promptForAPIKey() (string, error) {
	reader := bufio.NewReader(os.Stdin)
	logger.Printf("Enter API key: ")
	line, err := reader.ReadString('\n')
	if err != nil {
		return "", err
	}
	key := strings.TrimSpace(line)
	if key == "" {
		return "", fmt.Errorf("API key cannot be empty")
	}
	return key, nil
}
