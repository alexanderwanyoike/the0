package cmd

import (
	"os"

	"github.com/spf13/cobra"
	"the0/internal"
	"the0/internal/logger"
)

func NewAuthCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "auth",
		Short: "Manage API authentication",
		Long:  "Login, logout, and check authentication status",
	}

	cmd.AddCommand(NewLoginCmd())
	cmd.AddCommand(NewStatusCmd())
	cmd.AddCommand(NewLogoutCmd())
	cmd.AddCommand(NewConfigCmd())
	cmd.AddCommand(NewSecretsCmd())
	return cmd
}

func NewLoginCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "login",
		Short: "Set or update your API key",
		Long:  "Connect to the0 platform with your API key",
		Run:   authLogin,
	}
}

func NewStatusCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "status",
		Short: "Check if your API key is valid",
		Long:  "Verify your connection to the0 platform",
		Run:   authStatus,
	}
}

func NewLogoutCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "logout",
		Short: "Remove saved API key",
		Long:  "Clear your saved credentials from this device",
		Run:   authLogout,
	}
}

func NewConfigCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "config [api-url]",
		Short: "Configure API endpoint",
		Long: `Configure the API endpoint for the0 CLI.

If no URL is provided, shows the current API endpoint.
The API URL can also be set via THE0_API_URL environment variable.

Examples:
  the0 auth config                           # Show current API URL
  the0 auth config http://localhost:3001     # Set API URL to localhost
  the0 auth config http://the0-api:3001      # Set API URL to Docker service`,
		Run: authConfig,
	}
	return cmd
}

func authLogin(cmd *cobra.Command, args []string) {
	logger.StartSpinner("Logging in")

	auth, err := internal.PromptForNewAPIKey()
	if err != nil {
		logger.StopSpinnerWithError("Login failed")
		logger.Error("%v", err)
		os.Exit(1)
	}

	logger.UpdateSpinner("Verifying API key")

	// Test the key
	apiClient := internal.NewAPIClient(internal.GetAPIBaseURL())
	if err := apiClient.TestAPIKey(auth); err != nil {
		logger.StopSpinnerWithError("Authentication failed")
		logger.Error("%v", err)
		logger.Print("Check your API key and permissions")
		os.Exit(1)
	}

	logger.StopSpinnerWithSuccess("Logged in successfully")
	logger.Verbose("API key saved to ~/.the0/auth.json")
}

func authStatus(cmd *cobra.Command, args []string) {
	logger.StartSpinner("Checking authentication status")

	auth, err := internal.LoadAuth()
	if err != nil {
		logger.StopSpinnerWithError("Not logged in")
		logger.Print("Run 'the0 auth login' to authenticate")
		os.Exit(1)
	}

	apiClient := internal.NewAPIClient(internal.GetAPIBaseURL())
	if err := apiClient.TestAPIKey(auth); err != nil {
		logger.StopSpinnerWithError("Connection failed")
		logger.Error("%v", err)
		logger.Print("Run 'the0 auth login' to reconnect")
		os.Exit(1)
	}

	logger.StopSpinnerWithSuccess("Authenticated")
	logger.Print("  Connected since: %s", auth.CreatedAt.Format("2006-01-02 15:04:05"))
}

func authLogout(cmd *cobra.Command, args []string) {
	if err := internal.RemoveAuth(); err != nil {
		if os.IsNotExist(err) {
			logger.Info("No credentials found")
		} else {
			logger.Error("Failed to clear credentials: %v", err)
			os.Exit(1)
		}
	} else {
		logger.Success("Logged out successfully")
		logger.Verbose("Credentials removed from ~/.the0/auth.json")
	}
}

func authConfig(cmd *cobra.Command, args []string) {
	if len(args) == 0 {
		// Show current API URL
		currentURL := internal.GetAPIBaseURL()
		envURL := os.Getenv("THE0_API_URL")

		logger.Print("Current API Configuration:")
		logger.Print("  API URL: %s", currentURL)

		if envURL != "" {
			logger.Print("  Source: THE0_API_URL environment variable")
		} else {
			logger.Print("  Source: Default (http://localhost:3000)")
		}

		logger.Newline()
		logger.Print("To change the API URL:")
		logger.Print("  the0 auth config <new-url>")
		logger.Print("  export THE0_API_URL=<new-url>")
		return
	}

	// Set API URL via environment variable hint
	newURL := args[0]
	logger.Print("To set API URL to %s, use one of these methods:", newURL)
	logger.Newline()
	logger.Print("1. Environment variable (recommended):")
	logger.Print("   export THE0_API_URL=%s", newURL)
	logger.Newline()
	logger.Print("2. For this session only:")
	logger.Print("   THE0_API_URL=%s the0 <command>", newURL)
	logger.Newline()
	logger.Print("3. Add to your shell profile (~/.bashrc, ~/.zshrc, etc.):")
	logger.Print("   echo 'export THE0_API_URL=%s' >> ~/.bashrc", newURL)
}

// NewSecretsCmd creates the secrets subcommand for managing build secrets
func NewSecretsCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "secrets",
		Short: "Manage build secrets for private dependencies",
		Long: `Manage build-time secrets used during dependency vendoring.

These secrets are used to authenticate with private package repositories
(like private GitHub repos) when installing dependencies.

Examples:
  the0 auth secrets show                      # Show configured secrets
  the0 auth secrets set github-token <token>  # Set GitHub PAT
  the0 auth secrets clear                     # Remove all secrets`,
	}

	cmd.AddCommand(NewSecretsShowCmd())
	cmd.AddCommand(NewSecretsSetCmd())
	cmd.AddCommand(NewSecretsClearCmd())
	return cmd
}

func NewSecretsShowCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "show",
		Short: "Show configured build secrets",
		Long:  "Display currently configured build secrets (tokens are masked)",
		Run:   secretsShow,
	}
}

func NewSecretsSetCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "set <secret-name> <value>",
		Short: "Set a build secret",
		Long: `Set a build secret for dependency vendoring.

Available secrets:
  github-token    GitHub Personal Access Token for private git repos
  pip-index-url   Private PyPI index URL (include credentials in URL)

Examples:
  the0 auth secrets set github-token ghp_xxxxxxxxxxxx
  the0 auth secrets set pip-index-url https://user:pass@pypi.example.com/simple/`,
		Args: cobra.ExactArgs(2),
		Run:  secretsSet,
	}
}

func NewSecretsClearCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "clear",
		Short: "Clear all build secrets",
		Long:  "Remove all saved build secrets from this device",
		Run:   secretsClear,
	}
}

func secretsShow(cmd *cobra.Command, args []string) {
	secrets, err := internal.LoadBuildSecrets()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to load secrets: %v\n", err)
		os.Exit(1)
	}

	fmt.Println("Build Secrets:")
	fmt.Println("--------------")

	if secrets.GitHubToken != "" {
		fmt.Printf("  github-token:  %s\n", internal.MaskToken(secrets.GitHubToken))
	} else {
		fmt.Println("  github-token:  (not set)")
	}

	if secrets.PipIndexURL != "" {
		fmt.Printf("  pip-index-url: %s\n", internal.MaskToken(secrets.PipIndexURL))
	} else {
		fmt.Println("  pip-index-url: (not set)")
	}

	fmt.Println("\nThese secrets are used during 'the0 custom-bot deploy' to")
	fmt.Println("authenticate with private package repositories.")
}

func secretsSet(cmd *cobra.Command, args []string) {
	secretName := args[0]
	secretValue := args[1]

	secrets, err := internal.LoadBuildSecrets()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to load existing secrets: %v\n", err)
		os.Exit(1)
	}

	switch secretName {
	case "github-token":
		secrets.GitHubToken = secretValue
	case "pip-index-url":
		secrets.PipIndexURL = secretValue
	default:
		fmt.Fprintf(os.Stderr, "Unknown secret: %s\n", secretName)
		fmt.Println("Available secrets: github-token, pip-index-url")
		os.Exit(1)
	}

	if err := internal.SaveBuildSecrets(secrets); err != nil {
		fmt.Fprintf(os.Stderr, "Failed to save secret: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("✓ Secret '%s' saved\n", secretName)
	fmt.Printf("  Value: %s\n", internal.MaskToken(secretValue))
}

func secretsClear(cmd *cobra.Command, args []string) {
	if err := internal.ClearBuildSecrets(); err != nil {
		fmt.Fprintf(os.Stderr, "Failed to clear secrets: %v\n", err)
		os.Exit(1)
	}

	fmt.Println("✓ All build secrets cleared")
}
