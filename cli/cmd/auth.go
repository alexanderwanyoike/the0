package cmd

import (
	"fmt"
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

		fmt.Println("Current API Configuration:")
		fmt.Printf("  API URL: %s\n", currentURL)

		if envURL != "" {
			fmt.Printf("  Source: THE0_API_URL environment variable\n")
		} else {
			fmt.Printf("  Source: Default (http://localhost:3000)\n")
		}

		fmt.Println("\nTo change the API URL:")
		fmt.Println("  the0 auth config <new-url>")
		fmt.Println("  export THE0_API_URL=<new-url>")
		return
	}

	// Set API URL via environment variable hint
	newURL := args[0]
	fmt.Printf("To set API URL to %s, use one of these methods:\n\n", newURL)
	fmt.Printf("1. Environment variable (recommended):\n")
	fmt.Printf("   export THE0_API_URL=%s\n\n", newURL)
	fmt.Printf("2. For this session only:\n")
	fmt.Printf("   THE0_API_URL=%s the0 <command>\n\n", newURL)
	fmt.Printf("3. Add to your shell profile (~/.bashrc, ~/.zshrc, etc.):\n")
	fmt.Printf("   echo 'export THE0_API_URL=%s' >> ~/.bashrc\n", newURL)
}
