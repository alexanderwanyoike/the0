package cmd

import (
	"fmt"
	"os"

	"github.com/spf13/cobra"
	"the0/internal"
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
	fmt.Println("Jacking into the0...")

	auth, err := internal.PromptForNewAPIKey()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Connection failed: %v\n", err)
		os.Exit(1)
	}

	fmt.Println("✓ Access granted - welcome to the0")
	fmt.Println("Ready to deploy trading bots")

	// Test the key
	apiClient := internal.NewAPIClient(internal.GetAPIBaseURL())
	if err := apiClient.TestAPIKey(auth); err != nil {
		fmt.Printf("⚠️  Auth test failed: %v\n", err)
		fmt.Println("Check your API key and permissions")
	} else {
		fmt.Println("✓ Connection verified")
	}
}

func authStatus(cmd *cobra.Command, args []string) {
	fmt.Println("Running system diagnostics...")

	auth, err := internal.LoadAuth()
	if err != nil {
		fmt.Println("No access codes detected. Run 'the0 auth login' to jack in")
		os.Exit(1)
	}

	fmt.Printf("Connected: %s\n", auth.CreatedAt.Format("2006-01-02 15:04:05"))

	apiClient := internal.NewAPIClient(internal.GetAPIBaseURL())
	if err := apiClient.TestAPIKey(auth); err != nil {
		fmt.Printf("Connection failed: %v\n", err)
		fmt.Println("Run 'the0 auth login' to reconnect")
		os.Exit(1)
	} else {
		fmt.Println("✓ Connection active")
	}
}

func authLogout(cmd *cobra.Command, args []string) {
	if err := internal.RemoveAuth(); err != nil {
		if os.IsNotExist(err) {
			fmt.Println("No credentials found")
		} else {
			fmt.Fprintf(os.Stderr, "Failed to clear credentials: %v\n", err)
			os.Exit(1)
		}
	} else {
		fmt.Println("✓ Access codes wiped")
		fmt.Println("Disconnected from the0")
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
