package cmd

import (
	"bufio"
	"encoding/json"
	"fmt"
	"os"
	"strings"
	"time"

	"github.com/fatih/color"
	"github.com/olekukonko/tablewriter"
	"github.com/spf13/cobra"
	"the0/internal"
)

// NewBacktestCmd creates the backtest command group
func NewBacktestCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "backtest",
		Short: "Manage backtests",
		Long:  "📊 Deploy, list, and manage trading bot backtests on the0 platform",
	}

	cmd.AddCommand(NewBacktestDeployCmd())
	cmd.AddCommand(NewBacktestListCmd())
	cmd.AddCommand(NewBacktestDeleteCmd())

	return cmd
}

// NewBacktestDeployCmd creates the backtest deploy command
func NewBacktestDeployCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "deploy <config.json>",
		Short: "Deploy a new backtest",
		Long:  "🚀 Deploy a new backtest using a JSON configuration file",
		Args:  cobra.ExactArgs(1),
		Run:   deployBacktest,
	}
}

// NewBacktestListCmd creates the backtest list command
func NewBacktestListCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "list",
		Short: "List all backtests",
		Long:  "📋 List all your backtest instances with their status and details",
		Args:  cobra.NoArgs,
		Run:   listBacktests,
	}
}

// NewBacktestDeleteCmd creates the backtest delete command
func NewBacktestDeleteCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "delete <backtest-id>",
		Short: "Delete a backtest",
		Long:  "🗑️  Delete a backtest instance (requires confirmation)",
		Args:  cobra.ExactArgs(1),
		Run:   deleteBacktest,
	}
}

func deployBacktest(cmd *cobra.Command, args []string) {
	green := color.New(color.FgGreen)
	red := color.New(color.FgRed)
	blue := color.New(color.FgBlue)

	configPath := args[0]

	blue.Println("📊 Analyzing market data with backtest configuration...")
	fmt.Printf("Config: %s\n", configPath)

	// Load and validate configuration
	configData, err := os.ReadFile(configPath)
	if err != nil {
		red.Fprintf(os.Stderr, "❌ Error reading config file: %v\n", err)
		os.Exit(1)
	}

	var config map[string]any
	if err := json.Unmarshal(configData, &config); err != nil {
		red.Fprintf(os.Stderr, "❌ Invalid JSON in config file: %v\n", err)
		os.Exit(1)
	}

	green.Println("✓ Config loaded")

	// Validate required fields
	if config["name"] == nil {
		red.Fprintf(os.Stderr, "❌ Missing required field: name\n")
		os.Exit(1)
	}
	if config["type"] == nil {
		red.Fprintf(os.Stderr, "❌ Missing required field: type\n")
		os.Exit(1)
	}
	if config["version"] == nil {
		red.Fprintf(os.Stderr, "❌ Missing required field: version\n")
		os.Exit(1)
	}

	// Extract backtest name from config
	backtestName := config["name"].(string)
	fmt.Printf("Backtest: %s\n", backtestName)

	// Get authentication
	auth, err := internal.GetAuthTokenWithRetry()
	if err != nil {
		red.Fprintf(os.Stderr, "❌ Authentication failed: %v\n", err)
		os.Exit(1)
	}

	green.Println("✓ Authenticated")

	// Create API client and deploy backtest
	apiClient := internal.NewAPIClient(internal.GetAPIBaseURL())
	request := &internal.BacktestDeployRequest{
		Name:   backtestName,
		Config: config,
	}

	backtest, err := apiClient.CreateBacktest(auth, request)
	if err != nil {
		if internal.IsAuthError(err) {
			blue.Println("🔑 Access denied. Reconnecting to the grid...")
			auth, err = internal.PromptForNewAPIKey()
			if err != nil {
				red.Fprintf(os.Stderr, "❌ Failed to get new API key: %v\n", err)
				os.Exit(1)
			}
			// Retry with new auth
			backtest, err = apiClient.CreateBacktest(auth, request)
			if err != nil {
				red.Fprintf(os.Stderr, "❌ Failed to deploy backtest: %v\n", err)
				os.Exit(1)
			}
		} else {
			red.Fprintf(os.Stderr, "❌ Failed to deploy backtest: %v\n", err)
			os.Exit(1)
		}
	}

	green.Println("✅ Backtest deployed to the0 📊")
	fmt.Printf("ID: %s\n", backtest.ID)
	fmt.Printf("Name: %s\n", backtest.Name)
	fmt.Printf("Status: %s\n", backtest.Status)
	blue.Printf("📝 Use 'the0 backtest list' to monitor progress\n")
}

func listBacktests(cmd *cobra.Command, args []string) {
	green := color.New(color.FgGreen)
	red := color.New(color.FgRed)
	blue := color.New(color.FgBlue)

	blue.Println("📊 Scanning the grid for backtest results...")

	// Get authentication
	auth, err := internal.GetAuthTokenWithRetry()
	if err != nil {
		red.Fprintf(os.Stderr, "❌ Authentication failed: %v\n", err)
		os.Exit(1)
	}

	// Create API client and list backtests
	apiClient := internal.NewAPIClient(internal.GetAPIBaseURL())

	backtests, err := apiClient.ListBacktests(auth)
	if err != nil {
		if internal.IsAuthError(err) {
			blue.Println("🔑 Access denied. Reconnecting to the grid...")
			auth, err = internal.PromptForNewAPIKey()
			if err != nil {
				red.Fprintf(os.Stderr, "❌ Failed to get new API key: %v\n", err)
				os.Exit(1)
			}
			// Retry with new auth
			backtests, err = apiClient.ListBacktests(auth)
			if err != nil {
				red.Fprintf(os.Stderr, "❌ Failed to list backtests: %v\n", err)
				os.Exit(1)
			}
		} else {
			red.Fprintf(os.Stderr, "❌ Failed to list backtests: %v\n", err)
			os.Exit(1)
		}
	}

	if len(backtests) == 0 {
		blue.Println("📊 No backtests found. Deploy your first backtest with:")
		blue.Println("   the0 backtest deploy <config.json>")
		return
	}

	green.Printf("📊 Found %d backtest(s):\n\n", len(backtests))

	// Create table
	table := tablewriter.NewWriter(os.Stdout)
	table.Header("ID", "Name", "Status", "Progress", "Created At", "Updated At")

	for _, backtest := range backtests {
		status := formatBacktestStatus(backtest.Status)
		progress := formatProgress(backtest.Progress)

		// Convert ISO timestamps to readable dates
		createdTime, err := time.Parse(time.RFC3339, backtest.CreatedAt)
		var createdStr string
		if err != nil {
			createdStr = backtest.CreatedAt[:10] // fallback to simple truncation
		} else {
			createdStr = createdTime.Format("2006-01-02 15:04")
		}

		updatedTime, err := time.Parse(time.RFC3339, backtest.UpdatedAt)
		var updatedStr string
		if err != nil {
			updatedStr = backtest.UpdatedAt[:10] // fallback to simple truncation
		} else {
			updatedStr = updatedTime.Format("2006-01-02 15:04")
		}

		table.Append(truncateID(backtest.ID), backtest.Name, status, progress, createdStr, updatedStr)
	}

	if err := table.Render(); err != nil {
		red.Fprintf(os.Stderr, "Failed to render table: %v\n", err)
		os.Exit(1)
	}
	green.Println("\n📈 Backtest analysis complete. Ready to optimize your strategy!")
}

func deleteBacktest(cmd *cobra.Command, args []string) {
	green := color.New(color.FgGreen)
	red := color.New(color.FgRed)
	blue := color.New(color.FgBlue)
	yellow := color.New(color.FgYellow)

	backtestID := args[0]

	yellow.Printf("⚠️  Are you sure you want to delete backtest '%s'?\n", backtestID)
	yellow.Println("This action cannot be undone")
	fmt.Print("Type 'yes' to confirm: ")

	reader := bufio.NewReader(os.Stdin)
	confirmation, err := reader.ReadString('\n')
	if err != nil {
		red.Fprintf(os.Stderr, "Failed to read confirmation: %v\n", err)
		os.Exit(1)
	}

	confirmation = strings.TrimSpace(strings.ToLower(confirmation))
	if confirmation != "yes" {
		blue.Println("📊 Deletion cancelled. Backtest data remains safe")
		return
	}

	blue.Println("🗑️ Starting backtest deletion process...")

	// Get authentication
	auth, err := internal.GetAuthTokenWithRetry()
	if err != nil {
		red.Fprintf(os.Stderr, "❌ Authentication failed: %v\n", err)
		os.Exit(1)
	}

	green.Println("✓ Connected to the0")

	// Delete backtest
	apiClient := internal.NewAPIClient(internal.GetAPIBaseURL())
	err = apiClient.DeleteBacktest(auth, backtestID)
	if err != nil {
		if internal.IsAuthError(err) {
			blue.Println("🔑 Access denied. Reconnecting to the grid...")
			auth, err = internal.PromptForNewAPIKey()
			if err != nil {
				red.Fprintf(os.Stderr, "❌ Failed to get new API key: %v\n", err)
				os.Exit(1)
			}
			// Retry with new auth
			err = apiClient.DeleteBacktest(auth, backtestID)
			if err != nil {
				red.Fprintf(os.Stderr, "❌ Failed to delete backtest: %v\n", err)
				os.Exit(1)
			}
		} else {
			red.Fprintf(os.Stderr, "❌ Failed to delete backtest: %v\n", err)
			os.Exit(1)
		}
	}

	green.Printf("✅ Backtest '%s' deleted successfully 🗑️\n", backtestID)
}

// Helper functions
func formatBacktestStatus(status string) string {
	switch strings.ToLower(status) {
	case "pending":
		return "⏳ Pending"
	case "running":
		return "🔄 Running"
	case "completed":
		return "✅ Completed"
	case "failed":
		return "❌ Failed"
	default:
		return "❓ Unknown"
	}
}

func formatProgress(progress float64) string {
	if progress == 0 {
		return "-"
	}
	return fmt.Sprintf("%.1f%%", progress*100)
}

func truncateID(id string) string {
	if len(id) <= 8 {
		return id
	}
	return id[:8] + "..."
}
