package cmd

import (
	"bufio"
	"encoding/json"
	"fmt"
	"os"
	"os/signal"
	"regexp"
	"strings"
	"syscall"
	"time"

	"github.com/fatih/color"
	"github.com/olekukonko/tablewriter"
	"github.com/spf13/cobra"
	"the0/internal"
	"the0/internal/logger"
)

func NewBotCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "bot",
		Short: "Manage your bot instances",
		Long:  "Deploy, list, update, and delete your trading bots on the0 platform",
	}

	cmd.AddCommand(NewBotDeployCmd())
	cmd.AddCommand(NewBotListCmd())
	cmd.AddCommand(NewBotUpdateCmd())
	cmd.AddCommand(NewBotDeleteCmd())
	cmd.AddCommand(NewBotLogsCmd())
	cmd.AddCommand(NewBotEnableCmd())
	cmd.AddCommand(NewBotDisableCmd())
	cmd.AddCommand(NewBotStateCmd())
	return cmd
}

func NewBotDeployCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "deploy <config.json>",
		Short: "Deploy a new bot instance",
		Long:  "Deploy a new trading bot to the0 platform",
		Args:  cobra.ExactArgs(1),
		Run:   deployBotInstance,
	}
}

func NewBotListCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "list",
		Short: "List all deployed bot instances",
		Long:  "List all your deployed trading bots with their details",
		Run:   listBotInstances,
	}
}

func NewBotUpdateCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "update <bot_id> <config.json>",
		Short: "Update an existing bot instance",
		Long:  "Update the configuration of an existing bot",
		Args:  cobra.ExactArgs(2),
		Run:   updateBotInstance,
	}
}

func NewBotDeleteCmd() *cobra.Command {
	var skipConfirm bool

	cmd := &cobra.Command{
		Use:   "delete <bot_id>",
		Short: "Delete a bot instance",
		Long:  "Delete a deployed bot (requires confirmation unless -y is provided)",
		Args:  cobra.ExactArgs(1),
		Run: func(cmd *cobra.Command, args []string) {
			deleteBotInstance(cmd, args, skipConfirm)
		},
	}

	cmd.Flags().BoolVarP(&skipConfirm, "yes", "y", false, "Skip confirmation prompt")

	return cmd
}

func NewBotLogsCmd() *cobra.Command {
	var watchMode bool
	var limit int

	cmd := &cobra.Command{
		Use:   "logs <bot_id> [date|dateRange]",
		Short: "View bot logs",
		Long:  "View logs for a deployed bot with optional date filtering",
		Args:  cobra.RangeArgs(1, 2),
		Run: func(cmd *cobra.Command, args []string) {
			getBotLogs(cmd, args, watchMode, limit)
		},
	}

	cmd.Flags().BoolVarP(&watchMode, "watch", "w", false, "Watch logs in real-time (polls every 5 seconds)")
	cmd.Flags().IntVar(&limit, "limit", 100, "Maximum number of log entries to return (max 1000)")

	return cmd
}

func NewBotEnableCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "enable <bot_id>",
		Short: "Enable a bot instance",
		Long:  "Enable a stopped bot instance to start running",
		Args:  cobra.ExactArgs(1),
		Run:   enableBotInstance,
	}
}

func NewBotDisableCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "disable <bot_id>",
		Short: "Disable a bot instance",
		Long:  "Disable a running bot instance to stop it",
		Args:  cobra.ExactArgs(1),
		Run:   disableBotInstance,
	}
}

func NewBotStateCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "state",
		Short: "Manage bot persistent state",
		Long:  "List, get, delete, or clear persistent state for a bot",
	}

	cmd.AddCommand(NewBotStateListCmd())
	cmd.AddCommand(NewBotStateGetCmd())
	cmd.AddCommand(NewBotStateDeleteCmd())
	cmd.AddCommand(NewBotStateClearCmd())
	return cmd
}

func NewBotStateListCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "list <bot_id>",
		Short: "List all state keys for a bot",
		Long:  "List all persistent state keys stored for a bot",
		Args:  cobra.ExactArgs(1),
		Run:   listBotState,
	}
}

func NewBotStateGetCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "get <bot_id> <key>",
		Short: "Get a state value",
		Long:  "Get a specific state value by key for a bot",
		Args:  cobra.ExactArgs(2),
		Run:   getBotStateKey,
	}
}

func NewBotStateDeleteCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "delete <bot_id> <key>",
		Short: "Delete a state key",
		Long:  "Delete a specific state key for a bot",
		Args:  cobra.ExactArgs(2),
		Run:   deleteBotStateKey,
	}
}

func NewBotStateClearCmd() *cobra.Command {
	var skipConfirm bool

	cmd := &cobra.Command{
		Use:   "clear <bot_id>",
		Short: "Clear all state for a bot",
		Long:  "Delete all persistent state for a bot (requires confirmation unless -y is provided)",
		Args:  cobra.ExactArgs(1),
		Run: func(cmd *cobra.Command, args []string) {
			clearBotState(cmd, args, skipConfirm)
		},
	}

	cmd.Flags().BoolVarP(&skipConfirm, "yes", "y", false, "Skip confirmation prompt")
	return cmd
}

func deployBotInstance(cmd *cobra.Command, args []string) {
	configPath := args[0]

	logger.StartSpinner("Deploying bot")
	logger.Verbose("Config file: %s", configPath)

	// Load and parse config.json
	configData, err := os.ReadFile(configPath)
	if err != nil {
		logger.StopSpinnerWithError("Failed to read config file")
		logger.Error("%v", err)
		os.Exit(1)
	}

	var config map[string]interface{}
	if err := json.Unmarshal(configData, &config); err != nil {
		logger.StopSpinnerWithError("Invalid JSON in config file")
		logger.Error("%v", err)
		os.Exit(1)
	}

	// Extract bot name from config
	var botName string
	if nameVal, ok := config["name"].(string); ok {
		botName = nameVal
	} else {
		logger.StopSpinnerWithError("Bot name not found")
		logger.Error("Please add a 'name' field to your config.json")
		os.Exit(1)
	}

	logger.UpdateSpinner("Authenticating")

	// Get auth token
	auth, err := internal.GetAuthTokenWithRetry()
	if err != nil {
		logger.StopSpinnerWithError("Authentication failed")
		logger.Error("%v", err)
		os.Exit(1)
	}

	logger.UpdateSpinner("Creating bot instance")

	// Deploy bot
	apiClient := internal.NewAPIClient(internal.GetAPIBaseURL())
	request := &internal.BotDeployRequest{
		Name:   botName,
		Config: config,
	}

	bot, err := apiClient.DeployBotInstance(auth, request)
	if err != nil {
		// Retry with new auth if needed
		if internal.IsAuthError(err) {
			logger.UpdateSpinner("Session expired. Re-authenticating")
			auth, err = internal.PromptForNewAPIKey()
			if err != nil {
				logger.StopSpinnerWithError("Authentication failed")
				logger.Error("%v", err)
				os.Exit(1)
			}

			// Retry deployment
			bot, err = apiClient.DeployBotInstance(auth, request)
			if err != nil {
				logger.StopSpinnerWithError("Deployment failed")
				logger.Error("%v", err)
				os.Exit(1)
			}
		} else {
			logger.StopSpinnerWithError("Deployment failed")
			logger.Error("%v", err)
			os.Exit(1)
		}
	}

	logger.StopSpinnerWithSuccess("Bot deployed successfully ⚡")
	logger.Print("  ID: %s", bot.ID)
	logger.Print("  Name: %s", bot.Name)
}

func listBotInstances(cmd *cobra.Command, args []string) {
	logger.StartSpinner("Fetching bots")

	// Get auth token
	auth, err := internal.GetAuthTokenWithRetry()
	if err != nil {
		logger.StopSpinnerWithError("Authentication failed")
		logger.Error("%v", err)
		os.Exit(1)
	}

	// List bots
	apiClient := internal.NewAPIClient(internal.GetAPIBaseURL())
	bots, err := apiClient.ListBots(auth)
	if err != nil {
		// Retry with new auth if needed
		if internal.IsAuthError(err) {
			logger.UpdateSpinner("Session expired. Re-authenticating")
			auth, err = internal.PromptForNewAPIKey()
			if err != nil {
				logger.StopSpinnerWithError("Authentication failed")
				logger.Error("%v", err)
				os.Exit(1)
			}

			// Retry listing
			bots, err = apiClient.ListBots(auth)
			if err != nil {
				logger.StopSpinnerWithError("Failed to fetch bots")
				logger.Error("%v", err)
				os.Exit(1)
			}
		} else {
			logger.StopSpinnerWithError("Failed to fetch bots")
			logger.Error("%v", err)
			os.Exit(1)
		}
	}

	logger.StopSpinner()

	if len(bots) == 0 {
		logger.Info("No bots found")
		logger.Print("Deploy your first bot with 'the0 bot deploy <config.json>'")
		return
	}

	// Create table
	table := tablewriter.NewWriter(os.Stdout)
	table.Header("ID", "Name", "Type", "Version", "Schedule", "Created At", "Updated At")

	for _, bot := range bots {
		// Extract type and version from config
		botType := "N/A"
		botVersion := "N/A"
		schedule := "N/A"

		if bot.Config != nil {
			if typeVal, ok := bot.Config["type"].(string); ok {
				botType = typeVal
			}
			if versionVal, ok := bot.Config["version"].(string); ok {
				botVersion = versionVal
			}
			if scheduleVal, ok := bot.Config["schedule"].(string); ok {
				schedule = scheduleVal
			}
		}

		// Convert ISO timestamps to readable dates
		createdTime, err := time.Parse(time.RFC3339, bot.CreatedAt)
		if err != nil {
			createdTime = time.Now() // fallback
		}
		createdStr := createdTime.Format("2006-01-02 15:04")

		updatedTime, err := time.Parse(time.RFC3339, bot.UpdatedAt)
		if err != nil {
			updatedTime = time.Now() // fallback
		}
		updatedStr := updatedTime.Format("2006-01-02 15:04")

		table.Append(bot.ID, bot.Name, botType, botVersion, schedule, createdStr, updatedStr)
	}

	logger.Success("Found %d bot(s)", len(bots))
	logger.Newline()
	if err := table.Render(); err != nil {
		logger.Error("Failed to render table: %v", err)
		os.Exit(1)
	}
}

func updateBotInstance(cmd *cobra.Command, args []string) {
	botID := args[0]
	configPath := args[1]

	logger.StartSpinner("Updating bot")
	logger.Verbose("Bot ID: %s", botID)
	logger.Verbose("Config file: %s", configPath)

	// Load and parse config.json
	configData, err := os.ReadFile(configPath)
	if err != nil {
		logger.StopSpinnerWithError("Failed to read config file")
		logger.Error("%v", err)
		os.Exit(1)
	}

	var configMap map[string]interface{}
	if err := json.Unmarshal(configData, &configMap); err != nil {
		logger.StopSpinnerWithError("Invalid JSON in config file")
		logger.Error("%v", err)
		os.Exit(1)
	}

	logger.UpdateSpinner("Authenticating")

	// Get auth token
	auth, err := internal.GetAuthTokenWithRetry()
	if err != nil {
		logger.StopSpinnerWithError("Authentication failed")
		logger.Error("%v", err)
		os.Exit(1)
	}

	// Extract name from config (optional)
	var botName string
	if nameVal, ok := configMap["name"].(string); ok {
		botName = nameVal
	}

	logger.UpdateSpinner("Applying configuration")

	// Update bot
	apiClient := internal.NewAPIClient(internal.GetAPIBaseURL())
	request := &internal.BotUpdateRequest{
		Name:   botName,
		Config: configMap,
	}

	err = apiClient.UpdateBotInstance(auth, botID, request)
	if err != nil {
		// Retry with new auth if needed
		if internal.IsAuthError(err) {
			logger.UpdateSpinner("Session expired. Re-authenticating")
			auth, err = internal.PromptForNewAPIKey()
			if err != nil {
				logger.StopSpinnerWithError("Authentication failed")
				logger.Error("%v", err)
				os.Exit(1)
			}

			// Retry update
			err = apiClient.UpdateBotInstance(auth, botID, request)
			if err != nil {
				logger.StopSpinnerWithError("Update failed")
				logger.Error("%v", err)
				os.Exit(1)
			}
		} else {
			logger.StopSpinnerWithError("Update failed")
			logger.Error("%v", err)
			os.Exit(1)
		}
	}

	logger.StopSpinnerWithSuccess("Bot updated successfully")
	logger.Print("  ID: %s", botID)
}

func deleteBotInstance(cmd *cobra.Command, args []string, skipConfirm bool) {
	botID := args[0]

	if !skipConfirm {
		logger.Warning("Are you sure you want to delete bot '%s'?", botID)
		logger.Print("This action cannot be undone")
		logger.Printf("Type 'yes' to confirm: ")

		reader := bufio.NewReader(os.Stdin)
		confirmation, err := reader.ReadString('\n')
		if err != nil {
			logger.Error("Failed to read confirmation: %v", err)
			os.Exit(1)
		}

		confirmation = strings.TrimSpace(strings.ToLower(confirmation))
		if confirmation != "yes" {
			logger.Info("Deletion cancelled")
			return
		}
	}

	logger.StartSpinner("Deleting bot")

	// Get auth token
	auth, err := internal.GetAuthTokenWithRetry()
	if err != nil {
		logger.StopSpinnerWithError("Authentication failed")
		logger.Error("%v", err)
		os.Exit(1)
	}

	// Delete bot
	apiClient := internal.NewAPIClient(internal.GetAPIBaseURL())
	err = apiClient.DeleteBotInstance(auth, botID)
	if err != nil {
		// Retry with new auth if needed
		if internal.IsAuthError(err) {
			logger.UpdateSpinner("Session expired. Re-authenticating")
			auth, err = internal.PromptForNewAPIKey()
			if err != nil {
				logger.StopSpinnerWithError("Authentication failed")
				logger.Error("%v", err)
				os.Exit(1)
			}

			// Retry deletion
			err = apiClient.DeleteBotInstance(auth, botID)
			if err != nil {
				logger.StopSpinnerWithError("Deletion failed")
				logger.Error("%v", err)
				os.Exit(1)
			}
		} else {
			logger.StopSpinnerWithError("Deletion failed")
			logger.Error("%v", err)
			os.Exit(1)
		}
	}

	logger.StopSpinnerWithSuccess("Bot deleted successfully")
	logger.Print("  ID: %s", botID)
}

func enableBotInstance(cmd *cobra.Command, args []string) {
	setBotEnabled(args[0], true)
}

func disableBotInstance(cmd *cobra.Command, args []string) {
	setBotEnabled(args[0], false)
}

func setBotEnabled(botID string, enabled bool) {
	action := "Enabling"
	if !enabled {
		action = "Disabling"
	}

	logger.StartSpinner(action + " bot")
	logger.Verbose("Bot ID: %s", botID)

	// Get auth token
	auth, err := internal.GetAuthTokenWithRetry()
	if err != nil {
		logger.StopSpinnerWithError("Authentication failed")
		logger.Error("%v", err)
		os.Exit(1)
	}

	apiClient := internal.NewAPIClient(internal.GetAPIBaseURL())

	// First, fetch the bot to get its current config
	logger.UpdateSpinner("Fetching bot configuration")
	bot, err := apiClient.FindBotByNameOrID(auth, botID)
	if err != nil {
		if internal.IsAuthError(err) {
			logger.UpdateSpinner("Session expired. Re-authenticating")
			auth, err = internal.PromptForNewAPIKey()
			if err != nil {
				logger.StopSpinnerWithError("Authentication failed")
				logger.Error("%v", err)
				os.Exit(1)
			}
			bot, err = apiClient.FindBotByNameOrID(auth, botID)
			if err != nil {
				logger.StopSpinnerWithError("Failed to fetch bot")
				logger.Error("%v", err)
				os.Exit(1)
			}
		} else {
			logger.StopSpinnerWithError("Failed to fetch bot")
			logger.Error("%v", err)
			os.Exit(1)
		}
	}

	// Update config with enabled status
	if bot.Config == nil {
		bot.Config = make(map[string]any)
	}
	bot.Config["enabled"] = enabled

	logger.UpdateSpinner("Updating bot status")

	// Update the bot
	request := &internal.BotUpdateRequest{
		Name:   bot.Name,
		Config: bot.Config,
	}

	err = apiClient.UpdateBotInstance(auth, bot.ID, request)
	if err != nil {
		if internal.IsAuthError(err) {
			logger.UpdateSpinner("Session expired. Re-authenticating")
			auth, err = internal.PromptForNewAPIKey()
			if err != nil {
				logger.StopSpinnerWithError("Authentication failed")
				logger.Error("%v", err)
				os.Exit(1)
			}
			err = apiClient.UpdateBotInstance(auth, bot.ID, request)
			if err != nil {
				logger.StopSpinnerWithError("Update failed")
				logger.Error("%v", err)
				os.Exit(1)
			}
		} else {
			logger.StopSpinnerWithError("Update failed")
			logger.Error("%v", err)
			os.Exit(1)
		}
	}

	status := "enabled"
	if !enabled {
		status = "disabled"
	}
	logger.StopSpinnerWithSuccess("Bot " + status + " successfully")
	logger.Print("  ID: %s", bot.ID)
	logger.Print("  Name: %s", bot.Name)
}

func listBotState(cmd *cobra.Command, args []string) {
	botID := args[0]

	logger.StartSpinner("Fetching state keys")
	logger.Verbose("Bot ID: %s", botID)

	auth, err := internal.GetAuthTokenWithRetry()
	if err != nil {
		logger.StopSpinnerWithError("Authentication failed")
		logger.Error("%v", err)
		os.Exit(1)
	}

	apiClient := internal.NewAPIClient(internal.GetAPIBaseURL())
	keys, err := apiClient.ListBotState(auth, botID)
	if err != nil {
		if internal.IsAuthError(err) {
			logger.UpdateSpinner("Session expired. Re-authenticating")
			auth, err = internal.PromptForNewAPIKey()
			if err != nil {
				logger.StopSpinnerWithError("Authentication failed")
				logger.Error("%v", err)
				os.Exit(1)
			}
			keys, err = apiClient.ListBotState(auth, botID)
			if err != nil {
				logger.StopSpinnerWithError("Failed to list state")
				logger.Error("%v", err)
				os.Exit(1)
			}
		} else {
			logger.StopSpinnerWithError("Failed to list state")
			logger.Error("%v", err)
			os.Exit(1)
		}
	}

	logger.StopSpinner()

	if len(keys) == 0 {
		logger.Info("No state keys found for bot %s", botID)
		return
	}

	// Create table
	table := tablewriter.NewWriter(os.Stdout)
	table.Header("Key", "Size")

	for _, key := range keys {
		sizeStr := formatBytes(key.Size)
		table.Append(key.Key, sizeStr)
	}

	logger.Success("Found %d state key(s)", len(keys))
	logger.Newline()
	if err := table.Render(); err != nil {
		logger.Error("Failed to render table: %v", err)
		os.Exit(1)
	}
}

func formatBytes(bytes int64) string {
	const unit = 1024
	if bytes < unit {
		return fmt.Sprintf("%d B", bytes)
	}
	div, exp := int64(unit), 0
	for n := bytes / unit; n >= unit; n /= unit {
		div *= unit
		exp++
	}
	return fmt.Sprintf("%.1f %cB", float64(bytes)/float64(div), "KMGTPE"[exp])
}

func getBotStateKey(cmd *cobra.Command, args []string) {
	botID := args[0]
	key := args[1]

	logger.StartSpinner("Fetching state value")
	logger.Verbose("Bot ID: %s, Key: %s", botID, key)

	auth, err := internal.GetAuthTokenWithRetry()
	if err != nil {
		logger.StopSpinnerWithError("Authentication failed")
		logger.Error("%v", err)
		os.Exit(1)
	}

	apiClient := internal.NewAPIClient(internal.GetAPIBaseURL())
	value, err := apiClient.GetBotStateKey(auth, botID, key)
	if err != nil {
		if internal.IsAuthError(err) {
			logger.UpdateSpinner("Session expired. Re-authenticating")
			auth, err = internal.PromptForNewAPIKey()
			if err != nil {
				logger.StopSpinnerWithError("Authentication failed")
				logger.Error("%v", err)
				os.Exit(1)
			}
			value, err = apiClient.GetBotStateKey(auth, botID, key)
			if err != nil {
				logger.StopSpinnerWithError("Failed to get state key")
				logger.Error("%v", err)
				os.Exit(1)
			}
		} else {
			logger.StopSpinnerWithError("Failed to get state key")
			logger.Error("%v", err)
			os.Exit(1)
		}
	}

	logger.StopSpinner()

	// Pretty print JSON
	jsonBytes, err := json.MarshalIndent(value, "", "  ")
	if err != nil {
		logger.Error("Failed to format value: %v", err)
		os.Exit(1)
	}

	fmt.Println(string(jsonBytes))
}

func deleteBotStateKey(cmd *cobra.Command, args []string) {
	botID := args[0]
	key := args[1]

	logger.StartSpinner("Deleting state key")
	logger.Verbose("Bot ID: %s, Key: %s", botID, key)

	auth, err := internal.GetAuthTokenWithRetry()
	if err != nil {
		logger.StopSpinnerWithError("Authentication failed")
		logger.Error("%v", err)
		os.Exit(1)
	}

	apiClient := internal.NewAPIClient(internal.GetAPIBaseURL())
	deleted, err := apiClient.DeleteBotStateKey(auth, botID, key)
	if err != nil {
		if internal.IsAuthError(err) {
			logger.UpdateSpinner("Session expired. Re-authenticating")
			auth, err = internal.PromptForNewAPIKey()
			if err != nil {
				logger.StopSpinnerWithError("Authentication failed")
				logger.Error("%v", err)
				os.Exit(1)
			}
			deleted, err = apiClient.DeleteBotStateKey(auth, botID, key)
			if err != nil {
				logger.StopSpinnerWithError("Failed to delete state key")
				logger.Error("%v", err)
				os.Exit(1)
			}
		} else {
			logger.StopSpinnerWithError("Failed to delete state key")
			logger.Error("%v", err)
			os.Exit(1)
		}
	}

	if deleted {
		logger.StopSpinnerWithSuccess("State key deleted successfully")
		logger.Print("  Key: %s", key)
	} else {
		logger.StopSpinner()
		logger.Info("State key '%s' not found", key)
	}
}

func clearBotState(cmd *cobra.Command, args []string, skipConfirm bool) {
	botID := args[0]

	if !skipConfirm {
		logger.Warning("Are you sure you want to clear all state for bot '%s'?", botID)
		logger.Print("This action cannot be undone")
		logger.Printf("Type 'yes' to confirm: ")

		reader := bufio.NewReader(os.Stdin)
		confirmation, err := reader.ReadString('\n')
		if err != nil {
			logger.Error("Failed to read confirmation: %v", err)
			os.Exit(1)
		}

		confirmation = strings.TrimSpace(strings.ToLower(confirmation))
		if confirmation != "yes" {
			logger.Info("Operation cancelled")
			return
		}
	}

	logger.StartSpinner("Clearing bot state")
	logger.Verbose("Bot ID: %s", botID)

	auth, err := internal.GetAuthTokenWithRetry()
	if err != nil {
		logger.StopSpinnerWithError("Authentication failed")
		logger.Error("%v", err)
		os.Exit(1)
	}

	apiClient := internal.NewAPIClient(internal.GetAPIBaseURL())
	cleared, err := apiClient.ClearBotState(auth, botID)
	if err != nil {
		if internal.IsAuthError(err) {
			logger.UpdateSpinner("Session expired. Re-authenticating")
			auth, err = internal.PromptForNewAPIKey()
			if err != nil {
				logger.StopSpinnerWithError("Authentication failed")
				logger.Error("%v", err)
				os.Exit(1)
			}
			cleared, err = apiClient.ClearBotState(auth, botID)
			if err != nil {
				logger.StopSpinnerWithError("Failed to clear state")
				logger.Error("%v", err)
				os.Exit(1)
			}
		} else {
			logger.StopSpinnerWithError("Failed to clear state")
			logger.Error("%v", err)
			os.Exit(1)
		}
	}

	if cleared {
		logger.StopSpinnerWithSuccess("Bot state cleared successfully")
	} else {
		logger.StopSpinner()
		logger.Info("No state to clear for bot %s", botID)
	}
}

func getBotLogs(cmd *cobra.Command, args []string, watchMode bool, limit int) {
	botID := args[0]
	var dateParam string

	// Parse date/dateRange parameter
	if len(args) > 1 {
		dateParam = args[1]
		if err := validateDateParam(dateParam); err != nil {
			logger.Error("Invalid date format: %v", err)
			os.Exit(1)
		}
	} else {
		// Default to current date
		dateParam = time.Now().Format("20060102")
	}

	// Validate limit
	if limit < 1 || limit > 1000 {
		logger.Error("Limit must be between 1 and 1000")
		os.Exit(1)
	}

	logger.StartSpinner("Fetching logs for bot: " + botID)

	// Get auth token
	auth, err := internal.GetAuthTokenWithRetry()
	if err != nil {
		logger.StopSpinnerWithError("Authentication failed")
		logger.Error("%v", err)
		os.Exit(1)
	}

	apiClient := internal.NewAPIClient(internal.GetAPIBaseURL())

	if watchMode {
		logger.StopSpinner()
		logger.Info("Watching logs (Ctrl+C to stop)")
		watchBotLogs(apiClient, auth, botID, dateParam, limit)
	} else {
		displayBotLogs(apiClient, auth, botID, dateParam, limit)
	}
}

func validateDateParam(dateParam string) error {
	// Check for date format (YYYYMMDD)
	dateRegex := regexp.MustCompile(`^\d{8}$`)
	if dateRegex.MatchString(dateParam) {
		// Validate it's a real date
		_, err := time.Parse("20060102", dateParam)
		if err != nil {
			return fmt.Errorf("invalid date: %s (use YYYYMMDD format)", dateParam)
		}
		return nil
	}

	// Check for date range format (YYYYMMDD-YYYYMMDD)
	dateRangeRegex := regexp.MustCompile(`^\d{8}-\d{8}$`)
	if dateRangeRegex.MatchString(dateParam) {
		parts := strings.Split(dateParam, "-")
		startDate, err := time.Parse("20060102", parts[0])
		if err != nil {
			return fmt.Errorf("invalid start date: %s (use YYYYMMDD format)", parts[0])
		}
		endDate, err := time.Parse("20060102", parts[1])
		if err != nil {
			return fmt.Errorf("invalid end date: %s (use YYYYMMDD format)", parts[1])
		}
		if startDate.After(endDate) {
			return fmt.Errorf("start date cannot be after end date")
		}
		return nil
	}

	return fmt.Errorf("invalid format: %s (use YYYYMMDD or YYYYMMDD-YYYYMMDD)", dateParam)
}

func displayBotLogs(apiClient *internal.APIClient, auth *internal.Auth, botID, dateParam string, limit int) {
	// Build parameters
	params := &internal.LogsParams{
		Limit: limit,
	}

	if strings.Contains(dateParam, "-") {
		params.DateRange = dateParam
	} else {
		params.Date = dateParam
	}

	// Fetch logs
	logs, err := apiClient.GetBotLogs(auth, botID, params)
	if err != nil {
		// Retry with new auth if needed
		if internal.IsAuthError(err) {
			logger.UpdateSpinner("Session expired. Re-authenticating")
			auth, err = internal.PromptForNewAPIKey()
			if err != nil {
				logger.StopSpinnerWithError("Authentication failed")
				logger.Error("%v", err)
				os.Exit(1)
			}

			// Retry fetching logs
			logs, err = apiClient.GetBotLogs(auth, botID, params)
			if err != nil {
				logger.StopSpinnerWithError("Failed to fetch logs")
				logger.Error("%v", err)
				os.Exit(1)
			}
		} else {
			logger.StopSpinnerWithError("Failed to fetch logs")
			logger.Error("%v", err)
			os.Exit(1)
		}
	}

	logger.StopSpinner()

	if len(logs) == 0 {
		logger.Info("No logs found for the specified criteria")
		return
	}

	// Display logs
	displayLogEntries(logs)
	logger.Newline()
	logger.Success("Retrieved %d log entries", len(logs))
}

func watchBotLogs(apiClient *internal.APIClient, auth *internal.Auth, botID, dateParam string, limit int) {
	// Set up signal handling for graceful shutdown
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)

	// Keep track of last seen timestamp to avoid duplicates
	var lastTimestamp int64 = 0

	ticker := time.NewTicker(1 * time.Second)
	defer ticker.Stop()

	// Initial fetch
	params := &internal.LogsParams{
		Limit: limit,
	}

	if strings.Contains(dateParam, "-") {
		params.DateRange = dateParam
	} else {
		params.Date = dateParam
	}

	// Fetch initial logs
	logs, err := fetchLogsWithRetry(apiClient, auth, botID, params)
	if err != nil {
		logger.Error("Failed to fetch logs: %v", err)
		os.Exit(1)
	}

	if len(logs) > 0 {
		displayLogEntries(logs)
		// Update last timestamp
		for _, log := range logs {
			if logTime, err := time.Parse(time.RFC3339, log.Date); err == nil {
				if logTime.Unix() > lastTimestamp {
					lastTimestamp = logTime.Unix()
				}
			}
		}
	}

	logger.Newline()

	for {
		select {
		case <-sigChan:
			logger.Newline()
			logger.Info("Stopped watching logs")
			return
		case <-ticker.C:
			// Fetch new logs since last timestamp
			newLogs, err := fetchLogsWithRetry(apiClient, auth, botID, params)
			if err != nil {
				logger.Error("Error fetching logs: %v", err)
				continue
			}

			// Filter out logs we've already seen
			var unseenLogs []internal.LogEntry
			for _, log := range newLogs {
				if logTime, err := time.Parse(time.RFC3339, log.Date); err == nil {
					if logTime.Unix() > lastTimestamp {
						unseenLogs = append(unseenLogs, log)
					}
				}
			}

			if len(unseenLogs) > 0 {
				displayLogEntries(unseenLogs)
				// Update last timestamp
				for _, log := range unseenLogs {
					if logTime, err := time.Parse(time.RFC3339, log.Date); err == nil {
						if logTime.Unix() > lastTimestamp {
							lastTimestamp = logTime.Unix()
						}
					}
				}
			}
		}
	}
}

func fetchLogsWithRetry(apiClient *internal.APIClient, auth *internal.Auth, botID string, params *internal.LogsParams) ([]internal.LogEntry, error) {
	logs, err := apiClient.GetBotLogs(auth, botID, params)
	if err != nil && internal.IsAuthError(err) {
		// Try to get new auth token
		newAuth, authErr := internal.PromptForNewAPIKey()
		if authErr != nil {
			return nil, authErr
		}
		// Retry with new auth
		logs, err = apiClient.GetBotLogs(newAuth, botID, params)
	}
	return logs, err
}

func displayLogEntries(logs []internal.LogEntry) {
	cyan := color.New(color.FgCyan)
	white := color.New(color.FgWhite)

	for _, log := range logs {
		// Parse and format date
		logTime, err := time.Parse(time.RFC3339, log.Date)
		var timeStr string
		if err != nil {
			timeStr = log.Date // Fallback to original date string
		} else {
			timeStr = logTime.Format("2006-01-02 15:04:05")
		}

		// Display log entry
		fmt.Printf("[%s] %s\n", cyan.Sprint(timeStr), white.Sprint(log.Content))
	}
}
