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
	return &cobra.Command{
		Use:   "delete <bot_id>",
		Short: "Delete a bot instance",
		Long:  "Delete a deployed bot (requires confirmation)",
		Args:  cobra.ExactArgs(1),
		Run:   deleteBotInstance,
	}
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

func deployBotInstance(cmd *cobra.Command, args []string) {
	green := color.New(color.FgGreen)
	red := color.New(color.FgRed)
	blue := color.New(color.FgBlue)

	configPath := args[0]

	blue.Println("Injecting bot into the trading grid...")
	fmt.Printf("Config: %s\n", configPath)

	// Load and parse config.json
	configData, err := os.ReadFile(configPath)
	if err != nil {
		red.Fprintf(os.Stderr, "Failed to read config file: %v\n", err)
		os.Exit(1)
	}

	var config map[string]interface{}
	if err := json.Unmarshal(configData, &config); err != nil {
		red.Fprintf(os.Stderr, "Invalid JSON in config file: %v\n", err)
		os.Exit(1)
	}

	green.Println("✓ Config loaded")

	// Extract bot name from config
	var botName string
	if nameVal, ok := config["name"].(string); ok {
		botName = nameVal
	} else {
		red.Fprintf(os.Stderr, "Bot name not found in config file. Please add a 'name' field to your config.json\n")
		os.Exit(1)
	}

	fmt.Printf("Bot: %s\n", botName)

	// Get auth token
	auth, err := internal.GetAuthTokenWithRetry()
	if err != nil {
		red.Fprintf(os.Stderr, "Authentication failed: %v\n", err)
		os.Exit(1)
	}

	green.Println("✓ Authenticated")

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
			blue.Println("Access denied. Reconnecting to the grid...")
			auth, err = internal.PromptForNewAPIKey()
			if err != nil {
				red.Fprintf(os.Stderr, "Authentication failed: %v\n", err)
				os.Exit(1)
			}

			// Retry deployment
			bot, err = apiClient.DeployBotInstance(auth, request)
			if err != nil {
				red.Fprintf(os.Stderr, "Deployment failed: %v\n", err)
				os.Exit(1)
			}
		} else {
			red.Fprintf(os.Stderr, "Deployment failed: %v\n", err)
			os.Exit(1)
		}
	}

	green.Println("Bot deployed to the0 ⚡")
	fmt.Printf("ID: %s\n", bot.ID)
	fmt.Printf("Name: %s\n", bot.Name)
	green.Println("Ready to trade 📈")
}

func listBotInstances(cmd *cobra.Command, args []string) {
	green := color.New(color.FgGreen)
	red := color.New(color.FgRed)
	blue := color.New(color.FgBlue)

	blue.Println("Scanning the grid for active bots...")

	// Get auth token
	auth, err := internal.GetAuthTokenWithRetry()
	if err != nil {
		red.Fprintf(os.Stderr, "Authentication failed: %v\n", err)
		os.Exit(1)
	}

	// List bots
	apiClient := internal.NewAPIClient(internal.GetAPIBaseURL())
	bots, err := apiClient.ListBots(auth)
	if err != nil {
		// Retry with new auth if needed
		if internal.IsAuthError(err) {
			blue.Println("Access denied. Reconnecting to the grid...")
			auth, err = internal.PromptForNewAPIKey()
			if err != nil {
				red.Fprintf(os.Stderr, "Authentication failed: %v\n", err)
				os.Exit(1)
			}

			// Retry listing
			bots, err = apiClient.ListBots(auth)
			if err != nil {
				red.Fprintf(os.Stderr, "Scan failed: %v\n", err)
				os.Exit(1)
			}
		} else {
			red.Fprintf(os.Stderr, "Scan failed: %v\n", err)
			os.Exit(1)
		}
	}

	if len(bots) == 0 {
		blue.Println("No bots in the grid. Time to deploy your first trading bot 🤖")
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

	green.Printf("Found %d active bot(s) ⚡\n\n", len(bots))
	if err := table.Render(); err != nil {
		red.Fprintf(os.Stderr, "Failed to render table: %v\n", err)
		os.Exit(1)
	}
	green.Println("\nBots locked and loaded. Ready to dominate the markets 📈")
}

func updateBotInstance(cmd *cobra.Command, args []string) {
	green := color.New(color.FgGreen)
	red := color.New(color.FgRed)
	blue := color.New(color.FgBlue)

	botID := args[0]
	configPath := args[1]

	blue.Println("Updating bot...")
	fmt.Printf("Bot ID: %s\n", botID)
	fmt.Printf("Config: %s\n", configPath)

	// Load and parse config.json
	configData, err := os.ReadFile(configPath)
	if err != nil {
		red.Fprintf(os.Stderr, "Failed to read config file: %v\n", err)
		os.Exit(1)
	}

	var configMap map[string]interface{}
	if err := json.Unmarshal(configData, &configMap); err != nil {
		red.Fprintf(os.Stderr, "Invalid JSON in config file: %v\n", err)
		os.Exit(1)
	}

	green.Println("✓ Config loaded")

	// Get auth token
	auth, err := internal.GetAuthTokenWithRetry()
	if err != nil {
		red.Fprintf(os.Stderr, "Authentication failed: %v\n", err)
		os.Exit(1)
	}

	green.Println("✓ Connected to the0")

	// Extract name from config (optional)
	var botName string
	if nameVal, ok := configMap["name"].(string); ok {
		botName = nameVal
	}

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
			blue.Println("Access denied. Reconnecting to the grid...")
			auth, err = internal.PromptForNewAPIKey()
			if err != nil {
				red.Fprintf(os.Stderr, "Authentication failed: %v\n", err)
				os.Exit(1)
			}

			// Retry update
			err = apiClient.UpdateBotInstance(auth, botID, request)
			if err != nil {
				red.Fprintf(os.Stderr, "Update failed: %v\n", err)
				os.Exit(1)
			}
		} else {
			red.Fprintf(os.Stderr, "Update failed: %v\n", err)
			os.Exit(1)
		}
	}

	green.Println("Bot updated ⚡")
	fmt.Printf("Bot ID: %s\n", botID)
	green.Println("Bot configuration updated and active")
}

func deleteBotInstance(cmd *cobra.Command, args []string) {
	green := color.New(color.FgGreen)
	red := color.New(color.FgRed)
	blue := color.New(color.FgBlue)
	yellow := color.New(color.FgYellow)

	botID := args[0]

	yellow.Printf("⚠️ Are you sure you want to delete bot '%s'?\n", botID)
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
		blue.Println("Deletion cancelled. Bot remains active 📈")
		return
	}

	blue.Println("🗑️ Starting bot deletion process...")

	// Get auth token
	auth, err := internal.GetAuthTokenWithRetry()
	if err != nil {
		red.Fprintf(os.Stderr, "Authentication failed: %v\n", err)
		os.Exit(1)
	}

	green.Println("✓ Connected to the0")

	// Delete bot
	apiClient := internal.NewAPIClient(internal.GetAPIBaseURL())
	err = apiClient.DeleteBotInstance(auth, botID)
	if err != nil {
		// Retry with new auth if needed
		if internal.IsAuthError(err) {
			blue.Println("Access denied. Reconnecting to the grid...")
			auth, err = internal.PromptForNewAPIKey()
			if err != nil {
				red.Fprintf(os.Stderr, "Authentication failed: %v\n", err)
				os.Exit(1)
			}

			// Retry deletion
			err = apiClient.DeleteBotInstance(auth, botID)
			if err != nil {
				red.Fprintf(os.Stderr, "Deletion failed: %v\n", err)
				os.Exit(1)
			}
		} else {
			red.Fprintf(os.Stderr, "Deletion failed: %v\n", err)
			os.Exit(1)
		}
	}

	green.Println("Bot terminated 💀")
	fmt.Printf("Bot ID: %s\n", botID)
	green.Println("Bot purged from the grid")
}

func getBotLogs(cmd *cobra.Command, args []string, watchMode bool, limit int) {
	red := color.New(color.FgRed)
	blue := color.New(color.FgBlue)
	yellow := color.New(color.FgYellow)

	botID := args[0]
	var dateParam string

	// Parse date/dateRange parameter
	if len(args) > 1 {
		dateParam = args[1]
		if err := validateDateParam(dateParam); err != nil {
			red.Fprintf(os.Stderr, "Invalid date format: %v\n", err)
			os.Exit(1)
		}
	} else {
		// Default to current date
		dateParam = time.Now().Format("20060102")
	}

	// Validate limit
	if limit < 1 || limit > 1000 {
		red.Fprintf(os.Stderr, "Limit must be between 1 and 1000\n")
		os.Exit(1)
	}

	blue.Printf("⚡ Accessing bot logs from the grid: %s\n", botID)
	if watchMode {
		yellow.Println("👁️ Watch mode active - monitoring live log stream (press Ctrl+C to stop)")
	}

	// Get auth token
	auth, err := internal.GetAuthTokenWithRetry()
	if err != nil {
		red.Fprintf(os.Stderr, "Authentication failed: %v\n", err)
		os.Exit(1)
	}

	apiClient := internal.NewAPIClient(internal.GetAPIBaseURL())

	if watchMode {
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
	green := color.New(color.FgGreen)
	red := color.New(color.FgRed)

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
			blue := color.New(color.FgBlue)
			blue.Println("Access denied. Reconnecting to the grid...")
			auth, err = internal.PromptForNewAPIKey()
			if err != nil {
				red.Fprintf(os.Stderr, "Authentication failed: %v\n", err)
				os.Exit(1)
			}

			// Retry fetching logs
			logs, err = apiClient.GetBotLogs(auth, botID, params)
			if err != nil {
				red.Fprintf(os.Stderr, "Failed to fetch logs: %v\n", err)
				os.Exit(1)
			}
		} else {
			red.Fprintf(os.Stderr, "Failed to fetch logs: %v\n", err)
			os.Exit(1)
		}
	}

	if len(logs) == 0 {
		blue := color.New(color.FgBlue)
		blue.Println("🔍 No logs found in the grid for the specified criteria")
		return
	}

	// Display logs
	displayLogEntries(logs)
	green.Printf("\n✓ Bot log entries retrieved from the grid: %d\n", len(logs))
}

func watchBotLogs(apiClient *internal.APIClient, auth *internal.Auth, botID, dateParam string, limit int) {
	red := color.New(color.FgRed)
	yellow := color.New(color.FgYellow)

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
		red.Fprintf(os.Stderr, "Failed to fetch logs: %v\n", err)
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

	yellow.Println("\n⚡ Live log monitoring active - streaming from the grid... (press Ctrl+C to disconnect)")

	for {
		select {
		case <-sigChan:
			yellow.Println("\n🔌 Stopping live log stream monitor...")
			return
		case <-ticker.C:
			// Fetch new logs since last timestamp
			newLogs, err := fetchLogsWithRetry(apiClient, auth, botID, params)
			if err != nil {
				red.Printf("Error fetching logs: %v\n", err)
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
