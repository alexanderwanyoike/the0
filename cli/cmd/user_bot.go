package cmd

import (
	"encoding/json"
	"fmt"
	"os"
	"time"

	"github.com/fatih/color"
	"github.com/olekukonko/tablewriter"
	"github.com/spf13/cobra"
	"the0/internal"
)

func NewUserBotCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "user-bot",
		Short: "Manage your installed bot instances",
		Long:  "🤖 List and manage your installed bot instances on the0 platform",
	}

	cmd.AddCommand(NewUserBotListCmd())
	cmd.AddCommand(NewUserBotSchemaCmd())
	cmd.AddCommand(NewUserBotVersionsCmd())
	return cmd
}

func NewUserBotListCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "list",
		Short: "List all your installed bots",
		Long:  "📋 List all the bots you have installed with their details",
		Run:   listUserBots,
	}
}

func listUserBots(cmd *cobra.Command, args []string) {
	green := color.New(color.FgGreen)
	red := color.New(color.FgRed)
	blue := color.New(color.FgBlue)

	blue.Println("Scanning installed bots...")

	// Get auth token
	auth, err := internal.GetAuthTokenWithRetry()
	if err != nil {
		red.Fprintf(os.Stderr, "Authentication failed: %v\n", err)
		os.Exit(1)
	}

	// List user bots
	apiClient := internal.NewAPIClient(internal.GetAPIBaseURL())
	userBots, err := apiClient.ListUserBots(auth)
	if err != nil {
		// Retry with new auth if needed
		if internal.IsAuthError(err) {
			blue.Println("Access denied. Reconnecting...")
			auth, err = internal.PromptForNewAPIKey()
			if err != nil {
				red.Fprintf(os.Stderr, "Authentication failed: %v\n", err)
				os.Exit(1)
			}

			// Retry listing
			userBots, err = apiClient.ListUserBots(auth)
			if err != nil {
				red.Fprintf(os.Stderr, "Failed to list user bots: %v\n", err)
				os.Exit(1)
			}
		} else {
			red.Fprintf(os.Stderr, "Failed to list user bots: %v\n", err)
			os.Exit(1)
		}
	}

	if len(userBots) == 0 {
		blue.Println("No installed bots detected. Time to install some bots 🤖")
		return
	}

	// Create table
	table := tablewriter.NewWriter(os.Stdout)
	table.Header("Custom Bot Name", "Version", "Type", "Acquired At")

	for _, userBot := range userBots {
		// Convert ISO timestamp to readable date
		acquiredTime, err := time.Parse(time.RFC3339, userBot.AcquiredAt)
		if err != nil {
			acquiredTime = time.Now() // fallback
		}
		acquiredStr := acquiredTime.Format("2006-01-02 15:04")

		// Get version and type using helper functions
		version := getUserBotVersion(userBot)
		botType := getUserBotType(userBot)

		table.Append(userBot.CustomBotName, version, botType, acquiredStr)
	}

	green.Printf("Found %d installed bot(s) ⚡\n\n", len(userBots))
	if err := table.Render(); err != nil {
		red.Fprintf(os.Stderr, "Failed to render table: %v\n", err)
		os.Exit(1)
	}
	green.Println("\nBot collection ready for trading ⚡")
}

// getUserBotVersion returns the latest version of a custom bot or "N/A" if not available
func getUserBotVersion(userBot internal.UserBot) string {
	if userBot.CustomBot == nil {
		return "N/A"
	}
	if userBot.CustomBot.LatestVersion == "" {
		return "N/A"
	}
	return userBot.CustomBot.LatestVersion
}

// getUserBotType returns the type in format "type/customBotName" or "N/A" if not available
func getUserBotType(userBot internal.UserBot) string {
	if userBot.CustomBot == nil {
		return "N/A"
	}

	// Find the latest version to get the type
	latestVersion := userBot.CustomBot.LatestVersion
	if latestVersion == "" {
		return "N/A"
	}

	for _, version := range userBot.CustomBot.Versions {
		if version.Version == latestVersion {
			if version.Config.Type == "" {
				return "N/A"
			}
			return version.Config.Type + "/" + userBot.CustomBotName
		}
	}

	return "N/A"
}

func NewUserBotSchemaCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "schema <type|name> <version> [bot|backtest]",
		Short: "Get the schema of a user bot",
		Long:  "📄 Get the JSON schema for either the bot or backtest entrypoint of a user bot",
		Args:  cobra.RangeArgs(2, 3),
		Run:   getUserBotSchema,
	}
}

func NewUserBotVersionsCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "versions <type|name>",
		Short: "List all versions of a user bot",
		Long:  "📋 List all versions for a specific user bot type",
		Args:  cobra.ExactArgs(1),
		Run:   getUserBotVersions,
	}
}

func getUserBotSchema(cmd *cobra.Command, args []string) {
	green := color.New(color.FgGreen)
	red := color.New(color.FgRed)
	blue := color.New(color.FgBlue)

	typeName := args[0]
	version := args[1]
	schemaType := "bot"
	if len(args) == 3 {
		schemaType = args[2]
	}

	if schemaType != "bot" && schemaType != "backtest" {
		red.Fprintf(os.Stderr, "Schema type must be 'bot' or 'backtest', got: %s\n", schemaType)
		os.Exit(1)
	}

	blue.Printf("📄 Fetching %s schema for user bot '%s' version '%s'...\n", schemaType, typeName, version)

	auth, err := internal.GetAuthTokenWithRetry()
	if err != nil {
		red.Fprintf(os.Stderr, "Authentication failed: %v\n", err)
		os.Exit(1)
	}

	apiClient := internal.NewAPIClient(internal.GetAPIBaseURL())
	userBots, err := apiClient.ListUserBots(auth)
	if err != nil {
		if internal.IsAuthError(err) {
			blue.Println("Access denied. Reconnecting...")
			auth, err = internal.PromptForNewAPIKey()
			if err != nil {
				red.Fprintf(os.Stderr, "Authentication failed: %v\n", err)
				os.Exit(1)
			}

			userBots, err = apiClient.ListUserBots(auth)
			if err != nil {
				red.Fprintf(os.Stderr, "Failed to list user bots: %v\n", err)
				os.Exit(1)
			}
		} else {
			red.Fprintf(os.Stderr, "Failed to list user bots: %v\n", err)
			os.Exit(1)
		}
	}

	var targetUserBot *internal.UserBot
	for _, userBot := range userBots {
		if userBot.CustomBot != nil {
			botType := getUserBotType(userBot)
			if botType == typeName {
				targetUserBot = &userBot
				break
			}
		}
	}

	if targetUserBot == nil {
		red.Fprintf(os.Stderr, "User bot not found: %s\n", typeName)
		os.Exit(1)
	}

	var targetVersion *internal.CustomBotVersion
	for _, v := range targetUserBot.CustomBot.Versions {
		if v.Version == version {
			targetVersion = &v
			break
		}
	}

	if targetVersion == nil {
		red.Fprintf(os.Stderr, "Version '%s' not found for user bot '%s'\n", version, typeName)
		os.Exit(1)
	}

	var schema map[string]any
	if schemaType == "bot" {
		schema = targetVersion.Config.Schema.Bot
	} else { // schemaType == "backtest"
		schema = targetVersion.Config.Schema.Backtest
	}

	if schema == nil || len(schema) == 0 {
		blue.Printf("📄 No %s schema found for user bot '%s' version '%s'\n", schemaType, typeName, version)
		return
	}

	schemaJSON, err := json.MarshalIndent(schema, "", "  ")
	if err != nil {
		red.Fprintf(os.Stderr, "Failed to format schema: %v\n", err)
		os.Exit(1)
	}

	green.Printf("📄 Schema for user bot '%s' version '%s' (%s entry point):\n\n", typeName, version, schemaType)
	fmt.Println(string(schemaJSON))
}

func getUserBotVersions(cmd *cobra.Command, args []string) {
	green := color.New(color.FgGreen)
	red := color.New(color.FgRed)
	blue := color.New(color.FgBlue)

	typeName := args[0]

	blue.Printf("📋 Fetching versions for user bot '%s'...\n", typeName)

	auth, err := internal.GetAuthTokenWithRetry()
	if err != nil {
		red.Fprintf(os.Stderr, "Authentication failed: %v\n", err)
		os.Exit(1)
	}

	apiClient := internal.NewAPIClient(internal.GetAPIBaseURL())
	userBots, err := apiClient.ListUserBots(auth)
	if err != nil {
		if internal.IsAuthError(err) {
			blue.Println("Access denied. Reconnecting...")
			auth, err = internal.PromptForNewAPIKey()
			if err != nil {
				red.Fprintf(os.Stderr, "Authentication failed: %v\n", err)
				os.Exit(1)
			}

			userBots, err = apiClient.ListUserBots(auth)
			if err != nil {
				red.Fprintf(os.Stderr, "Failed to list user bots: %v\n", err)
				os.Exit(1)
			}
		} else {
			red.Fprintf(os.Stderr, "Failed to list user bots: %v\n", err)
			os.Exit(1)
		}
	}

	var targetUserBot *internal.UserBot
	for _, userBot := range userBots {
		if userBot.CustomBot != nil {
			botType := getUserBotType(userBot)
			if botType == typeName {
				targetUserBot = &userBot
				break
			}
		}
	}

	if targetUserBot == nil {
		red.Fprintf(os.Stderr, "User bot not found: %s\n", typeName)
		os.Exit(1)
	}

	if len(targetUserBot.CustomBot.Versions) == 0 {
		blue.Printf("🤷 No versions found for user bot '%s'\n", typeName)
		return
	}

	table := tablewriter.NewWriter(os.Stdout)
	table.Header("Version", "Created At", "Type")

	for _, version := range targetUserBot.CustomBot.Versions {
		createdTime, err := time.Parse(time.RFC3339, version.CreatedAt)
		if err != nil {
			createdTime = time.Now() // fallback
		}
		createdStr := createdTime.Format("2006-01-02 15:04")

		botType := fmt.Sprintf("%s/%s", version.Config.Type, version.Config.Name)

		table.Append(version.Version, createdStr, botType)
	}

	green.Printf("📋 Found %d version(s) for user bot '%s':\n\n", len(targetUserBot.CustomBot.Versions), typeName)
	if err := table.Render(); err != nil {
		red.Fprintf(os.Stderr, "Failed to render table: %v\n", err)
		os.Exit(1)
	}
	green.Println("\n🎉 Version history looking good! 📈")
}
