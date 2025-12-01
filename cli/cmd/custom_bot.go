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

func NewCustomBotCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "custom-bot",
		Short: "Manage your custom trading bots",
		Long:  "Deploy, update, and manage your custom trading bots on the0 platform",
	}

	cmd.AddCommand(NewDeployCmd())
	cmd.AddCommand(NewCustomBotListCmd())
	cmd.AddCommand(NewCustomBotSchemaCmd())
	cmd.AddCommand(NewCustomBotVersionsCmd())
	return cmd
}

func NewDeployCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "deploy",
		Short: "Deploy your bot to the0 platform",
		Long:  "Deploy your trading bot to the0 platform",
		Run:   deployBot,
	}
}

func deployBot(cmd *cobra.Command, args []string) {
	fmt.Println("Deploying custom bot...")
	fmt.Println("Compiling trading neural pathways")

	// Step 1: Load bot config
	config, err := internal.LoadBotConfig()
	if err != nil {
		fmt.Fprintf(os.Stderr, "%v\n", err)
		os.Exit(1)
	}

	fmt.Printf("✓ Config loaded: %s v%s\n", config.Name, config.Version)

	// Step 2: Validate config
	if err := internal.ValidateBotConfig(config); err != nil {
		fmt.Fprintf(os.Stderr, "Config validation failed: %v\n", err)
		os.Exit(1)
	}

	fmt.Println("✓ Config validated")

	// Step 2.5: Check for dependencies and perform compilation if needed
	if err := internal.PerformVendoringIfNeeded("."); err != nil {
		fmt.Fprintf(os.Stderr, "Dependency compilation failed: %v\n", err)
		internal.CleanupVendoring(".")
		os.Exit(1)
	}

	// Step 3: Get auth token (with retry on failure)
	auth, err := internal.GetAuthTokenWithRetry()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Authentication failed: %v\n", err)
		os.Exit(1)
	}

	fmt.Println("✓ Access granted")

	// Step 4: Check if bot exists and validate ownership
	apiClient := internal.NewAPIClient(internal.GetAPIBaseURL())
	isUpdate, err := apiClient.CheckBotExists(config, auth)
	if err != nil {
		// If auth error, try to get new token and retry
		if internal.IsAuthError(err) {
			fmt.Println("Access denied. Reconnecting...")
			auth, err = internal.PromptForNewAPIKey()
			if err != nil {
				fmt.Fprintf(os.Stderr, "Authentication failed: %v\n", err)
				internal.CleanupVendoring(".")
				os.Exit(1)
			}

			// Retry with new key
			isUpdate, err = apiClient.CheckBotExists(config, auth)
			if err != nil {
				fmt.Fprintf(os.Stderr, "Bot validation failed: %v\n", err)
				internal.CleanupVendoring(".")
				os.Exit(1)
			}
		} else {
			fmt.Fprintf(os.Stderr, "Bot validation failed: %v\n", err)
			internal.CleanupVendoring(".")
			os.Exit(1)
		}
	}

	if isUpdate {
		fmt.Println("Updating existing bot")
	} else {
		fmt.Println("Deploying new bot")
	}

	// Step 5: Create ZIP file
	zipPath, err := internal.CreateBotZip()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to create ZIP: %v\n", err)
		internal.CleanupVendoring(".")
		os.Exit(1)
	}
	defer os.Remove(zipPath)

	fmt.Println("✓ Bot packaged")

	// Step 6: Deploy to API with multipart upload
	if err := apiClient.DeployBot(config, auth, zipPath, isUpdate); err != nil {
		// If auth error, try to get new token and retry
		if internal.IsAuthError(err) {
			fmt.Println("Access denied. Reconnecting...")
			auth, err = internal.PromptForNewAPIKey()
			if err != nil {
				fmt.Fprintf(os.Stderr, "Authentication failed: %v\n", err)
				internal.CleanupVendoring(".")
				os.Exit(1)
			}

			// Retry deployment with new key
			if err := apiClient.DeployBot(config, auth, zipPath, isUpdate); err != nil {
				fmt.Fprintf(os.Stderr, "Deployment failed: %v\n", err)
				internal.CleanupVendoring(".")
				os.Exit(1)
			}
		} else {
			fmt.Fprintf(os.Stderr, "Deployment failed: %v\n", err)
			internal.CleanupVendoring(".")
			os.Exit(1)
		}
	}

	fmt.Println("Bot uploaded to the plaform ⚡")
	fmt.Printf("'%s' v%s deployed successfully\n", config.Name, config.Version)
	fmt.Println("Awaiting review by the 0vers33r 👀")
}

func NewCustomBotListCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "list",
		Short: "List all your deployed custom bots",
		Long:  "📋 List all the custom bots you have deployed with their details",
		Run:   listCustomBots,
	}
}

func NewCustomBotSchemaCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "schema <version> <custom-bot-name>",
		Short: "Get the schema of a custom bot",
		Long:  "📄 Get the JSON schema for a custom bot",
		Args:  cobra.RangeArgs(1, 2), // 1 or 2 arguments
		Run:   getCustomBotSchema,
	}
}

func listCustomBots(cmd *cobra.Command, args []string) {
	green := color.New(color.FgGreen)
	red := color.New(color.FgRed)
	blue := color.New(color.FgBlue)

	blue.Println("Scanning deployed bots...")

	// Get auth token
	auth, err := internal.GetAuthTokenWithRetry()
	if err != nil {
		red.Fprintf(os.Stderr, "Authentication failed: %v\n", err)
		os.Exit(1)
	}

	// List custom bots
	apiClient := internal.NewAPIClient(internal.GetAPIBaseURL())
	customBots, err := apiClient.ListCustomBots(auth)
	if err != nil {
		// Retry with new auth if needed
		if internal.IsAuthError(err) {
			blue.Println("🔑 API key appears to be invalid. Let's get a new one...")
			auth, err = internal.PromptForNewAPIKey()
			if err != nil {
				red.Fprintf(os.Stderr, "Authentication failed: %v\n", err)
				os.Exit(1)
			}

			// Retry listing
			customBots, err = apiClient.ListCustomBots(auth)
			if err != nil {
				red.Fprintf(os.Stderr, "Failed to list custom bots: %v\n", err)
				os.Exit(1)
			}
		} else {
			red.Fprintf(os.Stderr, "Failed to list custom bots: %v\n", err)
			os.Exit(1)
		}
	}

	if len(customBots) == 0 {
		blue.Println("No custom bots detected. Deploy your first trading bot 🤖")
		return
	}

	// Create table
	table := tablewriter.NewWriter(os.Stdout)
	table.Header("Custom Bot Name", "Latest Version", "Created At", "Type")

	for _, customBot := range customBots {
		// Convert ISO timestamp to readable date
		createdTime, err := time.Parse(time.RFC3339, customBot.CreatedAt)
		if err != nil {
			createdTime = time.Now() // fallback
		}
		createdStr := createdTime.Format("2006-01-02 15:04")

		// Get the latest version's config to determine type
		var botType string
		if len(customBot.Versions) > 0 {
			// Find the latest version
			for _, version := range customBot.Versions {
				if version.Version == customBot.LatestVersion {
					botType = fmt.Sprintf("%s/%s", version.Config.Type, version.Config.Name)
					break
				}
			}
		}

		if botType == "" {
			botType = "unknown/unknown"
		}

		table.Append(customBot.Name, customBot.LatestVersion, createdStr, botType)
	}

	green.Printf("Found %d custom bot(s) ⚡\n\n", len(customBots))
	if err := table.Render(); err != nil {
		red.Fprintf(os.Stderr, "Failed to render table: %v\n", err)
		os.Exit(1)
	}
	green.Println("\nCustom bot portfolio ready for deployment ⚡")
}

func getCustomBotSchema(cmd *cobra.Command, args []string) {
	green := color.New(color.FgGreen)
	red := color.New(color.FgRed)
	blue := color.New(color.FgBlue)

	version := args[0]
	var customBotName string

	if len(args) == 2 {
		customBotName = args[1]
	} else {
		// If version is actually the bot name and no version specified, use latest
		customBotName = version
		version = "" // Will use latest version
	}

	blue.Printf("📄 Fetching schema for custom bot '%s'...\n", customBotName)

	// Get auth token
	auth, err := internal.GetAuthTokenWithRetry()
	if err != nil {
		red.Fprintf(os.Stderr, "Authentication failed: %v\n", err)
		os.Exit(1)
	}

	// Get custom bot details
	apiClient := internal.NewAPIClient(internal.GetAPIBaseURL())
	customBot, err := apiClient.GetCustomBot(auth, customBotName)
	if err != nil {
		// Retry with new auth if needed
		if internal.IsAuthError(err) {
			blue.Println("🔑 API key appears to be invalid. Let's get a new one...")
			auth, err = internal.PromptForNewAPIKey()
			if err != nil {
				red.Fprintf(os.Stderr, "Authentication failed: %v\n", err)
				os.Exit(1)
			}

			// Retry fetching
			customBot, err = apiClient.GetCustomBot(auth, customBotName)
			if err != nil {
				red.Fprintf(os.Stderr, "Failed to get custom bot: %v\n", err)
				os.Exit(1)
			}
		} else {
			red.Fprintf(os.Stderr, "Failed to get custom bot: %v\n", err)
			os.Exit(1)
		}
	}

	// Find the requested version or use latest
	var targetVersion *internal.CustomBotVersion
	if version == "" {
		version = customBot.LatestVersion
	}

	for _, v := range customBot.Versions {
		if v.Version == version {
			targetVersion = &v
			break
		}
	}

	if targetVersion == nil {
		red.Fprintf(os.Stderr, "Version '%s' not found for custom bot '%s'\n", version, customBotName)
		os.Exit(1)
	}

	// Get the bot schema
	schema := targetVersion.Config.Schema.Bot

	if schema == nil || len(schema) == 0 {
		blue.Printf("📄 No schema found for custom bot '%s' version '%s'\n", customBotName, version)
		return
	}

	// Pretty print the schema
	schemaJSON, err := json.MarshalIndent(schema, "", "  ")
	if err != nil {
		red.Fprintf(os.Stderr, "Failed to format schema: %v\n", err)
		os.Exit(1)
	}

	green.Printf("📄 Schema for custom bot '%s' version '%s':\n\n", customBotName, version)
	fmt.Println(string(schemaJSON))
}

func NewCustomBotVersionsCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "versions <type|name>",
		Short: "List all versions of a custom bot",
		Long:  "📋 List all versions for a specific custom bot type",
		Args:  cobra.ExactArgs(1),
		Run:   getCustomBotVersions,
	}
}

func getCustomBotVersions(cmd *cobra.Command, args []string) {
	green := color.New(color.FgGreen)
	red := color.New(color.FgRed)
	blue := color.New(color.FgBlue)

	typeName := args[0]

	blue.Printf("📋 Fetching versions for custom bot '%s'...\n", typeName)

	auth, err := internal.GetAuthTokenWithRetry()
	if err != nil {
		red.Fprintf(os.Stderr, "Authentication failed: %v\n", err)
		os.Exit(1)
	}

	apiClient := internal.NewAPIClient(internal.GetAPIBaseURL())
	customBots, err := apiClient.ListCustomBots(auth)
	if err != nil {
		if internal.IsAuthError(err) {
			blue.Println("🔑 API key appears to be invalid. Let's get a new one...")
			auth, err = internal.PromptForNewAPIKey()
			if err != nil {
				red.Fprintf(os.Stderr, "Authentication failed: %v\n", err)
				os.Exit(1)
			}

			customBots, err = apiClient.ListCustomBots(auth)
			if err != nil {
				red.Fprintf(os.Stderr, "Failed to list custom bots: %v\n", err)
				os.Exit(1)
			}
		} else {
			red.Fprintf(os.Stderr, "Failed to list custom bots: %v\n", err)
			os.Exit(1)
		}
	}

	var targetCustomBot *internal.CustomBotData
	for _, customBot := range customBots {
		if len(customBot.Versions) > 0 {
			for _, version := range customBot.Versions {
				if version.Version == customBot.LatestVersion {
					botType := fmt.Sprintf("%s/%s", version.Config.Type, version.Config.Name)
					if botType == typeName || customBot.Name == typeName {
						targetCustomBot = &customBot
						break
					}
				}
			}
			if targetCustomBot != nil {
				break
			}
		}
	}

	if targetCustomBot == nil {
		red.Fprintf(os.Stderr, "Custom bot not found: %s\n", typeName)
		os.Exit(1)
	}

	if len(targetCustomBot.Versions) == 0 {
		blue.Printf("🤷 No versions found for custom bot '%s'\n", typeName)
		return
	}

	table := tablewriter.NewWriter(os.Stdout)
	table.Header("Version", "Created At", "Type")

	for _, version := range targetCustomBot.Versions {
		createdTime, err := time.Parse(time.RFC3339, version.CreatedAt)
		if err != nil {
			createdTime = time.Now() // fallback
		}
		createdStr := createdTime.Format("2006-01-02 15:04")

		botType := fmt.Sprintf("%s/%s", version.Config.Type, version.Config.Name)

		table.Append(version.Version, createdStr, botType)
	}

	green.Printf("📋 Found %d version(s) for custom bot '%s':\n\n", len(targetCustomBot.Versions), typeName)
	if err := table.Render(); err != nil {
		red.Fprintf(os.Stderr, "Failed to render table: %v\n", err)
		os.Exit(1)
	}
	green.Println("\n🎉 Version history looking solid! 🚀")
}
