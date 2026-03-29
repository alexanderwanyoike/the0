package cmd

import (
	"bufio"
	"encoding/json"
	"fmt"
	"os"
	"strings"
	"time"

	"github.com/olekukonko/tablewriter"
	"github.com/spf13/cobra"
	"the0/internal"
	"the0/internal/logger"
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
	cmd.AddCommand(NewCustomBotDeleteCmd())
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
	logger.StartSpinner("Starting deployment")

	// Step 1: Load bot config
	config, err := internal.LoadBotConfig()
	if err != nil {
		logger.StopSpinnerWithError("Failed to load config")
		logger.Error("%v", err)
		os.Exit(1)
	}

	logger.Verbose("Config loaded: %s v%s", config.Name, config.Version)

	// Step 2: Validate config
	if err := internal.ValidateBotConfig(config); err != nil {
		logger.StopSpinnerWithError("Config validation failed")
		logger.Error("%v", err)
		os.Exit(1)
	}

	logger.UpdateSpinner("Installing dependencies")

	// Step 2.5: Check for dependencies and perform compilation if needed
	if err := internal.PerformVendoringIfNeeded("."); err != nil {
		logger.StopSpinnerWithError("Dependency installation failed")
		logger.Error("%v", err)
		internal.CleanupVendoring(".")
		os.Exit(1)
	}

	// Step 2.6: Build compiled languages (Rust, C++, C#, Scala, Haskell)
	logger.UpdateSpinner("Building project")
	if err := internal.BuildCompiledLanguagesIfNeeded("."); err != nil {
		logger.StopSpinnerWithError("Build failed")
		logger.Error("%v", err)
		internal.CleanupVendoring(".")
		os.Exit(1)
	}

	// Step 2.7: Build frontend if frontend/ directory exists
	logger.UpdateSpinner("Building frontend")
	if err := internal.BuildFrontendIfNeeded("."); err != nil {
		logger.StopSpinnerWithError("Frontend build failed")
		logger.Error("%v", err)
		internal.CleanupVendoring(".")
		os.Exit(1)
	}

	// Step 2.8: Validate that all build outputs exist
	logger.UpdateSpinner("Validating build outputs")
	if err := internal.ValidateBotFiles(config); err != nil {
		logger.StopSpinnerWithError("Build validation failed")
		logger.Error("%v", err)
		internal.CleanupVendoring(".")
		os.Exit(1)
	}

	logger.UpdateSpinner("Authenticating")

	// Step 3: Get auth token (with retry on failure)
	auth, err := internal.GetAuthTokenWithRetry()
	if err != nil {
		logger.StopSpinnerWithError("Authentication failed")
		logger.Error("%v", err)
		os.Exit(1)
	}

	logger.UpdateSpinner("Checking bot status")

	// Step 4: Check if bot exists and validate ownership
	apiClient := internal.NewAPIClient(internal.GetAPIBaseURL())
	isUpdate, err := apiClient.CheckBotExists(config, auth)
	if err != nil {
		// If auth error, try to get new token and retry
		if internal.IsAuthError(err) {
			logger.UpdateSpinner("Session expired. Re-authenticating")
			auth, err = internal.PromptForNewAPIKey()
			if err != nil {
				logger.StopSpinnerWithError("Authentication failed")
				logger.Error("%v", err)
				internal.CleanupVendoring(".")
				os.Exit(1)
			}

			// Retry with new key
			isUpdate, err = apiClient.CheckBotExists(config, auth)
			if err != nil {
				logger.StopSpinnerWithError("Bot validation failed")
				logger.Error("%v", err)
				internal.CleanupVendoring(".")
				os.Exit(1)
			}
		} else {
			logger.StopSpinnerWithError("Bot validation failed")
			logger.Error("%v", err)
			internal.CleanupVendoring(".")
			os.Exit(1)
		}
	}

	if isUpdate {
		logger.UpdateSpinner("Updating existing bot")
	} else {
		logger.UpdateSpinner("Deploying new bot")
	}

	// Step 5: Create ZIP file
	zipPath, err := internal.CreateBotZip()
	if err != nil {
		logger.StopSpinnerWithError("Failed to create package")
		logger.Error("%v", err)
		internal.CleanupVendoring(".")
		os.Exit(1)
	}
	defer os.Remove(zipPath)

	logger.StopSpinner()

	// Step 6: Deploy to API with multipart upload (DeployBot handles its own spinner)
	if err := apiClient.DeployBot(config, auth, zipPath, isUpdate); err != nil {
		// If auth error, try to get new token and retry
		if internal.IsAuthError(err) {
			logger.StartSpinner("Session expired. Re-authenticating")
			auth, err = internal.PromptForNewAPIKey()
			if err != nil {
				logger.StopSpinnerWithError("Authentication failed")
				logger.Error("%v", err)
				internal.CleanupVendoring(".")
				os.Exit(1)
			}
			logger.StopSpinner()

			// Retry deployment with new key
			if err := apiClient.DeployBot(config, auth, zipPath, isUpdate); err != nil {
				logger.Error("Deployment failed: %v", err)
				internal.CleanupVendoring(".")
				os.Exit(1)
			}
		} else {
			logger.Error("Deployment failed: %v", err)
			internal.CleanupVendoring(".")
			os.Exit(1)
		}
	}

	logger.Print("'%s' v%s deployed successfully", config.Name, config.Version)
}

func NewCustomBotListCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "list",
		Short: "List all your deployed custom bots",
		Long:  "List all the custom bots you have deployed with their details",
		Run:   listCustomBots,
	}
}

func NewCustomBotSchemaCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "schema <version> <custom-bot-name>",
		Short: "Get the schema of a custom bot",
		Long:  "Get the JSON schema for a custom bot",
		Args:  cobra.RangeArgs(1, 2), // 1 or 2 arguments
		Run:   getCustomBotSchema,
	}
}

func listCustomBots(cmd *cobra.Command, args []string) {
	logger.StartSpinner("Fetching custom bots")

	// Get auth token
	auth, err := internal.GetAuthTokenWithRetry()
	if err != nil {
		logger.StopSpinnerWithError("Authentication failed")
		logger.Error("%v", err)
		os.Exit(1)
	}

	// List custom bots
	apiClient := internal.NewAPIClient(internal.GetAPIBaseURL())
	customBots, err := apiClient.ListCustomBots(auth)
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
			customBots, err = apiClient.ListCustomBots(auth)
			if err != nil {
				logger.StopSpinnerWithError("Failed to list custom bots")
				logger.Error("%v", err)
				os.Exit(1)
			}
		} else {
			logger.StopSpinnerWithError("Failed to list custom bots")
			logger.Error("%v", err)
			os.Exit(1)
		}
	}

	logger.StopSpinner()

	if len(customBots) == 0 {
		logger.Info("No custom bots found")
		logger.Print("Deploy your first bot with 'the0 custom-bot deploy'")
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

	logger.Success("Found %d custom bot(s)", len(customBots))
	logger.Newline()
	if err := table.Render(); err != nil {
		logger.Error("Failed to render table: %v", err)
		os.Exit(1)
	}
}

func getCustomBotSchema(cmd *cobra.Command, args []string) {
	version := args[0]
	var customBotName string

	if len(args) == 2 {
		customBotName = args[1]
	} else {
		// If version is actually the bot name and no version specified, use latest
		customBotName = version
		version = "" // Will use latest version
	}

	logger.StartSpinner("Fetching schema for " + customBotName)

	// Get auth token
	auth, err := internal.GetAuthTokenWithRetry()
	if err != nil {
		logger.StopSpinnerWithError("Authentication failed")
		logger.Error("%v", err)
		os.Exit(1)
	}

	// Get custom bot details
	apiClient := internal.NewAPIClient(internal.GetAPIBaseURL())
	customBot, err := apiClient.GetCustomBot(auth, customBotName)
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

			// Retry fetching
			customBot, err = apiClient.GetCustomBot(auth, customBotName)
			if err != nil {
				logger.StopSpinnerWithError("Failed to get custom bot")
				logger.Error("%v", err)
				os.Exit(1)
			}
		} else {
			logger.StopSpinnerWithError("Failed to get custom bot")
			logger.Error("%v", err)
			os.Exit(1)
		}
	}

	logger.StopSpinner()

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
		logger.Error("Version '%s' not found for custom bot '%s'", version, customBotName)
		os.Exit(1)
	}

	// Get the bot schema
	schema := targetVersion.Config.Schema.Bot

	if schema == nil || len(schema) == 0 {
		logger.Info("No schema found for custom bot '%s' version '%s'", customBotName, version)
		return
	}

	// Pretty print the schema
	schemaJSON, err := json.MarshalIndent(schema, "", "  ")
	if err != nil {
		logger.Error("Failed to format schema: %v", err)
		os.Exit(1)
	}

	logger.Success("Schema for '%s' v%s:", customBotName, version)
	logger.Newline()
	fmt.Println(string(schemaJSON))
}

func NewCustomBotVersionsCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "versions <type|name>",
		Short: "List all versions of a custom bot",
		Long:  "List all versions for a specific custom bot type",
		Args:  cobra.ExactArgs(1),
		Run:   getCustomBotVersions,
	}
}

func getCustomBotVersions(cmd *cobra.Command, args []string) {
	botName := args[0]

	logger.StartSpinner("Fetching versions for " + botName)

	auth, err := internal.GetAuthTokenWithRetry()
	if err != nil {
		logger.StopSpinnerWithError("Authentication failed")
		logger.Error("%v", err)
		os.Exit(1)
	}

	apiClient := internal.NewAPIClient(internal.GetAPIBaseURL())
	versions, err := apiClient.GetCustomBotVersionsWithInstances(auth, botName)
	if err != nil {
		if internal.IsAuthError(err) {
			logger.UpdateSpinner("Session expired. Re-authenticating")
			auth, err = internal.PromptForNewAPIKey()
			if err != nil {
				logger.StopSpinnerWithError("Authentication failed")
				logger.Error("%v", err)
				os.Exit(1)
			}

			versions, err = apiClient.GetCustomBotVersionsWithInstances(auth, botName)
			if err != nil {
				logger.StopSpinnerWithError("Failed to fetch versions")
				logger.Error("%v", err)
				os.Exit(1)
			}
		} else {
			logger.StopSpinnerWithError("Failed to fetch versions")
			logger.Error("%v", err)
			os.Exit(1)
		}
	}

	logger.StopSpinner()

	if len(versions) == 0 {
		logger.Info("No versions found for custom bot '%s'", botName)
		return
	}

	table := tablewriter.NewWriter(os.Stdout)
	table.Header("Version", "Instances", "Created At", "Type")

	for _, version := range versions {
		createdTime, err := time.Parse(time.RFC3339, version.CreatedAt)
		if err != nil {
			createdTime = time.Now() // fallback
		}
		createdStr := createdTime.Format("2006-01-02 15:04")

		botType := fmt.Sprintf("%s/%s", version.Config.Type, version.Config.Name)

		instanceStr := "unused"
		if version.InstanceCount > 0 {
			instanceStr = fmt.Sprintf("%d instance(s)", version.InstanceCount)
		}

		table.Append(version.Version, instanceStr, createdStr, botType)
	}

	logger.Success("Found %d version(s) for '%s':", len(versions), botName)
	logger.Newline()
	if err := table.Render(); err != nil {
		logger.Error("Failed to render table: %v", err)
		os.Exit(1)
	}
}

func NewCustomBotDeleteCmd() *cobra.Command {
	var version string
	var skipConfirm bool

	cmd := &cobra.Command{
		Use:   "delete <name>",
		Short: "Delete a custom bot or specific version",
		Long:  "Delete all versions of a custom bot, or a specific version with --version flag",
		Args:  cobra.ExactArgs(1),
		Run: func(cmd *cobra.Command, args []string) {
			deleteCustomBot(cmd, args, version, skipConfirm)
		},
	}

	cmd.Flags().StringVar(&version, "version", "", "Delete a specific version")
	cmd.Flags().BoolVarP(&skipConfirm, "yes", "y", false, "Skip confirmation prompt")

	return cmd
}

func deleteCustomBot(cmd *cobra.Command, args []string, version string, skipConfirm bool) {
	botName := args[0]

	if !skipConfirm {
		if version != "" {
			logger.Warning("Are you sure you want to delete version '%s' of '%s'?", version, botName)
		} else {
			logger.Warning("Are you sure you want to delete ALL versions of '%s'?", botName)
		}
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

	logger.StartSpinner("Deleting custom bot")

	auth, err := internal.GetAuthTokenWithRetry()
	if err != nil {
		logger.StopSpinnerWithError("Authentication failed")
		logger.Error("%v", err)
		os.Exit(1)
	}

	apiClient := internal.NewAPIClient(internal.GetAPIBaseURL())

	if version != "" {
		err = apiClient.DeleteCustomBotVersion(auth, botName, version)
	} else {
		err = apiClient.DeleteCustomBot(auth, botName)
	}

	if err != nil {
		if internal.IsAuthError(err) {
			logger.UpdateSpinner("Session expired. Re-authenticating")
			auth, err = internal.PromptForNewAPIKey()
			if err != nil {
				logger.StopSpinnerWithError("Authentication failed")
				logger.Error("%v", err)
				os.Exit(1)
			}

			if version != "" {
				err = apiClient.DeleteCustomBotVersion(auth, botName, version)
			} else {
				err = apiClient.DeleteCustomBot(auth, botName)
			}

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

	logger.StopSpinner()

	if version != "" {
		logger.Success("Version '%s' of '%s' deleted successfully", version, botName)
	} else {
		logger.Success("All versions of '%s' deleted successfully", botName)
	}
}
