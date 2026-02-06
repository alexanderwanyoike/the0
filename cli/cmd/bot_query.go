package cmd

import (
	"encoding/json"
	"fmt"
	"os"

	"github.com/spf13/cobra"
	"the0/internal"
	"the0/internal/logger"
)

func NewBotQueryCmd() *cobra.Command {
	var paramsJSON string
	var timeout int
	var raw bool

	cmd := &cobra.Command{
		Use:   "query <bot_id> <query_path>",
		Short: "Execute a query against a bot",
		Long: `Execute a custom query against a bot's query handlers.

The query_path should match a registered handler in the bot's code.
Common paths include /portfolio, /status, /health, etc.

Examples:
  the0 bot query my-bot /portfolio
  the0 bot query my-bot /status --params '{"symbol":"BTC"}'
  the0 bot query my-bot /positions --timeout 60`,
		Args: cobra.ExactArgs(2),
		Run: func(cmd *cobra.Command, args []string) {
			executeBotQuery(args[0], args[1], paramsJSON, timeout, raw)
		},
	}

	cmd.Flags().StringVarP(&paramsJSON, "params", "p", "{}", "Query parameters as JSON")
	cmd.Flags().IntVarP(&timeout, "timeout", "t", 30, "Query timeout in seconds (default: 30)")
	cmd.Flags().BoolVarP(&raw, "raw", "r", false, "Output raw JSON without formatting")

	return cmd
}

func executeBotQuery(botID, queryPath, paramsJSON string, timeout int, raw bool) {
	// Parse params JSON
	var params map[string]interface{}
	if err := json.Unmarshal([]byte(paramsJSON), &params); err != nil {
		logger.Error("Invalid params JSON: %v", err)
		os.Exit(1)
	}

	// Ensure query_path starts with /
	if len(queryPath) > 0 && queryPath[0] != '/' {
		queryPath = "/" + queryPath
	}

	logger.StartSpinner("Executing query")
	logger.Verbose("Bot ID: %s", botID)
	logger.Verbose("Query path: %s", queryPath)

	// Get auth token
	auth, err := internal.GetAuthTokenWithRetry()
	if err != nil {
		logger.StopSpinnerWithError("Authentication failed")
		logger.Error("%v", err)
		os.Exit(1)
	}

	// Execute query
	apiClient := internal.NewAPIClient(internal.GetAPIBaseURL())
	request := &internal.BotQueryRequest{
		QueryPath:  queryPath,
		Params:     params,
		TimeoutSec: timeout,
	}

	result, err := apiClient.ExecuteBotQuery(auth, botID, request)
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

			// Retry query
			result, err = apiClient.ExecuteBotQuery(auth, botID, request)
			if err != nil {
				logger.StopSpinnerWithError("Query failed")
				logger.Error("%v", err)
				os.Exit(1)
			}
		} else {
			logger.StopSpinnerWithError("Query failed")
			logger.Error("%v", err)
			os.Exit(1)
		}
	}

	logger.StopSpinner()

	// Output result
	if raw {
		// Raw JSON output
		jsonBytes, err := json.Marshal(result.Data)
		if err != nil {
			logger.Error("Failed to format result: %v", err)
			os.Exit(1)
		}
		fmt.Println(string(jsonBytes))
	} else {
		// Pretty print
		jsonBytes, err := json.MarshalIndent(result.Data, "", "  ")
		if err != nil {
			logger.Error("Failed to format result: %v", err)
			os.Exit(1)
		}
		logger.Success("Query executed successfully")
		logger.Newline()
		fmt.Println(string(jsonBytes))
	}
}
