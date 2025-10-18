# PRP: Backtest CLI Support

## 📋 Feature Overview

Implement comprehensive backtest management commands for the the0 CLI, enabling users to deploy, monitor, and manage trading bot backtests using the same robust patterns as existing bot commands.

## 🎯 Context and Research Findings

### Existing Codebase Patterns (CRITICAL TO FOLLOW)

#### 1. Command Structure Patterns
The codebase uses Cobra with established patterns in `cmd/bot.go`, `cmd/custom_bot.go`:

```go
// Parent command structure from cmd/bot.go
func NewBotCmd() *cobra.Command {
    cmd := &cobra.Command{
        Use:   "bot",
        Short: "Manage your bot instances",
        Long:  "Deploy, list, update, and delete your trading bots on the0 platform",
    }
    cmd.AddCommand(NewBotDeployCmd())
    cmd.AddCommand(NewBotListCmd())
    cmd.AddCommand(NewBotDeleteCmd())
    return cmd
}

// Command function pattern from cmd/bot.go (deployBotInstance function)
func deployBotInstance(cmd *cobra.Command, args []string) {
    // 1. Setup colors
    green := color.New(color.FgGreen)
    red := color.New(color.FgRed)

    // 2. Process arguments
    configPath := args[0]

    // 3. Load and validate configuration
    configData, err := os.ReadFile(configPath)
    if err != nil {
        red.Fprintf(os.Stderr, "❌ Error reading config file: %v\n", err)
        os.Exit(1)
    }

    // 4. Get authentication with retry
    auth, err := internal.GetAuthTokenWithRetry()
    if err != nil {
        red.Fprintf(os.Stderr, "❌ Authentication failed: %v\n", err)
        os.Exit(1)
    }

    // 5. Create API client and make request
    apiClient := internal.NewAPIClient(internal.GetAPIBaseURL())

    // 6. Handle response with success/error formatting
    green.Println("✅ Bot deployed to the0 ⚡")
}
```

#### 2. API Client Patterns (internal/api.go)
The codebase uses consistent API client patterns with robust error handling:

```go
// Standard API call pattern from ListBots method
func (c *APIClient) ListBots(auth *Auth) ([]BotInstance, error) {
    req, err := http.NewRequest("GET", c.BaseURL+"/bot", nil)
    if err != nil {
        return nil, fmt.Errorf("failed to create request: %v", err)
    }

    req.Header.Set("Authorization", "ApiKey "+auth.APIKey)

    resp, err := c.HTTPClient.Do(req)
    if err != nil {
        return nil, fmt.Errorf("network error: %v", err)
    }
    defer resp.Body.Close()

    // Handle authentication errors specifically
    if resp.StatusCode == 401 || resp.StatusCode == 403 {
        return nil, fmt.Errorf("authentication failed: API key is invalid or revoked")
    }

    // Parse response with fallback for different formats
    body, err := io.ReadAll(resp.Body)
    if err != nil {
        return nil, fmt.Errorf("failed to read response body: %v", err)
    }

    // Handle wrapped API responses
    var apiResponse BotListAPIResponse
    if err := json.Unmarshal(body, &apiResponse); err == nil {
        if !apiResponse.Success {
            return nil, fmt.Errorf("API returned error: %s", apiResponse.Message)
        }
        return apiResponse.Data, nil
    }

    // Fallback parsing
    var bots []BotInstance
    if err := json.Unmarshal(body, &bots); err != nil {
        return nil, fmt.Errorf("failed to parse API response: %v", err)
    }

    return bots, nil
}
```

#### 3. Authentication Retry Pattern
All commands must use the authentication retry pattern:

```go
// From cmd/bot.go - handle authentication errors with retry
if internal.IsAuthError(err) {
    red.Printf("❌ Authentication expired. Please re-enter your API key.\n")
    auth, err = internal.PromptForNewAPIKey()
    if err != nil {
        red.Fprintf(os.Stderr, "❌ Failed to get new API key: %v\n", err)
        os.Exit(1)
    }
    // Retry the operation with new auth
}
```

#### 4. Configuration Transformation Pattern
Commands transform flat JSON configs to API format:

```go
// From cmd/bot.go - transform flat config to API format
var config map[string]any
if err := json.Unmarshal(configData, &config); err != nil {
    red.Fprintf(os.Stderr, "❌ Invalid JSON in config file: %v\n", err)
    os.Exit(1)
}

// Transform flat config to API format
request := &internal.BotDeployRequest{
    Name:   config["name"].(string),
    Config: config,
}
```

#### 5. Table Output Pattern
List commands use consistent table formatting:

```go
// From cmd/bot.go - table output pattern
func listBotInstances(cmd *cobra.Command, args []string) {
    // ... get bots from API

    table := tablewriter.NewWriter(os.Stdout)
    table.SetHeader([]string{"ID", "Name", "Status", "Created"})
    table.SetAutoWrapText(false)
    table.SetAutoFormatHeaders(true)
    table.SetHeaderAlignment(tablewriter.ALIGN_LEFT)
    table.SetAlignment(tablewriter.ALIGN_LEFT)
    table.SetCenterSeparator("")
    table.SetColumnSeparator("")
    table.SetRowSeparator("")
    table.SetHeaderLine(false)
    table.SetBorder(false)
    table.SetTablePadding("\t")
    table.SetNoWhiteSpace(true)

    for _, bot := range bots {
        status := "🟢 Running" // Add status indicators
        table.Append([]string{bot.ID, bot.Name, status, bot.CreatedAt})
    }

    table.Render()
}
```

### Existing Backtest Infrastructure

IMPORTANT: The codebase already has partial backtest support:

1. **Schema Support**: `internal/api.go:27` has `Schema` struct with `Backtest map[string]any`
2. **Config Support**: `internal/config.go` supports backtest entrypoints and schemas
3. **Custom Bot Commands**: `cmd/custom_bot.go` already has `the0 custom-bot schema <bot|backtest>` command

### Data Structures to Reference

**Existing Relevant Structures** (`internal/api.go`):
```go
type BotInstance struct {
    ID        string         `json:"id"`
    Name      string         `json:"name"`
    Config    map[string]any `json:"config"`
    Topic     string         `json:"topic"`
    CreatedAt   string `json:"createdAt"`
    UpdatedAt   string `json:"updatedAt"`
    UserID      string `json:"userId"`
    CustomBotId string `json:"customBotId"`
}

type BotDeployRequest struct {
    Name   string         `json:"name"`
    Config map[string]any `json:"config"`
}
```

## 🔧 Implementation Blueprint

### Step 1: Add Backtest Data Structures to internal/api.go

Add these new structures following existing patterns:

```go
// BacktestInstance represents a deployed backtest instance
type BacktestInstance struct {
    ID          string         `json:"id"`
    Name        string         `json:"name"`
    Config      map[string]any `json:"config"`
    Status      string         `json:"status"`
    Progress    float64        `json:"progress,omitempty"`
    Results     map[string]any `json:"results,omitempty"`
    CreatedAt   string         `json:"createdAt"`
    UpdatedAt   string         `json:"updatedAt"`
    UserID      string         `json:"userId"`
    CustomBotId string         `json:"customBotId"`
}

// BacktestDeployRequest represents the request for deploying a backtest
type BacktestDeployRequest struct {
    Name   string         `json:"name"`
    Config map[string]any `json:"config"`
}

// BacktestAPIResponse represents the full API response structure for backtests
type BacktestAPIResponse struct {
    Success bool           `json:"success"`
    Data    []BacktestInstance `json:"data"`
    Message string         `json:"message"`
}

// SingleBacktestAPIResponse represents API response for single backtest operations
type SingleBacktestAPIResponse struct {
    Success bool         `json:"success"`
    Data    BacktestInstance `json:"data"`
    Message string       `json:"message"`
}
```

### Step 2: Add API Client Methods to internal/api.go

Add these methods following the exact pattern of existing bot methods:

```go
// CreateBacktest deploys a new backtest instance
func (c *APIClient) CreateBacktest(auth *Auth, request *BacktestDeployRequest) (*BacktestInstance, error) {
    requestData, err := json.Marshal(request)
    if err != nil {
        return nil, fmt.Errorf("failed to marshal request: %v", err)
    }

    req, err := http.NewRequest("POST", c.BaseURL+"/backtest", bytes.NewBuffer(requestData))
    if err != nil {
        return nil, fmt.Errorf("failed to create request: %v", err)
    }

    req.Header.Set("Authorization", "ApiKey "+auth.APIKey)
    req.Header.Set("Content-Type", "application/json")

    resp, err := c.HTTPClient.Do(req)
    if err != nil {
        return nil, fmt.Errorf("network error: %v", err)
    }
    defer resp.Body.Close()

    if resp.StatusCode == 401 || resp.StatusCode == 403 {
        return nil, fmt.Errorf("authentication failed: API key is invalid or revoked")
    }

    if resp.StatusCode != 200 && resp.StatusCode != 201 {
        return nil, fmt.Errorf("API error: %d", resp.StatusCode)
    }

    body, err := io.ReadAll(resp.Body)
    if err != nil {
        return nil, fmt.Errorf("failed to read response body: %v", err)
    }

    // Handle wrapped API response
    var apiResponse SingleBacktestAPIResponse
    if err := json.Unmarshal(body, &apiResponse); err == nil {
        if !apiResponse.Success {
            return nil, fmt.Errorf("API returned error: %s", apiResponse.Message)
        }
        return &apiResponse.Data, nil
    }

    // Fallback: direct parsing
    var backtest BacktestInstance
    if err := json.Unmarshal(body, &backtest); err != nil {
        return nil, fmt.Errorf("failed to parse API response: %v", err)
    }

    return &backtest, nil
}

// ListBacktests retrieves all backtest instances for the authenticated user
func (c *APIClient) ListBacktests(auth *Auth) ([]BacktestInstance, error) {
    req, err := http.NewRequest("GET", c.BaseURL+"/backtest", nil)
    if err != nil {
        return nil, fmt.Errorf("failed to create request: %v", err)
    }

    req.Header.Set("Authorization", "ApiKey "+auth.APIKey)

    resp, err := c.HTTPClient.Do(req)
    if err != nil {
        return nil, fmt.Errorf("network error: %v", err)
    }
    defer resp.Body.Close()

    if resp.StatusCode == 401 || resp.StatusCode == 403 {
        return nil, fmt.Errorf("authentication failed: API key is invalid or revoked")
    }

    body, err := io.ReadAll(resp.Body)
    if err != nil {
        return nil, fmt.Errorf("failed to read response body: %v", err)
    }

    // Handle wrapped API response
    var apiResponse BacktestAPIResponse
    if err := json.Unmarshal(body, &apiResponse); err == nil {
        if !apiResponse.Success {
            return nil, fmt.Errorf("API returned error: %s", apiResponse.Message)
        }
        return apiResponse.Data, nil
    }

    // Fallback: direct array parsing
    var backtests []BacktestInstance
    if err := json.Unmarshal(body, &backtests); err != nil {
        return nil, fmt.Errorf("failed to parse API response: %v", err)
    }

    return backtests, nil
}

// DeleteBacktest deletes a specific backtest instance
func (c *APIClient) DeleteBacktest(auth *Auth, backtestID string) error {
    req, err := http.NewRequest("DELETE", c.BaseURL+"/backtest/"+backtestID, nil)
    if err != nil {
        return fmt.Errorf("failed to create request: %v", err)
    }

    req.Header.Set("Authorization", "ApiKey "+auth.APIKey)

    resp, err := c.HTTPClient.Do(req)
    if err != nil {
        return fmt.Errorf("network error: %v", err)
    }
    defer resp.Body.Close()

    if resp.StatusCode == 401 || resp.StatusCode == 403 {
        return fmt.Errorf("authentication failed: API key is invalid or revoked")
    }

    if resp.StatusCode == 404 {
        return fmt.Errorf("backtest not found: %s", backtestID)
    }

    if resp.StatusCode != 200 && resp.StatusCode != 204 {
        return fmt.Errorf("API error: %d", resp.StatusCode)
    }

    return nil
}
```

### Step 3: Create cmd/backtest.go

Create the complete backtest command structure following existing patterns:

```go
package cmd

import (
    "fmt"
    "os"
    "strings"

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

    // Get authentication
    auth, err := internal.GetAuthTokenWithRetry()
    if err != nil {
        red.Fprintf(os.Stderr, "❌ Authentication failed: %v\n", err)
        os.Exit(1)
    }

    // Create API client and deploy backtest
    apiClient := internal.NewAPIClient(internal.GetAPIBaseURL())

    request := &internal.BacktestDeployRequest{
        Name:   config["name"].(string),
        Config: config,
    }

    backtest, err := apiClient.CreateBacktest(auth, request)
    if err != nil {
        if internal.IsAuthError(err) {
            red.Printf("❌ Authentication expired. Please re-enter your API key.\n")
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
    yellow := color.New(color.FgYellow)

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
            red.Printf("❌ Authentication expired. Please re-enter your API key.\n")
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
        yellow.Println("⚠️  No backtests found. Deploy your first backtest with:")
        yellow.Println("   the0 backtest deploy <config.json>")
        return
    }

    green.Printf("📊 Found %d backtest(s):\n\n", len(backtests))

    table := tablewriter.NewWriter(os.Stdout)
    table.SetHeader([]string{"ID", "Name", "Status", "Progress", "Created"})
    table.SetAutoWrapText(false)
    table.SetAutoFormatHeaders(true)
    table.SetHeaderAlignment(tablewriter.ALIGN_LEFT)
    table.SetAlignment(tablewriter.ALIGN_LEFT)
    table.SetCenterSeparator("")
    table.SetColumnSeparator("")
    table.SetRowSeparator("")
    table.SetHeaderLine(false)
    table.SetBorder(false)
    table.SetTablePadding("\t")
    table.SetNoWhiteSpace(true)

    for _, backtest := range backtests {
        status := formatBacktestStatus(backtest.Status)
        progress := formatProgress(backtest.Progress)
        created := formatTime(backtest.CreatedAt)

        table.Append([]string{
            truncateID(backtest.ID),
            backtest.Name,
            status,
            progress,
            created,
        })
    }

    table.Render()
}

func deleteBacktest(cmd *cobra.Command, args []string) {
    green := color.New(color.FgGreen)
    red := color.New(color.FgRed)
    yellow := color.New(color.FgYellow)

    backtestID := args[0]

    // Get authentication
    auth, err := internal.GetAuthTokenWithRetry()
    if err != nil {
        red.Fprintf(os.Stderr, "❌ Authentication failed: %v\n", err)
        os.Exit(1)
    }

    // Create API client
    apiClient := internal.NewAPIClient(internal.GetAPIBaseURL())

    // Show confirmation prompt
    yellow.Printf("⚠️  Are you sure you want to delete backtest '%s'? This action cannot be undone.\n", backtestID)
    yellow.Print("Type 'yes' to confirm: ")

    var confirmation string
    fmt.Scanln(&confirmation)

    if strings.ToLower(confirmation) != "yes" {
        red.Println("❌ Deletion cancelled.")
        os.Exit(0)
    }

    // Delete backtest
    err = apiClient.DeleteBacktest(auth, backtestID)
    if err != nil {
        if internal.IsAuthError(err) {
            red.Printf("❌ Authentication expired. Please re-enter your API key.\n")
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

func formatTime(timestamp string) string {
    // Parse and format timestamp (use existing time formatting patterns)
    if timestamp == "" {
        return "-"
    }
    // Add proper time formatting similar to existing commands
    return timestamp[:10] // Simple truncation for now
}

func truncateID(id string) string {
    if len(id) <= 8 {
        return id
    }
    return id[:8] + "..."
}
```

### Step 4: Update main.go

Add the backtest command group to root commands:

```go
// In main.go, around line 36, add:
rootCmd.AddCommand(cmd.NewBacktestCmd())
```

### Step 5: Create Comprehensive Tests

Create `tests/backtest_test.go` following existing patterns:

```go
package internal

import (
    "fmt"
    "net/http"
    "net/http/httptest"
    "testing"
    "the0/internal"
)

func TestAPIClient_CreateBacktest(t *testing.T) {
    tests := []struct {
        name           string
        statusCode     int
        responseBody   string
        expectedError  bool
        errorContains  string
    }{
        {
            name:       "successful backtest creation",
            statusCode: 201,
            responseBody: `{
                "success": true,
                "data": {
                    "id": "bt_123456789",
                    "name": "test-backtest",
                    "status": "pending",
                    "config": {"type": "test"},
                    "createdAt": "2024-01-01T00:00:00Z",
                    "updatedAt": "2024-01-01T00:00:00Z"
                }
            }`,
            expectedError: false,
        },
        {
            name:          "authentication failed",
            statusCode:    401,
            responseBody:  `{"error": "Unauthorized"}`,
            expectedError: true,
            errorContains: "authentication failed",
        },
        {
            name:          "invalid request",
            statusCode:    400,
            responseBody:  `{"success": false, "message": "Invalid configuration"}`,
            expectedError: true,
            errorContains: "Invalid configuration",
        },
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
                if r.URL.Path != "/backtest" {
                    t.Errorf("Expected path /backtest, got %s", r.URL.Path)
                }

                if r.Method != "POST" {
                    t.Errorf("Expected POST method, got %s", r.Method)
                }

                authHeader := r.Header.Get("Authorization")
                if !strings.HasPrefix(authHeader, "ApiKey ") {
                    t.Errorf("Expected Authorization header with ApiKey prefix, got %s", authHeader)
                }

                w.WriteHeader(tt.statusCode)
                w.Write([]byte(tt.responseBody))
            }))
            defer server.Close()

            client := internal.NewAPIClient(server.URL)
            auth := &internal.Auth{APIKey: "test-key", CreatedAt: time.Now()}

            request := &internal.BacktestDeployRequest{
                Name: "test-backtest",
                Config: map[string]any{
                    "type":    "test",
                    "version": "1.0.0",
                },
            }

            backtest, err := client.CreateBacktest(auth, request)

            if tt.expectedError {
                if err == nil {
                    t.Errorf("Expected error but got nil")
                } else if !strings.Contains(err.Error(), tt.errorContains) {
                    t.Errorf("Expected error containing '%s', got '%s'", tt.errorContains, err.Error())
                }
            } else {
                if err != nil {
                    t.Errorf("Unexpected error: %v", err)
                }
                if backtest.ID != "bt_123456789" {
                    t.Errorf("Expected backtest ID 'bt_123456789', got '%s'", backtest.ID)
                }
            }
        })
    }
}

func TestAPIClient_ListBacktests(t *testing.T) {
    tests := []struct {
        name          string
        statusCode    int
        responseBody  string
        expectedCount int
        expectedError bool
        errorContains string
    }{
        {
            name:       "successful listing with multiple backtests",
            statusCode: 200,
            responseBody: `{
                "success": true,
                "data": [
                    {
                        "id": "bt_123",
                        "name": "backtest-1",
                        "status": "completed",
                        "createdAt": "2024-01-01T00:00:00Z"
                    },
                    {
                        "id": "bt_456",
                        "name": "backtest-2",
                        "status": "running",
                        "createdAt": "2024-01-02T00:00:00Z"
                    }
                ]
            }`,
            expectedCount: 2,
            expectedError: false,
        },
        {
            name:          "unauthorized",
            statusCode:    401,
            responseBody:  `{"error": "Unauthorized"}`,
            expectedError: true,
            errorContains: "authentication failed",
        },
        {
            name:          "empty list",
            statusCode:    200,
            responseBody:  `{"success": true, "data": []}`,
            expectedCount: 0,
            expectedError: false,
        },
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
                if r.URL.Path != "/backtest" {
                    t.Errorf("Expected path /backtest, got %s", r.URL.Path)
                }

                if r.Method != "GET" {
                    t.Errorf("Expected GET method, got %s", r.Method)
                }

                w.WriteHeader(tt.statusCode)
                w.Write([]byte(tt.responseBody))
            }))
            defer server.Close()

            client := internal.NewAPIClient(server.URL)
            auth := &internal.Auth{APIKey: "test-key", CreatedAt: time.Now()}

            backtests, err := client.ListBacktests(auth)

            if tt.expectedError {
                if err == nil {
                    t.Errorf("Expected error but got nil")
                } else if !strings.Contains(err.Error(), tt.errorContains) {
                    t.Errorf("Expected error containing '%s', got '%s'", tt.errorContains, err.Error())
                }
            } else {
                if err != nil {
                    t.Errorf("Unexpected error: %v", err)
                }
                if len(backtests) != tt.expectedCount {
                    t.Errorf("Expected %d backtests, got %d", tt.expectedCount, len(backtests))
                }
            }
        })
    }
}

func TestAPIClient_DeleteBacktest(t *testing.T) {
    tests := []struct {
        name          string
        backtestID    string
        statusCode    int
        responseBody  string
        expectedError bool
        errorContains string
    }{
        {
            name:          "successful deletion",
            backtestID:    "bt_123456789",
            statusCode:    200,
            responseBody:  `{"success": true, "message": "Backtest deleted"}`,
            expectedError: false,
        },
        {
            name:          "backtest not found",
            backtestID:    "bt_nonexistent",
            statusCode:    404,
            responseBody:  `{"error": "Not found"}`,
            expectedError: true,
            errorContains: "not found",
        },
        {
            name:          "unauthorized",
            backtestID:    "bt_123456789",
            statusCode:    401,
            responseBody:  `{"error": "Unauthorized"}`,
            expectedError: true,
            errorContains: "authentication failed",
        },
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
                expectedPath := "/backtest/" + tt.backtestID
                if r.URL.Path != expectedPath {
                    t.Errorf("Expected path %s, got %s", expectedPath, r.URL.Path)
                }

                if r.Method != "DELETE" {
                    t.Errorf("Expected DELETE method, got %s", r.Method)
                }

                w.WriteHeader(tt.statusCode)
                w.Write([]byte(tt.responseBody))
            }))
            defer server.Close()

            client := internal.NewAPIClient(server.URL)
            auth := &internal.Auth{APIKey: "test-key", CreatedAt: time.Now()}

            err := client.DeleteBacktest(auth, tt.backtestID)

            if tt.expectedError {
                if err == nil {
                    t.Errorf("Expected error but got nil")
                } else if !strings.Contains(err.Error(), tt.errorContains) {
                    t.Errorf("Expected error containing '%s', got '%s'", tt.errorContains, err.Error())
                }
            } else {
                if err != nil {
                    t.Errorf("Unexpected error: %v", err)
                }
            }
        })
    }
}
```

Create `tests/cli_backtest_test.go` for CLI command testing:

```go
package cmd

import (
    "testing"
    "github.com/spf13/cobra"
)

func TestBacktestDeployCommand_Arguments(t *testing.T) {
    tests := []struct {
        name        string
        args        []string
        expectError bool
        errorMsg    string
    }{
        {
            name:        "correct single argument",
            args:        []string{"config.json"},
            expectError: false,
        },
        {
            name:        "no arguments",
            args:        []string{},
            expectError: true,
            errorMsg:    "accepts 1 arg(s), received 0",
        },
        {
            name:        "too many arguments",
            args:        []string{"config.json", "extra"},
            expectError: true,
            errorMsg:    "accepts 1 arg(s), received 2",
        },
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            cmd := NewBacktestDeployCmd()
            err := cmd.Args(cmd, tt.args)

            if tt.expectError {
                if err == nil {
                    t.Errorf("Expected error but got nil")
                } else if tt.errorMsg != "" && !contains(err.Error(), tt.errorMsg) {
                    t.Errorf("Expected error containing '%s', got '%s'", tt.errorMsg, err.Error())
                }
            } else {
                if err != nil {
                    t.Errorf("Unexpected error: %v", err)
                }
            }
        })
    }
}

func TestBacktestListCommand_Arguments(t *testing.T) {
    tests := []struct {
        name        string
        args        []string
        expectError bool
    }{
        {
            name:        "no arguments (correct)",
            args:        []string{},
            expectError: false,
        },
        {
            name:        "with arguments (incorrect)",
            args:        []string{"extra"},
            expectError: true,
        },
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            cmd := NewBacktestListCmd()
            err := cmd.Args(cmd, tt.args)

            if tt.expectError {
                if err == nil {
                    t.Errorf("Expected error but got nil")
                }
            } else {
                if err != nil {
                    t.Errorf("Unexpected error: %v", err)
                }
            }
        })
    }
}

func TestBacktestDeleteCommand_Arguments(t *testing.T) {
    tests := []struct {
        name        string
        args        []string
        expectError bool
        errorMsg    string
    }{
        {
            name:        "correct single argument",
            args:        []string{"bt_123456789"},
            expectError: false,
        },
        {
            name:        "no arguments",
            args:        []string{},
            expectError: true,
            errorMsg:    "accepts 1 arg(s), received 0",
        },
        {
            name:        "too many arguments",
            args:        []string{"bt_123", "extra"},
            expectError: true,
            errorMsg:    "accepts 1 arg(s), received 2",
        },
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            cmd := NewBacktestDeleteCmd()
            err := cmd.Args(cmd, tt.args)

            if tt.expectError {
                if err == nil {
                    t.Errorf("Expected error but got nil")
                } else if tt.errorMsg != "" && !contains(err.Error(), tt.errorMsg) {
                    t.Errorf("Expected error containing '%s', got '%s'", tt.errorMsg, err.Error())
                }
            } else {
                if err != nil {
                    t.Errorf("Unexpected error: %v", err)
                }
            }
        })
    }
}

func contains(s, substr string) bool {
    return len(s) >= len(substr) && (s == substr || len(substr) == 0 ||
        (len(s) > len(substr) && containsAt(s, substr)))
}

func containsAt(s, substr string) bool {
    for i := 0; i <= len(s)-len(substr); i++ {
        if s[i:i+len(substr)] == substr {
            return true
        }
    }
    return false
}
```

## ✅ Validation Gates

### Build and Syntax Validation
```bash
# Build the CLI to ensure no syntax errors
make build

# Verify all code is properly formatted
make fmt

# Run linting to ensure code quality
make lint
```

### Unit Tests
```bash
# Run all tests including new backtest tests
make test

# Run tests with coverage to ensure quality
make test-coverage
```

### Integration Tests
```bash
# Test CLI command structures (help commands)
./bin/the0 backtest --help
./bin/the0 backtest deploy --help
./bin/the0 backtest list --help
./bin/the0 backtest delete --help

# Test argument validation
./bin/the0 backtest deploy  # Should fail with argument error
./bin/the0 backtest list extra-arg  # Should fail with argument error
./bin/the0 backtest delete  # Should fail with argument error
```

## 🚨 Critical Implementation Notes

### MUST FOLLOW These Exact Patterns:

1. **Error Handling**: Use the exact same error handling patterns with `internal.IsAuthError()` and retry logic
2. **Output Formatting**: Use the same table formatting with `tablewriter` library and consistent styling
3. **Color Usage**: Use the same color patterns (`green`, `red`, `blue`, `yellow`)
4. **Emoji Usage**: Follow existing emoji patterns for success (✅), errors (❌), warnings (⚠️), etc.
5. **API Response Parsing**: Handle both wrapped API responses and direct parsing with fallbacks
6. **Authentication**: Always use `internal.GetAuthTokenWithRetry()` and implement retry logic
7. **Configuration Validation**: Validate required fields (`name`, `type`, `version`) before API calls

### File Structure to Create/Modify:
```
cmd/
├── backtest.go          # NEW: Complete backtest command implementation
├── bot.go               # EXISTING: Reference for patterns
├── custom_bot.go        # EXISTING: Reference for patterns
└── ...

internal/
├── api.go              # MODIFY: Add backtest data structures and methods
├── auth.go             # EXISTING: Authentication patterns
└── config.go           # EXISTING: Configuration patterns

main.go                 # MODIFY: Add backtest command to root

tests/
├── api_test.go         # MODIFY: Add backtest API tests
├── cli_backtest_test.go # NEW: CLI command tests
└── ...
```

### Order of Implementation:
1. **First**: Add data structures and API methods to `internal/api.go`
2. **Second**: Create `cmd/backtest.go` with all command implementations
3. **Third**: Update `main.go` to register the new command
4. **Fourth**: Create comprehensive test files
5. **Fifth**: Test build and functionality

### Testing Requirements:
- All API methods must have comprehensive test coverage
- All CLI commands must have argument validation tests
- Test both success and failure scenarios
- Test authentication retry logic
- Test API error handling

## 📊 Success Metrics

1. **All commands work**: `the0 backtest deploy`, `the0 backtest list`, `the0 backtest delete`
2. **Consistent UX**: Same styling, colors, emojis, and table format as existing commands
3. **Robust error handling**: Authentication retry, network errors, validation errors
4. **Complete test coverage**: API methods and CLI commands thoroughly tested
5. **Code quality**: Passes linting, formatting, and build checks

## 🔗 External Documentation References

- **Cobra CLI Documentation**: https://github.com/spf13/cobra
- **Tablewriter Library**: https://github.com/olekukonko/tablewriter
- **Fatih Color Library**: https://github.com/fatih/color
- **Go Testing Practices**: https://go.dev/doc/effective_go#testing

---

**Confidence Score: 9/10** - This PRP provides comprehensive context with exact code patterns, complete implementation blueprint, and thorough validation strategy. The high confidence is due to the detailed analysis of existing codebase patterns and step-by-step implementation guidance that mirrors established conventions exactly.