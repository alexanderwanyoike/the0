package internal

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"time"

	"github.com/Masterminds/semver/v3"
)

const DEFAULT_API_URL = "http://localhost:3000"

// APIClient handles all API interactions
type APIClient struct {
	BaseURL    string
	HTTPClient *http.Client
}

// Schema represents the JSON schema structure
type Schema struct {
	Backtest map[string]any `json:"backtest"`
	Bot      map[string]any `json:"bot"`
}

// APIBotConfig represents the config structure returned by the API
type APIBotConfig struct {
	Name        string            `json:"name"`
	Description string            `json:"description"`
	Version     string            `json:"version"`
	Author      string            `json:"author"`
	Type        string            `json:"type"`
	Entrypoints map[string]string `json:"entrypoints"`
	Schema      Schema            `json:"schema"`
	Readme      string            `json:"readme"`
	Metadata    map[string]any    `json:"metadata"`
}

// CustomBotVersion represents a single version of a custom bot
type CustomBotVersion struct {
	Version   string       `json:"version"`
	Config    APIBotConfig `json:"config"`
	GcsPath   string       `json:"gcsPath"`
	CreatedAt string       `json:"createdAt"`
	UpdatedAt string       `json:"updatedAt"`
}

// CustomBotData represents the data structure returned by the API
type CustomBotData struct {
	ID            string             `json:"id"`
	Name          string             `json:"name"`
	UserId        string             `json:"userId"`
	LatestVersion string             `json:"latestVersion"`
	Versions      []CustomBotVersion `json:"versions"`
	CreatedAt     string `json:"createdAt"`
	UpdatedAt     string `json:"updatedAt"`
}

// CustomBotAPIResponse represents the full API response structure
type CustomBotAPIResponse struct {
	Success bool          `json:"success"`
	Data    CustomBotData `json:"data"`
	Message string        `json:"message"`
}

// BotInstance represents a deployed bot instance
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

// BotListResponse represents the response for listing bot instances
type BotListResponse []BotInstance

// BotDeployRequest represents the request for deploying a bot instance
type BotDeployRequest struct {
	Name   string         `json:"name"`
	Config map[string]any `json:"config"`
}

// BotUpdateRequest represents the request for updating a bot instance
type BotUpdateRequest struct {
	Name   string         `json:"name"`
	Config map[string]any `json:"config"`
}

// APIResponse represents a generic API response
type APIResponse struct {
	Success bool   `json:"success"`
	Data    any    `json:"data,omitempty"`
	Message string `json:"message"`
}

// UploadUrlRequest represents the request for getting an upload URL
type UploadUrlRequest struct {
	Version string `json:"version"`
}

// UploadUrlResponse represents the response for upload URL generation
type UploadUrlResponse struct {
	UploadUrl string `json:"uploadUrl"`
	FilePath  string `json:"filePath"`
	ExpiresAt string `json:"expiresAt"`
}

// CustomBotDeployRequest represents the new deploy request with file path
type CustomBotDeployRequest struct {
	Config   string `json:"config"`
	FilePath string `json:"filePath"`
}

// NewAPIClient creates a new API client
func NewAPIClient(baseURL string) *APIClient {
	return &APIClient{
		BaseURL: baseURL,
		HTTPClient: &http.Client{
			Timeout: 60 * time.Second,
		},
	}
}

// GetAPIBaseURL returns the API base URL from environment or default
func GetAPIBaseURL() string {
	if url := os.Getenv("THE0_API_URL"); url != "" {
		return url
	}
	return DEFAULT_API_URL
}

// APIKeyValidationResponse represents the response from the API key validation endpoint
type APIKeyValidationResponse struct {
	Success bool `json:"success"`
	Data    struct {
		Valid      bool   `json:"valid"`
		UserID     string `json:"userId"`
		KeyID      string `json:"keyId"`
		KeyName    string `json:"keyName"`
		LastUsedAt string `json:"lastUsedAt"`
	} `json:"data"`
	Message string `json:"message"`
}

// LogEntry represents a single log entry
type LogEntry struct {
	Date    string `json:"date"`
	Content string `json:"content"`
}

// LogsResponse represents the API response for bot logs
type LogsResponse struct {
	Success bool       `json:"success"`
	Data    []LogEntry `json:"data"`
	Message string     `json:"message"`
}

// LogsParams represents parameters for the logs API call
type LogsParams struct {
	Date      string `json:"date,omitempty"`
	DateRange string `json:"dateRange,omitempty"`
	Limit     int    `json:"limit,omitempty"`
}

// TestAPIKey tests if an API key is valid
func (c *APIClient) TestAPIKey(auth *Auth) error {
	req, err := http.NewRequest("GET", c.BaseURL+"/auth/validate-api-key", nil)
	if err != nil {
		return err
	}

	req.Header.Set("Authorization", "ApiKey "+auth.APIKey)

	resp, err := c.HTTPClient.Do(req)
	if err != nil {
		return fmt.Errorf("network error: %v", err)
	}
	defer resp.Body.Close()

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return fmt.Errorf("failed to read response body: %v", err)
	}

	if resp.StatusCode == 401 || resp.StatusCode == 403 {
		return fmt.Errorf("API key is invalid or revoked")
	}

	if resp.StatusCode >= 400 {
		return fmt.Errorf("API error: %d - %s", resp.StatusCode, string(body))
	}

	var apiResponse APIKeyValidationResponse
	if err := json.Unmarshal(body, &apiResponse); err != nil {
		return fmt.Errorf("failed to parse API response: %v", err)
	}

	if !apiResponse.Success {
		return fmt.Errorf("API key validation failed: %s", apiResponse.Message)
	}

	if !apiResponse.Data.Valid {
		return fmt.Errorf("API key is invalid")
	}

	return nil
}

// CheckBotExists checks if a bot exists and validates ownership
func (c *APIClient) CheckBotExists(config *BotConfig, auth *Auth) (bool, error) {
	req, err := http.NewRequest("GET", fmt.Sprintf("%s/custom-bots/%s", c.BaseURL, config.Name), nil)
	if err != nil {
		return false, err
	}

	req.Header.Set("Authorization", "ApiKey "+auth.APIKey)

	resp, err := c.HTTPClient.Do(req)
	if err != nil {
		return false, err
	}
	defer resp.Body.Close()

	if resp.StatusCode == 404 {
		// Bot doesn't exist, this is a new deployment
		return false, nil
	}

	if resp.StatusCode == 401 || resp.StatusCode == 403 {
		return false, fmt.Errorf("authentication failed: API key is invalid or revoked")
	}

	if resp.StatusCode != 200 {
		return false, fmt.Errorf("failed to check bot existence: HTTP %d", resp.StatusCode)
	}

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return false, fmt.Errorf("failed to read response body: %v", err)
	}

	var apiResponse CustomBotAPIResponse
	if err := json.Unmarshal(body, &apiResponse); err != nil {
		return false, fmt.Errorf("failed to parse API response: %v", err)
	}

	if !apiResponse.Success {
		return false, fmt.Errorf("API returned error: %s", apiResponse.Message)
	}

	botData := apiResponse.Data
	// Check version for updates
	currentVersion, err := semver.NewVersion(botData.LatestVersion)
	if err != nil {
		return false, fmt.Errorf("invalid current version: %v", err)
	}

	newVersion, err := semver.NewVersion(config.Version)
	if err != nil {
		return false, fmt.Errorf("invalid new version: %v", err)
	}

	if !newVersion.GreaterThan(currentVersion) {
		return false, fmt.Errorf("new version (%s) must be greater than current version (%s). Time to level up! 📈", config.Version, botData.LatestVersion)
	}

	fmt.Printf("Bot %s is ready for update! Current version: %s, New version: %s\n", config.Name, botData.LatestVersion, config.Version)

	return true, nil
}

// DeployBot deploys a bot using the new two-step process: upload to storage, then deploy
func (c *APIClient) DeployBot(config *BotConfig, auth *Auth, zipPath string, isUpdate bool) error {
	fmt.Println("🚀 Starting two-step deployment process...")

	// Step 1: Request upload URL
	fmt.Println("📡 Requesting upload URL...")
	uploadResponse, err := c.RequestUploadURL(config.Name, config.Version, auth)
	if err != nil {
		return fmt.Errorf("failed to get upload URL: %v", err)
	}

	// Step 2: Upload ZIP file directly to storage
	fmt.Printf("📦 Uploading %s to storage...\n", filepath.Base(zipPath))
	err = c.UploadToStorage(zipPath, uploadResponse.UploadUrl)
	if err != nil {
		return fmt.Errorf("failed to upload file to storage: %v", err)
	}
	fmt.Println("✅ File uploaded successfully!")

	// Step 3: Deploy with config and GCS path
	fmt.Println("🔧 Configuring deployment...")

	// Read README content
	readmeContent, err := os.ReadFile(config.Readme)
	if err != nil {
		return fmt.Errorf("failed to read README: %v", err)
	}

	// Read and parse JSON schema files
	var backtestSchema map[string]any
	if config.Schema.Backtest != "" {
		backtestSchemaData, err := os.ReadFile(config.Schema.Backtest)
		if err != nil {
			return fmt.Errorf("failed to read backtest schema: %v", err)
		}

		if err := json.Unmarshal(backtestSchemaData, &backtestSchema); err != nil {
			return fmt.Errorf("invalid JSON in backtest schema: %v", err)
		}
	}

	botSchemaData, err := os.ReadFile(config.Schema.Bot)
	if err != nil {
		return fmt.Errorf("failed to read bot schema: %v", err)
	}

	var botSchema map[string]any
	if err := json.Unmarshal(botSchemaData, &botSchema); err != nil {
		return fmt.Errorf("invalid JSON in bot schema: %v", err)
	}

	// Prepare entrypoints (only include backtest if specified)
	entrypoints := map[string]any{
		"bot": config.Entrypoints.Bot,
	}
	if config.Entrypoints.Backtest != "" {
		entrypoints["backtest"] = config.Entrypoints.Backtest
	}

	// Prepare schema (only include backtest if specified)
	schema := map[string]any{
		"bot": botSchema,
	}
	if backtestSchema != nil {
		schema["backtest"] = backtestSchema
	}

	// Prepare config payload
	configPayload := map[string]any{
		"name":        config.Name,
		"description": config.Description,
		"runtime":     config.Runtime,
		"version":     config.Version,
		"author":      config.Author,
		"type":        config.Type,
		"entrypoints": entrypoints,
		"schema":      schema,
		"readme":      string(readmeContent),
		"metadata":    config.Metadata,
	}

	configJSON, err := json.Marshal(configPayload)
	if err != nil {
		return fmt.Errorf("failed to marshal config: %v", err)
	}

	// Create deploy request
	deployRequest := CustomBotDeployRequest{
		Config:   string(configJSON),
		FilePath: uploadResponse.FilePath,
	}

	deployRequestJSON, err := json.Marshal(deployRequest)
	if err != nil {
		return fmt.Errorf("failed to marshal deploy request: %v", err)
	}

	// Create HTTP request
	var endpoint string
	var method string

	if isUpdate {
		endpoint = fmt.Sprintf("%s/custom-bots/%s", c.BaseURL, config.Name)
		method = "PUT"
	} else {
		endpoint = fmt.Sprintf("%s/custom-bots/%s", c.BaseURL, config.Name)
		method = "POST"
	}

	req, err := http.NewRequest(method, endpoint, bytes.NewBuffer(deployRequestJSON))
	if err != nil {
		return fmt.Errorf("failed to create request: %v", err)
	}

	req.Header.Set("Authorization", "ApiKey "+auth.APIKey)
	req.Header.Set("Content-Type", "application/json")

	resp, err := c.HTTPClient.Do(req)
	if err != nil {
		return fmt.Errorf("network error: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode == 401 || resp.StatusCode == 403 {
		return fmt.Errorf("authentication failed: API key is invalid or revoked")
	}

	if resp.StatusCode != 200 && resp.StatusCode != 201 {
		// Try to read error message from response
		body, _ := io.ReadAll(resp.Body)
		return fmt.Errorf("deployment failed with status %d: %s", resp.StatusCode, string(body))
	}

	fmt.Println("🎉 Deployment configured successfully!")
	return nil
}

// BotListAPIResponse represents the response structure for bot list API
type BotListAPIResponse struct {
	Success bool          `json:"success"`
	Data    []BotInstance `json:"data"`
	Message string        `json:"message"`
}

// ListBots retrieves the list of deployed bot instances
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

	if resp.StatusCode == 401 || resp.StatusCode == 403 {
		return nil, fmt.Errorf("authentication failed: API key is invalid or revoked")
	}

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, fmt.Errorf("failed to read response body: %v", err)
	}

	if resp.StatusCode != 200 {
		return nil, fmt.Errorf("API error: %d - %s", resp.StatusCode, string(body))
	}

	// Try to parse as wrapped response first
	var apiResponse BotListAPIResponse
	if err := json.Unmarshal(body, &apiResponse); err == nil {
		if !apiResponse.Success {
			return nil, fmt.Errorf("API returned error: %s", apiResponse.Message)
		}
		return apiResponse.Data, nil
	}

	// Fallback: try to parse as direct array (for backward compatibility)
	var bots []BotInstance
	if err := json.Unmarshal(body, &bots); err != nil {
		return nil, fmt.Errorf("failed to parse API response: %v", err)
	}

	return bots, nil
}

// DeployBotInstance deploys a new bot instance
func (c *APIClient) DeployBotInstance(auth *Auth, request *BotDeployRequest) (*BotInstance, error) {
	reqBody, err := json.Marshal(request)
	if err != nil {
		return nil, fmt.Errorf("failed to marshal request: %v", err)
	}

	req, err := http.NewRequest("POST", c.BaseURL+"/bot", bytes.NewBuffer(reqBody))
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

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, fmt.Errorf("failed to read response body: %v", err)
	}

	if resp.StatusCode != 200 && resp.StatusCode != 201 {
		return nil, fmt.Errorf("deployment failed with status %d: %s", resp.StatusCode, string(body))
	}

	var botInstance BotInstance
	if err := json.Unmarshal(body, &botInstance); err != nil {
		return nil, fmt.Errorf("failed to parse API response: %v", err)
	}

	return &botInstance, nil
}

// UpdateBotInstance updates an existing bot instance
func (c *APIClient) UpdateBotInstance(auth *Auth, botID string, request *BotUpdateRequest) error {
	reqBody, err := json.Marshal(request)
	if err != nil {
		return fmt.Errorf("failed to marshal request: %v", err)
	}

	req, err := http.NewRequest("PUT", fmt.Sprintf("%s/bot/%s", c.BaseURL, botID), bytes.NewBuffer(reqBody))
	if err != nil {
		return fmt.Errorf("failed to create request: %v", err)
	}

	req.Header.Set("Authorization", "ApiKey "+auth.APIKey)
	req.Header.Set("Content-Type", "application/json")

	resp, err := c.HTTPClient.Do(req)
	if err != nil {
		return fmt.Errorf("network error: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode == 401 || resp.StatusCode == 403 {
		return fmt.Errorf("authentication failed: API key is invalid or revoked")
	}

	if resp.StatusCode == 404 {
		return fmt.Errorf("bot not found: %s", botID)
	}

	if resp.StatusCode != 200 {
		body, _ := io.ReadAll(resp.Body)
		return fmt.Errorf("update failed with status %d: %s", resp.StatusCode, string(body))
	}

	return nil
}

// UserBot represents a user-installed bot instance
type UserBot struct {
	ID            string         `json:"id"`
	CustomBotName string         `json:"customBotName"`
	CustomBot     *CustomBotData `json:"customBot,omitempty"`
	AcquiredAt    string `json:"acquiredAt"`
}

// UserBotListResponse represents the response for listing user bots
type UserBotListResponse struct {
	Success bool      `json:"success"`
	Data    []UserBot `json:"data"`
	Message string    `json:"message"`
}

// CustomBotListResponse represents the response for listing custom bots
type CustomBotListResponse struct {
	Success bool            `json:"success"`
	Data    []CustomBotData `json:"data"`
	Message string          `json:"message"`
}

// DeleteBotInstance deletes a bot instance
func (c *APIClient) DeleteBotInstance(auth *Auth, botID string) error {
	req, err := http.NewRequest("DELETE", fmt.Sprintf("%s/bot/%s", c.BaseURL, botID), nil)
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
		return fmt.Errorf("bot not found: %s", botID)
	}

	if resp.StatusCode != 200 && resp.StatusCode != 204 {
		body, _ := io.ReadAll(resp.Body)
		return fmt.Errorf("deletion failed with status %d: %s", resp.StatusCode, string(body))
	}

	return nil
}

// FindBotByNameOrID finds a bot by either name or ID
func (c *APIClient) FindBotByNameOrID(auth *Auth, identifier string) (*BotInstance, error) {
	bots, err := c.ListBots(auth)
	if err != nil {
		return nil, err
	}

	for _, bot := range bots {
		if bot.ID == identifier || bot.Name == identifier {
			return &bot, nil
		}
	}

	return nil, fmt.Errorf("bot not found: %s", identifier)
}

// ListUserBots retrieves the list of user-installed bots
func (c *APIClient) ListUserBots(auth *Auth) ([]UserBot, error) {
	req, err := http.NewRequest("GET", c.BaseURL+"/user-bots", nil)
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

	if resp.StatusCode != 200 {
		return nil, fmt.Errorf("API error: %d - %s", resp.StatusCode, string(body))
	}

	var apiResponse UserBotListResponse
	if err := json.Unmarshal(body, &apiResponse); err != nil {
		return nil, fmt.Errorf("failed to parse API response: %v", err)
	}

	if !apiResponse.Success {
		return nil, fmt.Errorf("API returned error: %s", apiResponse.Message)
	}

	return apiResponse.Data, nil
}

// ListCustomBots retrieves the list of user's custom bots
func (c *APIClient) ListCustomBots(auth *Auth) ([]CustomBotData, error) {
	req, err := http.NewRequest("GET", c.BaseURL+"/custom-bots", nil)
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

	if resp.StatusCode != 200 {
		return nil, fmt.Errorf("API error: %d - %s", resp.StatusCode, string(body))
	}

	var apiResponse CustomBotListResponse
	if err := json.Unmarshal(body, &apiResponse); err != nil {
		return nil, fmt.Errorf("failed to parse API response: %v", err)
	}

	if !apiResponse.Success {
		return nil, fmt.Errorf("API returned error: %s", apiResponse.Message)
	}

	return apiResponse.Data, nil
}

// GetCustomBot retrieves a specific custom bot by name
func (c *APIClient) GetCustomBot(auth *Auth, customBotName string) (*CustomBotData, error) {
	req, err := http.NewRequest("GET", fmt.Sprintf("%s/custom-bots/%s", c.BaseURL, customBotName), nil)
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

	if resp.StatusCode == 404 {
		return nil, fmt.Errorf("custom bot not found: %s", customBotName)
	}

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, fmt.Errorf("failed to read response body: %v", err)
	}

	if resp.StatusCode != 200 {
		return nil, fmt.Errorf("API error: %d - %s", resp.StatusCode, string(body))
	}

	var apiResponse CustomBotAPIResponse
	if err := json.Unmarshal(body, &apiResponse); err != nil {
		return nil, fmt.Errorf("failed to parse API response: %v", err)
	}

	if !apiResponse.Success {
		return nil, fmt.Errorf("API returned error: %s", apiResponse.Message)
	}

	return &apiResponse.Data, nil
}

// GetBotLogs retrieves logs for a specific bot
func (c *APIClient) GetBotLogs(auth *Auth, botID string, params *LogsParams) ([]LogEntry, error) {
	// Build URL with query parameters
	url := fmt.Sprintf("%s/logs/%s", c.BaseURL, botID)
	req, err := http.NewRequest("GET", url, nil)
	if err != nil {
		return nil, fmt.Errorf("failed to create request: %v", err)
	}

	// Add query parameters
	if params != nil {
		q := req.URL.Query()
		if params.Date != "" {
			q.Add("date", params.Date)
		}
		if params.DateRange != "" {
			q.Add("dateRange", params.DateRange)
		}
		if params.Limit > 0 {
			q.Add("limit", fmt.Sprintf("%d", params.Limit))
		}
		req.URL.RawQuery = q.Encode()
	}

	req.Header.Set("Authorization", "ApiKey "+auth.APIKey)

	resp, err := c.HTTPClient.Do(req)
	if err != nil {
		return nil, fmt.Errorf("network error: %v", err)
	}
	defer resp.Body.Close()
	if err != nil {
		return nil, fmt.Errorf("network error: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode == 401 || resp.StatusCode == 403 {
		return nil, fmt.Errorf("authentication failed: API key is invalid or revoked")
	}

	if resp.StatusCode == 404 {
		return nil, fmt.Errorf("bot not found: %s", botID)
	}

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, fmt.Errorf("failed to read response body: %v", err)
	}

	if resp.StatusCode != 200 {
		return nil, fmt.Errorf("API error: %d - %s", resp.StatusCode, string(body))
	}

	var apiResponse LogsResponse
	if err := json.Unmarshal(body, &apiResponse); err != nil {
		return nil, fmt.Errorf("failed to parse API response: %v", err)
	}

	if !apiResponse.Success {
		return nil, fmt.Errorf("API returned error: %s", apiResponse.Message)
	}

	return apiResponse.Data, nil
}

// RequestUploadURL requests a signed upload URL for deploying a bot
func (c *APIClient) RequestUploadURL(botName string, version string, auth *Auth) (*UploadUrlResponse, error) {
	reqBody := UploadUrlRequest{
		Version: version,
	}

	reqBodyJSON, err := json.Marshal(reqBody)
	if err != nil {
		return nil, fmt.Errorf("failed to marshal request: %v", err)
	}

	req, err := http.NewRequest("POST", fmt.Sprintf("%s/custom-bots/%s/upload-url", c.BaseURL, botName), bytes.NewBuffer(reqBodyJSON))
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

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, fmt.Errorf("failed to read response body: %v", err)
	}

	if resp.StatusCode != 200 {
		return nil, fmt.Errorf("failed to get upload URL: HTTP %d - %s", resp.StatusCode, string(body))
	}

	var uploadResponse UploadUrlResponse
	if err := json.Unmarshal(body, &uploadResponse); err != nil {
		return nil, fmt.Errorf("failed to parse upload URL response: %v", err)
	}

	return &uploadResponse, nil
}

// UploadToStorage uploads a file to Firebase Storage using a signed URL
func (c *APIClient) UploadToStorage(filePath string, uploadURL string) error {
	// Read the file content into memory
	fileContent, err := os.ReadFile(filePath)
	if err != nil {
		return fmt.Errorf("failed to read file: %v", err)
	}

	// Create PUT request with the signed URL
	req, err := http.NewRequest("PUT", uploadURL, bytes.NewReader(fileContent))
	if err != nil {
		return fmt.Errorf("failed to create upload request: %v", err)
	}

	// Set required headers for Firebase Storage
	req.Header.Set("Content-Type", "application/zip")
	req.Header.Set("Content-Length", fmt.Sprintf("%d", len(fileContent)))

	// Use a longer timeout for large file uploads
	client := &http.Client{
		Timeout: 30 * time.Minute, // 30 minutes for large files
	}

	resp, err := client.Do(req)
	if err != nil {
		return fmt.Errorf("upload failed: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != 200 {
		body, _ := io.ReadAll(resp.Body)
		return fmt.Errorf("upload failed with status %d: %s", resp.StatusCode, string(body))
	}

	return nil
}
