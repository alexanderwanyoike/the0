package internal

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"mime/multipart"
	"net/http"
	"net/url"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/Masterminds/semver/v3"
	"the0/internal/logger"
)

const DEFAULT_API_URL = "http://localhost:3000"

// APIClient handles all API interactions
type APIClient struct {
	BaseURL    string
	HTTPClient *http.Client
}

// Schema represents the JSON schema structure
type Schema struct {
	Bot map[string]any `json:"bot"`
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
	FilePath  string       `json:"FilePath"`
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
	CreatedAt     string             `json:"createdAt"`
	UpdatedAt     string             `json:"updatedAt"`
}

// BotInstance represents a deployed bot instance
type BotInstance struct {
	ID          string         `json:"id"`
	Name        string         `json:"name"`
	Config      map[string]any `json:"config"`
	Topic       string         `json:"topic"`
	CreatedAt   string         `json:"createdAt"`
	UpdatedAt   string         `json:"updatedAt"`
	UserID      string         `json:"userId"`
	CustomBotId string         `json:"customBotId"`
}

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

// GetAPIBaseURL returns the API base URL for the active environment. It
// consults ResolveActive so the --env flag, active named environment, legacy
// THE0_API_URL env var, and default URL all compose through a single path.
func GetAPIBaseURL() string {
	env, err := ResolveActive("")
	if err != nil || env == nil || env.URL == "" {
		return DEFAULT_API_URL
	}
	return env.URL
}

// apiKeyValidation is what the API key validation endpoint reports
type apiKeyValidation struct {
	Valid      bool   `json:"valid"`
	UserID     string `json:"userId"`
	KeyID      string `json:"keyId"`
	KeyName    string `json:"keyName"`
	LastUsedAt string `json:"lastUsedAt"`
}

// LogEntry represents a single log entry
type LogEntry struct {
	Key  string `json:"key"`
	Date string `json:"date"`
	// Empty when the api sends null: the line has no timestamp of its own.
	Timestamp string `json:"timestamp"`
	Content   string `json:"content"`
}

// LogsParams represents parameters for the logs API call
type LogsParams struct {
	Date      string `json:"date,omitempty"`
	DateRange string `json:"dateRange,omitempty"`
	Limit     int    `json:"limit,omitempty"`
	Sort      string `json:"sort,omitempty"`
}

// TestAPIKey tests if an API key is valid
func (c *APIClient) TestAPIKey(auth *Auth) error {
	body, err := c.send(auth, apiRequest{method: "GET", path: "/auth/validate-api-key"})
	if err != nil {
		return err
	}

	env, err := decode[envelope[apiKeyValidation]](body)
	if err != nil {
		return err
	}
	if !env.Success {
		return fmt.Errorf("API key validation failed: %s", env.Message)
	}
	if !env.Data.Valid {
		return fmt.Errorf("API key is invalid")
	}
	return nil
}

// CheckBotExists checks if a bot exists and validates ownership
func (c *APIClient) CheckBotExists(config *BotConfig, auth *Auth) (bool, error) {
	body, err := c.send(auth, apiRequest{method: "GET", path: "/custom-bots/" + config.Name})
	if IsNotFound(err) {
		// Bot doesn't exist, this is a new deployment
		return false, nil
	}
	if err != nil {
		return false, err
	}

	botData, err := unwrap[CustomBotData](body)
	if err != nil {
		return false, err
	}

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
		return false, fmt.Errorf("new version (%s) must be greater than current version (%s)", config.Version, botData.LatestVersion)
	}

	logger.Verbose("Bot %s ready for update: %s -> %s", config.Name, botData.LatestVersion, config.Version)

	return true, nil
}

// DeployBot deploys a bot using direct upload: upload file and deploy in one step
func (c *APIClient) DeployBot(config *BotConfig, auth *Auth, zipPath string, isUpdate bool) error {
	logger.StartSpinner("Starting deployment")

	// Step 1: Upload ZIP file directly to API
	logger.UpdateSpinner("Uploading " + filepath.Base(zipPath))
	filePath, err := c.UploadFileDirect(config.Name, config.Version, zipPath, auth)
	if err != nil {
		logger.StopSpinner()
		return fmt.Errorf("failed to upload file: %v", err)
	}
	logger.StopSpinnerWithSuccess("Upload complete ⚡")

	// Step 2: Deploy with config and file path
	logger.StartSpinner("Configuring deployment")

	// Read README content
	readmeContent, err := os.ReadFile(config.Readme)
	if err != nil {
		return fmt.Errorf("failed to read README: %v", err)
	}

	// Read and parse bot schema
	botSchemaData, err := os.ReadFile(config.Schema.Bot)
	if err != nil {
		return fmt.Errorf("failed to read bot schema: %v", err)
	}

	var botSchema map[string]any
	if err := json.Unmarshal(botSchemaData, &botSchema); err != nil {
		return fmt.Errorf("invalid JSON in bot schema: %v", err)
	}

	// Prepare entrypoints
	entrypoints := map[string]any{
		"bot": config.Entrypoints.Bot,
	}
	if config.Entrypoints.Query != "" {
		entrypoints["query"] = config.Entrypoints.Query
	}

	// Prepare schema
	schema := map[string]any{
		"bot": botSchema,
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

	deployRequest := CustomBotDeployRequest{
		Config:   string(configJSON),
		FilePath: filePath,
	}

	method := "POST"
	if isUpdate {
		method = "PUT"
	}

	if _, err := c.sendJSON(auth, method, "/custom-bots/"+config.Name, deployRequest); err != nil {
		logger.StopSpinner()
		return err
	}

	logger.StopSpinnerWithSuccess("Deployment successful")
	return nil
}

// ListBots retrieves the list of deployed bot instances
func (c *APIClient) ListBots(auth *Auth) ([]BotInstance, error) {
	body, err := c.send(auth, apiRequest{method: "GET", path: "/bot"})
	if err != nil {
		return nil, err
	}

	// The api answers with a bare array; older ones wrapped it in an envelope.
	var env envelope[[]BotInstance]
	if err := json.Unmarshal(body, &env); err == nil {
		if !env.Success {
			return nil, fmt.Errorf("API returned error: %s", env.Message)
		}
		return env.Data, nil
	}
	return decode[[]BotInstance](body)
}

// DeployBotInstance deploys a new bot instance
func (c *APIClient) DeployBotInstance(auth *Auth, request *BotDeployRequest) (*BotInstance, error) {
	body, err := c.sendJSON(auth, "POST", "/bot", request)
	if err != nil {
		return nil, err
	}

	bot, err := decode[BotInstance](body)
	if err != nil {
		return nil, err
	}
	return &bot, nil
}

// UpdateBotInstance updates an existing bot instance
func (c *APIClient) UpdateBotInstance(auth *Auth, botID string, request *BotUpdateRequest) error {
	_, err := c.sendJSON(auth, "PUT", "/bot/"+botID, request)
	return err
}

// UserBot represents a user-installed bot instance
type UserBot struct {
	ID            string         `json:"id"`
	CustomBotName string         `json:"customBotName"`
	CustomBot     *CustomBotData `json:"customBot,omitempty"`
	AcquiredAt    string         `json:"acquiredAt"`
}

// VersionWithInstances represents a version with its instance count
type VersionWithInstances struct {
	CustomBotVersion
	InstanceCount int `json:"instanceCount"`
}

// BotDeleteResponse represents the response from deleting a bot instance
type BotDeleteResponse struct {
	OrphanedVersion *OrphanedVersionInfo `json:"orphanedVersion,omitempty"`
}

// OrphanedVersionInfo contains info about an orphaned custom bot version
type OrphanedVersionInfo struct {
	Name        string `json:"name"`
	Version     string `json:"version"`
	CustomBotId string `json:"customBotId"`
}

// DeleteBotInstance deletes a bot instance and returns orphan info if applicable
func (c *APIClient) DeleteBotInstance(auth *Auth, botID string) (*BotDeleteResponse, error) {
	body, err := c.send(auth, apiRequest{method: "DELETE", path: "/bot/" + botID})
	if err != nil {
		return nil, err
	}

	// The orphan info is a hint; a body without it still means the bot is gone.
	var deleteResponse BotDeleteResponse
	if len(body) == 0 || json.Unmarshal(body, &deleteResponse) != nil {
		return &BotDeleteResponse{}, nil
	}
	return &deleteResponse, nil
}

// GetCustomBotVersionsWithInstances retrieves versions with instance counts
func (c *APIClient) GetCustomBotVersionsWithInstances(auth *Auth, name string) ([]VersionWithInstances, error) {
	body, err := c.send(auth, apiRequest{method: "GET", path: "/custom-bots/" + name + "/versions"})
	if err != nil {
		return nil, err
	}
	return unwrap[[]VersionWithInstances](body)
}

// DeleteCustomBotVersion deletes a specific version of a custom bot
func (c *APIClient) DeleteCustomBotVersion(auth *Auth, name string, version string) error {
	_, err := c.send(auth, apiRequest{method: "DELETE", path: "/custom-bots/" + name + "/" + version})
	return err
}

// DeleteCustomBot deletes all versions of a custom bot
func (c *APIClient) DeleteCustomBot(auth *Auth, name string) error {
	_, err := c.send(auth, apiRequest{method: "DELETE", path: "/custom-bots/" + name})
	return err
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
	body, err := c.send(auth, apiRequest{method: "GET", path: "/user-bots"})
	if err != nil {
		return nil, err
	}
	return unwrap[[]UserBot](body)
}

// ListCustomBots retrieves the list of user's custom bots
func (c *APIClient) ListCustomBots(auth *Auth) ([]CustomBotData, error) {
	body, err := c.send(auth, apiRequest{method: "GET", path: "/custom-bots"})
	if err != nil {
		return nil, err
	}
	return unwrap[[]CustomBotData](body)
}

// GetCustomBot retrieves a specific custom bot by name
func (c *APIClient) GetCustomBot(auth *Auth, customBotName string) (*CustomBotData, error) {
	body, err := c.send(auth, apiRequest{method: "GET", path: "/custom-bots/" + customBotName})
	if err != nil {
		return nil, err
	}

	customBot, err := unwrap[CustomBotData](body)
	if err != nil {
		return nil, err
	}
	return &customBot, nil
}

// GetBotLogs retrieves logs for a specific bot
func (c *APIClient) GetBotLogs(auth *Auth, botID string, params *LogsParams) ([]LogEntry, error) {
	query := url.Values{}
	if params != nil {
		if params.Date != "" {
			query.Add("date", params.Date)
		}
		if params.DateRange != "" {
			query.Add("dateRange", params.DateRange)
		}
		if params.Limit > 0 {
			query.Add("limit", fmt.Sprintf("%d", params.Limit))
		}
		if params.Sort != "" {
			query.Add("sort", params.Sort)
		}
	}

	body, err := c.send(auth, apiRequest{method: "GET", path: "/logs/" + botID, query: query})
	if err != nil {
		return nil, err
	}
	return unwrap[[]LogEntry](body)
}

// UploadFileDirect uploads a file directly to the API using multipart form data
func (c *APIClient) UploadFileDirect(botName string, version string, filePath string, auth *Auth) (string, error) {
	file, err := os.Open(filePath)
	if err != nil {
		return "", fmt.Errorf("failed to open file: %v", err)
	}
	defer file.Close()

	var requestBody bytes.Buffer
	writer := multipart.NewWriter(&requestBody)

	if err := writer.WriteField("version", version); err != nil {
		return "", fmt.Errorf("failed to write version field: %v", err)
	}

	part, err := writer.CreateFormFile("file", filepath.Base(filePath))
	if err != nil {
		return "", fmt.Errorf("failed to create form file: %v", err)
	}

	if _, err := io.Copy(part, file); err != nil {
		return "", fmt.Errorf("failed to copy file content: %v", err)
	}

	if err := writer.Close(); err != nil {
		return "", fmt.Errorf("failed to close writer: %v", err)
	}

	body, err := c.send(auth, apiRequest{
		method:      "POST",
		path:        "/custom-bots/" + botName + "/upload",
		body:        requestBody.Bytes(),
		contentType: writer.FormDataContentType(),
		timeout:     30 * time.Minute,
	})
	if err != nil {
		return "", err
	}

	// The upload route answers flat, not in an envelope.
	var uploadResponse struct {
		Success  bool   `json:"success"`
		FilePath string `json:"filePath"`
		Message  string `json:"message"`
	}
	if err := json.Unmarshal(body, &uploadResponse); err != nil {
		return "", fmt.Errorf("failed to parse upload response: %v", err)
	}

	if !uploadResponse.Success {
		return "", fmt.Errorf("upload failed: %s", uploadResponse.Message)
	}

	if uploadResponse.FilePath == "" {
		return "", fmt.Errorf("file path not found in response")
	}

	return uploadResponse.FilePath, nil
}

// BotStateKey represents a single state key with metadata
type BotStateKey struct {
	Key  string `json:"key"`
	Size int64  `json:"size"`
}

// ListBotState lists all state keys for a bot
func (c *APIClient) ListBotState(auth *Auth, botID string) ([]BotStateKey, error) {
	body, err := c.send(auth, apiRequest{method: "GET", path: "/bots/" + botID + "/state"})
	if err != nil {
		return nil, err
	}
	return unwrap[[]BotStateKey](body)
}

// GetBotStateKey gets a specific state value for a bot
func (c *APIClient) GetBotStateKey(auth *Auth, botID string, key string) (any, error) {
	body, err := c.send(auth, apiRequest{method: "GET", path: "/bots/" + botID + "/state/" + key})
	if err != nil {
		return nil, err
	}
	return unwrap[any](body)
}

// DeleteBotStateKey deletes a specific state key for a bot
func (c *APIClient) DeleteBotStateKey(auth *Auth, botID string, key string) (bool, error) {
	body, err := c.send(auth, apiRequest{method: "DELETE", path: "/bots/" + botID + "/state/" + key})
	if err != nil {
		return false, err
	}

	data, err := unwrap[struct {
		Deleted bool `json:"deleted"`
	}](body)
	if err != nil {
		return false, err
	}
	return data.Deleted, nil
}

// ClearBotState clears all state for a bot
func (c *APIClient) ClearBotState(auth *Auth, botID string) (bool, error) {
	body, err := c.send(auth, apiRequest{method: "DELETE", path: "/bots/" + botID + "/state"})
	if err != nil {
		return false, err
	}

	data, err := unwrap[struct {
		Cleared bool `json:"cleared"`
	}](body)
	if err != nil {
		return false, err
	}
	return data.Cleared, nil
}

// BotQueryRequest represents a request to execute a query against a bot
type BotQueryRequest struct {
	QueryPath  string         `json:"query_path"`
	Params     map[string]any `json:"params,omitempty"`
	TimeoutSec int            `json:"timeout_sec,omitempty"`
}

// BotQueryResponse represents the response from a bot query
type BotQueryResponse struct {
	Success bool   `json:"success"`
	Data    any    `json:"data,omitempty"`
	Message string `json:"message,omitempty"`
}

// ExecuteBotQuery executes a query against a bot's query handlers
func (c *APIClient) ExecuteBotQuery(auth *Auth, botID string, request *BotQueryRequest) (*BotQueryResponse, error) {
	reqBody, err := json.Marshal(request)
	if err != nil {
		return nil, fmt.Errorf("failed to marshal request: %v", err)
	}

	body, err := c.send(auth, apiRequest{
		method:      "POST",
		path:        "/query/" + botID,
		body:        reqBody,
		contentType: "application/json",
		// The bot gets the whole timeout; the extra covers the round trip.
		timeout: time.Duration(request.TimeoutSec+10) * time.Second,
	})
	if err != nil {
		var apiErr *APIError
		if errors.As(err, &apiErr) {
			if nested := nestedQueryError(apiErr.Message); nested != "" {
				apiErr.Message = nested
			}
		}
		return nil, err
	}

	response, err := decode[BotQueryResponse](body)
	if err != nil {
		return nil, err
	}
	return &response, nil
}

// nestedQueryError digs the bot's own error out of the api's
// "Query failed: {json}" message, which wraps the runtime's response.
func nestedQueryError(message string) string {
	idx := strings.Index(message, "{")
	if idx < 0 {
		return ""
	}
	jsonPart := message[idx:]
	endIdx := strings.LastIndex(jsonPart, "}")
	if endIdx < 0 {
		return ""
	}
	var nested struct {
		Error string `json:"error"`
	}
	if err := json.Unmarshal([]byte(jsonPart[:endIdx+1]), &nested); err != nil {
		return ""
	}
	return nested.Error
}
