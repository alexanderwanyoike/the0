package internal

import (
	"bufio"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"

	"the0/internal/logger"
)

const AUTH_FILE = ".the0/auth.json"

// Auth represents the authentication structure
type Auth struct {
	APIKey    string    `json:"api_key"`
	CreatedAt time.Time `json:"created_at"`
}

// LoadAuth loads authentication from file
func LoadAuth() (*Auth, error) {
	homeDir, err := os.UserHomeDir()
	if err != nil {
		return nil, err
	}

	authPath := filepath.Join(homeDir, AUTH_FILE)
	data, err := os.ReadFile(authPath)
	if err != nil {
		return nil, err
	}

	var auth Auth
	if err := json.Unmarshal(data, &auth); err != nil {
		return nil, err
	}

	return &auth, nil
}

// SaveAuth saves authentication to file
func SaveAuth(auth *Auth) error {
	homeDir, err := os.UserHomeDir()
	if err != nil {
		return err
	}

	authDir := filepath.Join(homeDir, ".the0")
	if err := os.MkdirAll(authDir, 0700); err != nil {
		return err
	}

	authPath := filepath.Join(homeDir, AUTH_FILE)
	data, err := json.MarshalIndent(auth, "", "  ")
	if err != nil {
		return err
	}

	return os.WriteFile(authPath, data, 0600)
}

// RemoveAuth removes the saved authentication
func RemoveAuth() error {
	homeDir, err := os.UserHomeDir()
	if err != nil {
		return err
	}

	authPath := filepath.Join(homeDir, AUTH_FILE)
	return os.Remove(authPath)
}

// PromptForNewAPIKey prompts the user for a new API key
func PromptForNewAPIKey() (*Auth, error) {
	reader := bufio.NewReader(os.Stdin)

	logger.Printf("Enter your the0 API key: ")
	apiKey, err := reader.ReadString('\n')
	if err != nil {
		return nil, err
	}
	apiKey = strings.TrimSpace(apiKey)

	if apiKey == "" {
		return nil, fmt.Errorf("API key cannot be empty")
	}

	auth := &Auth{
		APIKey:    apiKey,
		CreatedAt: time.Now(),
	}

	// Save the auth
	if err := SaveAuth(auth); err != nil {
		return nil, fmt.Errorf("failed to save API key: %v", err)
	}

	return auth, nil
}

// GetAuthTokenWithRetry gets auth token with retry logic
func GetAuthTokenWithRetry() (*Auth, error) {
	auth, err := LoadAuth()
	if err != nil {
		// No saved auth, prompt for API key
		return PromptForNewAPIKey()
	}

	// Test the existing key
	apiClient := NewAPIClient(GetAPIBaseURL())
	if err := apiClient.TestAPIKey(auth); err != nil {
		logger.Warning("Saved credentials invalid: %v", err)
		return PromptForNewAPIKey()
	}

	return auth, nil
}

// IsAuthError checks if an error is authentication related by looking for
// an APIError with a 401 or 403 status code
func IsAuthError(err error) bool {
	var apiErr *APIError
	if errors.As(err, &apiErr) {
		return apiErr.StatusCode == 401 || apiErr.StatusCode == 403
	}
	return false
}
