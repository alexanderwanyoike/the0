package internal

import (
	"bufio"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"
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

	fmt.Print("Enter your the0 API key: ")
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
		fmt.Printf("Saved credentials invalid: %v\n", err)
		return PromptForNewAPIKey()
	}

	return auth, nil
}

// IsAuthError checks if an error is authentication related
func IsAuthError(err error) bool {
	return strings.Contains(err.Error(), "401") ||
		strings.Contains(err.Error(), "403") ||
		strings.Contains(err.Error(), "invalid") ||
		strings.Contains(err.Error(), "revoked")
}
