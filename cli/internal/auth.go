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

// Auth represents the authentication structure
type Auth struct {
	APIKey    string    `json:"api_key"`
	CreatedAt time.Time `json:"created_at"`
}

func authFilePath() (string, error) {
	dir, err := ConfigDir()
	if err != nil {
		return "", err
	}
	return filepath.Join(dir, "auth.json"), nil
}

// LoadAuth returns the API key for the active environment (named environment
// if one is active, otherwise the legacy auth.json). Returns an error if no
// API key can be resolved.
func LoadAuth() (*Auth, error) {
	env, err := ResolveActive("")
	if err != nil {
		return nil, err
	}
	if env == nil || env.APIKey == "" {
		return nil, fmt.Errorf("no API key configured (run `the0 auth login` or `the0 env add`)")
	}
	return &Auth{APIKey: env.APIKey, CreatedAt: env.CreatedAt}, nil
}

// SaveAuth persists the API key. If a named environment is active, the key is
// stored on that environment. Otherwise it is written to the legacy
// ~/.the0/auth.json so brand-new users don't need to set up environments
// before their first login.
func SaveAuth(auth *Auth) error {
	envs, err := LoadEnvironments()
	if err != nil {
		return err
	}
	if envs.Active != "" {
		if active, ok := envs.Environments[envs.Active]; ok {
			active.APIKey = auth.APIKey
			if auth.CreatedAt.IsZero() {
				active.CreatedAt = time.Now().UTC()
			} else {
				active.CreatedAt = auth.CreatedAt
			}
			envs.Environments[envs.Active] = active
			return SaveEnvironments(envs)
		}
	}

	// Legacy fallback.
	dir, err := ConfigDir()
	if err != nil {
		return err
	}
	if err := os.MkdirAll(dir, 0700); err != nil {
		return err
	}
	path, err := authFilePath()
	if err != nil {
		return err
	}
	data, err := json.MarshalIndent(auth, "", "  ")
	if err != nil {
		return err
	}
	return os.WriteFile(path, data, 0600)
}

// RemoveAuth clears the API key. If a named environment is active, the key on
// that environment is cleared (the environment itself is retained). Otherwise
// the legacy auth.json file is removed.
func RemoveAuth() error {
	envs, err := LoadEnvironments()
	if err != nil {
		return err
	}
	if envs.Active != "" {
		if active, ok := envs.Environments[envs.Active]; ok && active.APIKey != "" {
			active.APIKey = ""
			envs.Environments[envs.Active] = active
			return SaveEnvironments(envs)
		}
	}

	path, err := authFilePath()
	if err != nil {
		return err
	}
	return os.Remove(path)
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
