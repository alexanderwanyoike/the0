package internal

import (
	"encoding/json"
	"os"
	"path/filepath"
)

const SECRETS_FILE = ".the0/secrets.json"

// BuildSecrets represents build-time secrets for vendoring
type BuildSecrets struct {
	// Universal
	GitHubToken    string `json:"github_token,omitempty"`    // GITHUB_TOKEN - for git-based deps (all languages)
	GitHubUsername string `json:"github_username,omitempty"` // Deprecated

	// Python
	PipIndexURL string `json:"pip_index_url,omitempty"` // PIP_EXTRA_INDEX_URL

	// Node.js
	NpmToken string `json:"npm_token,omitempty"` // NPM_TOKEN - for private npm registries

	// C#/.NET
	NuGetToken string `json:"nuget_token,omitempty"` // NUGET_TOKEN - for private NuGet feeds

	// Rust
	CargoRegistryToken string `json:"cargo_registry_token,omitempty"` // CARGO_REGISTRY_TOKEN

	// Scala/Java
	MavenUser  string `json:"maven_user,omitempty"`  // MAVEN_USER
	MavenToken string `json:"maven_token,omitempty"` // MAVEN_TOKEN

	// Legacy (kept for backwards compatibility)
	NuGetAPIKey string `json:"nuget_api_key,omitempty"` // Deprecated, use nuget_token
}

// GetGitHubUsername returns the GitHub username, defaulting to "user" if not set
func (s *BuildSecrets) GetGitHubUsername() string {
	if s.GitHubUsername != "" {
		return s.GitHubUsername
	}
	return "user"
}

// LoadBuildSecrets loads build secrets from file
func LoadBuildSecrets() (*BuildSecrets, error) {
	homeDir, err := os.UserHomeDir()
	if err != nil {
		return nil, err
	}

	secretsPath := filepath.Join(homeDir, SECRETS_FILE)
	data, err := os.ReadFile(secretsPath)
	if err != nil {
		if os.IsNotExist(err) {
			// Return empty secrets if file doesn't exist
			return &BuildSecrets{}, nil
		}
		return nil, err
	}

	var secrets BuildSecrets
	if err := json.Unmarshal(data, &secrets); err != nil {
		return nil, err
	}

	return &secrets, nil
}

// SaveBuildSecrets saves build secrets to file
func SaveBuildSecrets(secrets *BuildSecrets) error {
	homeDir, err := os.UserHomeDir()
	if err != nil {
		return err
	}

	secretsDir := filepath.Join(homeDir, ".the0")
	if err := os.MkdirAll(secretsDir, 0700); err != nil {
		return err
	}

	secretsPath := filepath.Join(homeDir, SECRETS_FILE)
	data, err := json.MarshalIndent(secrets, "", "  ")
	if err != nil {
		return err
	}

	return os.WriteFile(secretsPath, data, 0600)
}

// ClearBuildSecrets removes the saved build secrets
func ClearBuildSecrets() error {
	homeDir, err := os.UserHomeDir()
	if err != nil {
		return err
	}

	secretsPath := filepath.Join(homeDir, SECRETS_FILE)
	err = os.Remove(secretsPath)
	if os.IsNotExist(err) {
		return nil // Already cleared
	}
	return err
}

// MaskToken masks a token for display, showing only first 4 and last 4 characters
func MaskToken(token string) string {
	if len(token) <= 8 {
		return "****"
	}
	return token[:4] + "..." + token[len(token)-4:]
}
