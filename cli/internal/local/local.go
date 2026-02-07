package local

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"

	"the0/internal/logger"
)

const (
	composeDirName = ".the0/compose"
	stateFileName  = "local-state.json"
)

// LocalState persists configuration between CLI invocations
type LocalState struct {
	RepoPath string    `json:"repo_path"`
	Prebuilt bool      `json:"prebuilt"`
	InitAt   time.Time `json:"init_at"`
}

// ComposeDir returns the path to ~/.the0/compose/
func ComposeDir() (string, error) {
	home, err := os.UserHomeDir()
	if err != nil {
		return "", fmt.Errorf("failed to get home directory: %w", err)
	}
	return filepath.Join(home, composeDirName), nil
}

// statePath returns the full path to the state file
func statePath() (string, error) {
	dir, err := ComposeDir()
	if err != nil {
		return "", err
	}
	return filepath.Join(dir, stateFileName), nil
}

// SaveState persists LocalState to disk
func SaveState(state *LocalState) error {
	sp, err := statePath()
	if err != nil {
		return err
	}

	data, err := json.MarshalIndent(state, "", "  ")
	if err != nil {
		return fmt.Errorf("failed to marshal state: %w", err)
	}

	return os.WriteFile(sp, data, 0600)
}

// LoadState reads LocalState from disk
func LoadState() (*LocalState, error) {
	sp, err := statePath()
	if err != nil {
		return nil, err
	}

	data, err := os.ReadFile(sp)
	if err != nil {
		return nil, fmt.Errorf("failed to read state file: %w", err)
	}

	var state LocalState
	if err := json.Unmarshal(data, &state); err != nil {
		return nil, fmt.Errorf("failed to parse state file: %w", err)
	}

	return &state, nil
}

// EnsureInitialized checks that the local environment has been initialized.
// Returns the loaded state or an error if `the0 local init` hasn't been run.
func EnsureInitialized() (*LocalState, error) {
	state, err := LoadState()
	if err != nil {
		return nil, fmt.Errorf("local environment not initialized. Run 'the0 local init' first")
	}
	return state, nil
}

// ExtractComposeFiles writes the embedded compose files to ~/.the0/compose/
// with {{REPO_PATH}} replaced by the absolute repo path.
func ExtractComposeFiles(repoPath string, prebuilt bool) error {
	absPath, err := filepath.Abs(repoPath)
	if err != nil {
		return fmt.Errorf("failed to resolve repo path: %w", err)
	}

	dir, err := ComposeDir()
	if err != nil {
		return err
	}

	if err := os.MkdirAll(dir, 0755); err != nil {
		return fmt.Errorf("failed to create compose directory: %w", err)
	}

	// Replace placeholder in main compose file
	composeContent := strings.ReplaceAll(string(embeddedComposeFile), "{{REPO_PATH}}", absPath)
	composePath := filepath.Join(dir, "docker-compose.yml")
	if err := os.WriteFile(composePath, []byte(composeContent), 0644); err != nil {
		return fmt.Errorf("failed to write docker-compose.yml: %w", err)
	}
	logger.Verbose("Wrote %s", composePath)

	// Replace placeholder in dev compose file
	devContent := strings.ReplaceAll(string(embeddedComposeDevFile), "{{REPO_PATH}}", absPath)
	devPath := filepath.Join(dir, "docker-compose.dev.yml")
	if err := os.WriteFile(devPath, []byte(devContent), 0644); err != nil {
		return fmt.Errorf("failed to write docker-compose.dev.yml: %w", err)
	}
	logger.Verbose("Wrote %s", devPath)

	// Save state
	state := &LocalState{
		RepoPath: absPath,
		Prebuilt: prebuilt,
		InitAt:   time.Now(),
	}
	if err := SaveState(state); err != nil {
		return fmt.Errorf("failed to save state: %w", err)
	}

	return nil
}

// CleanupComposeDir removes the entire ~/.the0/compose/ directory
func CleanupComposeDir() error {
	dir, err := ComposeDir()
	if err != nil {
		return err
	}
	return os.RemoveAll(dir)
}
