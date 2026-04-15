package internal

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"time"
)

const (
	envFileName         = "environments.json"
	legacyAuthFileName  = "auth.json"
	defaultEnvName      = "default"
)

type Environment struct {
	URL       string    `json:"url"`
	APIKey    string    `json:"api_key"`
	CreatedAt time.Time `json:"created_at"`
}

type Environments struct {
	Active       string                 `json:"active"`
	Environments map[string]Environment `json:"environments"`
}

var (
	configDirOverride   string
	activeEnvOverride   string
)

// SetConfigDir overrides the config directory (default: ~/.the0). Used by tests
// and can also be used for isolated CLI runs. Pass "" to restore default.
func SetConfigDir(dir string) { configDirOverride = dir }

// SetActiveEnvOverride sets the name of the environment that wins over the
// persisted active selection for the lifetime of this process. Set by the root
// command's --env flag. Empty string clears it.
func SetActiveEnvOverride(name string) { activeEnvOverride = name }

// ConfigDir returns the resolved config directory (override if set, else ~/.the0).
func ConfigDir() (string, error) {
	if configDirOverride != "" {
		return configDirOverride, nil
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return "", err
	}
	return filepath.Join(home, ".the0"), nil
}

func environmentsPath() (string, error) {
	dir, err := ConfigDir()
	if err != nil {
		return "", err
	}
	return filepath.Join(dir, envFileName), nil
}

func legacyAuthPath() (string, error) {
	dir, err := ConfigDir()
	if err != nil {
		return "", err
	}
	return filepath.Join(dir, legacyAuthFileName), nil
}

// LoadEnvironments reads the environments file. Returns an empty, non-nil
// Environments struct if the file does not exist.
func LoadEnvironments() (*Environments, error) {
	path, err := environmentsPath()
	if err != nil {
		return nil, err
	}
	data, err := os.ReadFile(path)
	if err != nil {
		if os.IsNotExist(err) {
			return &Environments{Environments: map[string]Environment{}}, nil
		}
		return nil, err
	}
	var envs Environments
	if err := json.Unmarshal(data, &envs); err != nil {
		return nil, fmt.Errorf("parse %s: %w", path, err)
	}
	if envs.Environments == nil {
		envs.Environments = map[string]Environment{}
	}
	return &envs, nil
}

// SaveEnvironments writes the environments file atomically with 0600 perms.
// The write goes to a same-directory temp file that is fsynced and then
// renamed over the target so a crash mid-write cannot leave a partial file.
func SaveEnvironments(envs *Environments) error {
	dir, err := ConfigDir()
	if err != nil {
		return err
	}
	if err := os.MkdirAll(dir, 0700); err != nil {
		return err
	}
	path := filepath.Join(dir, envFileName)
	data, err := json.MarshalIndent(envs, "", "  ")
	if err != nil {
		return err
	}

	tmp, err := os.CreateTemp(dir, envFileName+".tmp-*")
	if err != nil {
		return err
	}
	tmpPath := tmp.Name()
	cleanup := func() { _ = os.Remove(tmpPath) }
	if err := tmp.Chmod(0600); err != nil {
		_ = tmp.Close()
		cleanup()
		return err
	}
	if _, err := tmp.Write(data); err != nil {
		_ = tmp.Close()
		cleanup()
		return err
	}
	if err := tmp.Sync(); err != nil {
		_ = tmp.Close()
		cleanup()
		return err
	}
	if err := tmp.Close(); err != nil {
		cleanup()
		return err
	}
	if err := os.Rename(tmpPath, path); err != nil {
		cleanup()
		return err
	}
	return nil
}

// EnvironmentsFileExists reports whether the persisted environments file is
// present on disk. Used to distinguish "user has never opted in to named
// environments" (legacy migration applies) from "user has opted in but
// currently has zero environments" (legacy auth must NOT be re-imported).
func EnvironmentsFileExists() bool {
	path, err := environmentsPath()
	if err != nil {
		return false
	}
	_, err = os.Stat(path)
	return err == nil
}

// Add registers a new environment. Errors if name already exists.
func (e *Environments) Add(name string, env Environment) error {
	if e.Environments == nil {
		e.Environments = map[string]Environment{}
	}
	if _, exists := e.Environments[name]; exists {
		return fmt.Errorf("environment %q already exists", name)
	}
	if env.CreatedAt.IsZero() {
		env.CreatedAt = time.Now().UTC()
	}
	e.Environments[name] = env
	return nil
}

// Use marks the named environment as active. Errors if unknown.
func (e *Environments) Use(name string) error {
	if _, ok := e.Environments[name]; !ok {
		return fmt.Errorf("environment %q does not exist", name)
	}
	e.Active = name
	return nil
}

// Remove deletes the named environment. If it was active, clears Active.
// Errors if the env does not exist.
func (e *Environments) Remove(name string) error {
	if _, ok := e.Environments[name]; !ok {
		return fmt.Errorf("environment %q does not exist", name)
	}
	delete(e.Environments, name)
	if e.Active == name {
		e.Active = ""
	}
	return nil
}

// Get returns the named environment, if present.
func (e *Environments) Get(name string) (*Environment, bool) {
	env, ok := e.Environments[name]
	if !ok {
		return nil, false
	}
	return &env, true
}

// ResolveActive returns the environment (URL + API key) to use for API calls,
// applying precedence: explicit override arg > process-level override (--env
// flag) > persisted active env > legacy auth.json + THE0_API_URL > default.
func ResolveActive(override string) (*Environment, error) {
	envs, err := LoadEnvironments()
	if err != nil {
		return nil, err
	}

	name := override
	if name == "" {
		name = activeEnvOverride
	}
	if name != "" {
		env, ok := envs.Get(name)
		if !ok {
			return nil, fmt.Errorf("environment %q is not defined (use `the0 env add %s`)", name, name)
		}
		return env, nil
	}

	if envs.Active != "" {
		env, ok := envs.Get(envs.Active)
		if !ok {
			return nil, fmt.Errorf("active environment %q is not defined; pick one with `the0 env use <name>` or remove the stale entry from %s",
				envs.Active, envFileName)
		}
		return env, nil
	}

	// Legacy fallback: auth.json + THE0_API_URL.
	legacy := &Environment{URL: legacyURL()}
	if auth, err := readLegacyAuth(); err == nil && auth != nil {
		legacy.APIKey = auth.APIKey
		legacy.CreatedAt = auth.CreatedAt
	}
	return legacy, nil
}

func legacyURL() string {
	if url := os.Getenv("THE0_API_URL"); url != "" {
		return url
	}
	return DEFAULT_API_URL
}

func readLegacyAuth() (*Auth, error) {
	path, err := legacyAuthPath()
	if err != nil {
		return nil, err
	}
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	var auth Auth
	if err := json.Unmarshal(data, &auth); err != nil {
		return nil, err
	}
	return &auth, nil
}

// MigrateLegacyAuth creates an environments.json seeded from the legacy
// auth.json + THE0_API_URL/default URL, stored under the name "default" and
// marked active. If no legacy state exists, returns an empty but valid
// Environments (still persisted so the CLI starts treating this user as
// opted-in to named environments).
func MigrateLegacyAuth() (*Environments, error) {
	envs := &Environments{Environments: map[string]Environment{}}

	auth, _ := readLegacyAuth()
	url := legacyURL()

	if auth != nil || os.Getenv("THE0_API_URL") != "" {
		createdAt := time.Now().UTC()
		if auth != nil && !auth.CreatedAt.IsZero() {
			createdAt = auth.CreatedAt.UTC()
		}
		env := Environment{URL: url, CreatedAt: createdAt}
		if auth != nil {
			env.APIKey = auth.APIKey
		}
		envs.Environments[defaultEnvName] = env
		envs.Active = defaultEnvName
	}

	if err := SaveEnvironments(envs); err != nil {
		return nil, err
	}
	return envs, nil
}
