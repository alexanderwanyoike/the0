package internal

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"the0/cmd"
	"the0/internal/local"
)

// ---- ResolveFriendlyName ----

func TestResolveFriendlyName_ValidFriendlyNames(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{"api", "the0-api"},
		{"frontend", "the0-frontend"},
		{"docs", "the0-docs"},
		{"postgres", "postgres"},
		{"mongo", "mongo"},
		{"nats", "nats"},
		{"minio", "minio"},
		{"runner", "bot-runner"},
		{"scheduler", "bot-scheduler"},
		{"query", "query-server"},
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			result, err := local.ResolveFriendlyName(tt.input)
			if err != nil {
				t.Fatalf("Unexpected error: %v", err)
			}
			if result != tt.expected {
				t.Errorf("Expected %q, got %q", tt.expected, result)
			}
		})
	}
}

func TestResolveFriendlyName_ComposeServiceNames(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{"the0-api", "the0-api"},
		{"the0-frontend", "the0-frontend"},
		{"the0-docs", "the0-docs"},
		{"bot-runner", "bot-runner"},
		{"bot-scheduler", "bot-scheduler"},
		{"query-server", "query-server"},
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			result, err := local.ResolveFriendlyName(tt.input)
			if err != nil {
				t.Fatalf("Unexpected error: %v", err)
			}
			if result != tt.expected {
				t.Errorf("Expected %q, got %q", tt.expected, result)
			}
		})
	}
}

func TestResolveFriendlyName_CaseInsensitive(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{"API", "the0-api"},
		{"Frontend", "the0-frontend"},
		{"POSTGRES", "postgres"},
		{"  api  ", "the0-api"},
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			result, err := local.ResolveFriendlyName(tt.input)
			if err != nil {
				t.Fatalf("Unexpected error: %v", err)
			}
			if result != tt.expected {
				t.Errorf("Expected %q, got %q", tt.expected, result)
			}
		})
	}
}

func TestResolveFriendlyName_InvalidNames(t *testing.T) {
	invalidNames := []string{
		"unknown",
		"redis",
		"",
		"the0-nonexistent",
		"database",
	}

	for _, name := range invalidNames {
		t.Run(name, func(t *testing.T) {
			_, err := local.ResolveFriendlyName(name)
			if err == nil {
				t.Fatalf("Expected error for %q, got nil", name)
			}
			if !strings.Contains(err.Error(), "unknown service") {
				t.Errorf("Expected 'unknown service' in error, got: %s", err.Error())
			}
			if !strings.Contains(err.Error(), "available:") {
				t.Errorf("Expected 'available:' in error message, got: %s", err.Error())
			}
		})
	}
}

// ---- ServiceRegistry ----

func TestServiceRegistry_AllCategoriesPresent(t *testing.T) {
	categories := map[string]bool{}
	for _, svc := range local.ServiceRegistry {
		categories[svc.Category] = true
	}

	expectedCategories := []string{"app", "infra", "runtime"}
	for _, cat := range expectedCategories {
		if !categories[cat] {
			t.Errorf("Expected category %q to be present in ServiceRegistry", cat)
		}
	}
}

func TestServiceRegistry_AllServicesHaveRequiredFields(t *testing.T) {
	for name, svc := range local.ServiceRegistry {
		t.Run(name, func(t *testing.T) {
			if svc.ComposeService == "" {
				t.Error("ComposeService is empty")
			}
			if svc.FriendlyName == "" {
				t.Error("FriendlyName is empty")
			}
			if svc.Port == 0 {
				t.Error("Port is zero")
			}
			if svc.URL == "" {
				t.Error("URL is empty")
			}
			if svc.Category == "" {
				t.Error("Category is empty")
			}
		})
	}
}

func TestServiceRegistry_NoDuplicateComposeSvcNames(t *testing.T) {
	seen := map[string]string{}
	for name, svc := range local.ServiceRegistry {
		if prev, ok := seen[svc.ComposeService]; ok {
			t.Errorf("Duplicate ComposeService %q found in %q and %q", svc.ComposeService, prev, name)
		}
		seen[svc.ComposeService] = name
	}
}

func TestServiceRegistry_NoDuplicatePorts(t *testing.T) {
	seen := map[int]string{}
	for name, svc := range local.ServiceRegistry {
		if prev, ok := seen[svc.Port]; ok {
			t.Errorf("Duplicate port %d found in %q and %q", svc.Port, prev, name)
		}
		seen[svc.Port] = name
	}
}

// ---- ComposeDir ----

func TestComposeDir_ReturnsExpectedPath(t *testing.T) {
	dir, err := local.ComposeDir()
	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}

	home, err := os.UserHomeDir()
	if err != nil {
		t.Fatalf("Cannot get home dir: %v", err)
	}

	expected := filepath.Join(home, ".the0", "compose")
	if dir != expected {
		t.Errorf("Expected %q, got %q", expected, dir)
	}
}

// ---- GenerateEnvFile ----

func TestGenerateEnvFile_CreatesFile(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "env-test")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	err = local.GenerateEnvFile(tmpDir)
	if err != nil {
		t.Fatalf("GenerateEnvFile failed: %v", err)
	}

	envPath := filepath.Join(tmpDir, ".env")
	data, err := os.ReadFile(envPath)
	if err != nil {
		t.Fatalf("Failed to read .env file: %v", err)
	}

	content := string(data)

	// Verify essential key=value pairs exist
	expectedKeys := []string{
		"POSTGRES_DB=",
		"POSTGRES_USER=",
		"POSTGRES_PASSWORD=",
		"MONGO_INITDB_ROOT_USERNAME=",
		"MONGO_INITDB_ROOT_PASSWORD=",
		"MINIO_ROOT_USER=",
		"MINIO_ROOT_PASSWORD=",
		"JWT_SECRET=",
		"API_PORT=",
		"FRONTEND_PORT=",
		"DOCS_PORT=",
	}

	for _, key := range expectedKeys {
		if !strings.Contains(content, key) {
			t.Errorf("Expected .env to contain %q", key)
		}
	}
}

func TestGenerateEnvFile_DoesNotOverwriteExisting(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "env-test")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	// Write a custom .env file first
	envPath := filepath.Join(tmpDir, ".env")
	customContent := "CUSTOM_KEY=custom_value\n"
	if err := os.WriteFile(envPath, []byte(customContent), 0600); err != nil {
		t.Fatalf("Failed to write custom .env: %v", err)
	}

	// GenerateEnvFile should skip the existing file
	err = local.GenerateEnvFile(tmpDir)
	if err != nil {
		t.Fatalf("GenerateEnvFile failed: %v", err)
	}

	// Verify original content is preserved
	data, err := os.ReadFile(envPath)
	if err != nil {
		t.Fatalf("Failed to read .env file: %v", err)
	}

	if string(data) != customContent {
		t.Errorf("Expected original content to be preserved, got: %s", string(data))
	}
}

func TestGenerateEnvFile_CorrectPermissions(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "env-test")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	err = local.GenerateEnvFile(tmpDir)
	if err != nil {
		t.Fatalf("GenerateEnvFile failed: %v", err)
	}

	envPath := filepath.Join(tmpDir, ".env")
	info, err := os.Stat(envPath)
	if err != nil {
		t.Fatalf("Failed to stat .env file: %v", err)
	}

	// File should be 0600 (owner read/write only)
	perm := info.Mode().Perm()
	if perm != 0600 {
		t.Errorf("Expected permissions 0600, got %o", perm)
	}
}

// ---- ValidateRepoPath ----

func TestValidateRepoPath_ValidRepo(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "repo-test")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	// Create the required directories
	requiredDirs := []string{"api", "frontend", "runtime", "docker"}
	for _, dir := range requiredDirs {
		if err := os.Mkdir(filepath.Join(tmpDir, dir), 0755); err != nil {
			t.Fatalf("Failed to create dir %s: %v", dir, err)
		}
	}

	err = local.ValidateRepoPath(tmpDir)
	if err != nil {
		t.Errorf("Expected valid repo path, got error: %v", err)
	}
}

func TestValidateRepoPath_MissingDirs(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "repo-test")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	// Only create some of the required directories
	os.Mkdir(filepath.Join(tmpDir, "api"), 0755)

	err = local.ValidateRepoPath(tmpDir)
	if err == nil {
		t.Fatal("Expected error for missing directories, got nil")
	}
	if !strings.Contains(err.Error(), "not a valid the0 repository") {
		t.Errorf("Expected 'not a valid the0 repository' in error, got: %s", err.Error())
	}
	// Should mention at least one missing dir
	if !strings.Contains(err.Error(), "missing:") {
		t.Errorf("Expected 'missing:' in error, got: %s", err.Error())
	}
}

func TestValidateRepoPath_NonExistentPath(t *testing.T) {
	err := local.ValidateRepoPath("/nonexistent/path/that/does/not/exist")
	if err == nil {
		t.Fatal("Expected error for non-existent path, got nil")
	}
	if !strings.Contains(err.Error(), "path does not exist") {
		t.Errorf("Expected 'path does not exist' in error, got: %s", err.Error())
	}
}

func TestValidateRepoPath_FileNotDir(t *testing.T) {
	tmpFile, err := os.CreateTemp("", "repo-test")
	if err != nil {
		t.Fatalf("Failed to create temp file: %v", err)
	}
	defer os.Remove(tmpFile.Name())
	tmpFile.Close()

	err = local.ValidateRepoPath(tmpFile.Name())
	if err == nil {
		t.Fatal("Expected error for file path, got nil")
	}
	if !strings.Contains(err.Error(), "not a directory") {
		t.Errorf("Expected 'not a directory' in error, got: %s", err.Error())
	}
}

// ---- LocalState JSON round-trip ----

func TestLocalState_JSONRoundTrip(t *testing.T) {
	original := &local.LocalState{
		RepoPath: "/home/user/the0",
		Prebuilt: false,
		InitAt:   time.Date(2025, 1, 15, 10, 30, 0, 0, time.UTC),
	}

	data, err := json.Marshal(original)
	if err != nil {
		t.Fatalf("Failed to marshal: %v", err)
	}

	var loaded local.LocalState
	if err := json.Unmarshal(data, &loaded); err != nil {
		t.Fatalf("Failed to unmarshal: %v", err)
	}

	if loaded.RepoPath != original.RepoPath {
		t.Errorf("RepoPath mismatch: expected %q, got %q", original.RepoPath, loaded.RepoPath)
	}
	if loaded.Prebuilt != original.Prebuilt {
		t.Errorf("Prebuilt mismatch: expected %v, got %v", original.Prebuilt, loaded.Prebuilt)
	}
	if !loaded.InitAt.Equal(original.InitAt) {
		t.Errorf("InitAt mismatch: expected %v, got %v", original.InitAt, loaded.InitAt)
	}
}

func TestLocalState_JSONFields(t *testing.T) {
	state := &local.LocalState{
		RepoPath: "/test/path",
		Prebuilt: true,
		InitAt:   time.Date(2025, 6, 1, 0, 0, 0, 0, time.UTC),
	}

	data, err := json.Marshal(state)
	if err != nil {
		t.Fatalf("Failed to marshal: %v", err)
	}

	content := string(data)

	// Verify JSON field names match the struct tags
	if !strings.Contains(content, `"repo_path"`) {
		t.Error("Expected JSON field 'repo_path'")
	}
	if !strings.Contains(content, `"prebuilt"`) {
		t.Error("Expected JSON field 'prebuilt'")
	}
	if !strings.Contains(content, `"init_at"`) {
		t.Error("Expected JSON field 'init_at'")
	}
}

// ---- ComposeRunner ----

func TestComposeRunner_BaseArgs(t *testing.T) {
	runner := &local.ComposeRunner{
		ComposeDir: "/tmp/test-compose",
		State:      &local.LocalState{RepoPath: "/test"},
	}

	args := runner.BaseArgs()

	expected := []string{
		"compose",
		"-f", "/tmp/test-compose/docker-compose.yml",
		"-p", "the0",
	}

	if len(args) != len(expected) {
		t.Fatalf("Expected %d args, got %d: %v", len(expected), len(args), args)
	}

	for i, arg := range expected {
		if args[i] != arg {
			t.Errorf("Arg[%d]: expected %q, got %q", i, arg, args[i])
		}
	}
}

func TestComposeRunner_DevArgs(t *testing.T) {
	runner := &local.ComposeRunner{
		ComposeDir: "/tmp/test-compose",
		State:      &local.LocalState{RepoPath: "/test"},
	}

	args := runner.DevArgs()

	expected := []string{
		"compose",
		"-f", "/tmp/test-compose/docker-compose.yml",
		"-f", "/tmp/test-compose/docker-compose.dev.yml",
		"-p", "the0",
	}

	if len(args) != len(expected) {
		t.Fatalf("Expected %d args, got %d: %v", len(expected), len(args), args)
	}

	for i, arg := range expected {
		if args[i] != arg {
			t.Errorf("Arg[%d]: expected %q, got %q", i, arg, args[i])
		}
	}
}

func TestComposeRunner_DevArgsIncludesBaseFile(t *testing.T) {
	runner := &local.ComposeRunner{
		ComposeDir: "/some/path",
		State:      &local.LocalState{},
	}

	devArgs := runner.DevArgs()
	baseArgs := runner.BaseArgs()

	// Dev args should include the base compose file plus the dev overlay
	if len(devArgs) != len(baseArgs)+2 {
		t.Errorf("Dev args should have 2 more entries than base args (got base=%d, dev=%d)", len(baseArgs), len(devArgs))
	}

	// First -f should be the same in both
	if devArgs[1] != baseArgs[1] || devArgs[2] != baseArgs[2] {
		t.Error("Dev args should start with the same -f flag as base args")
	}
}

// ---- Local Command Structure ----

func TestLocalCommand_HasSubcommands(t *testing.T) {
	localCmd := cmd.NewLocalCmd()

	expectedSubcommands := []string{
		"init",
		"start",
		"stop",
		"restart",
		"status",
		"logs",
		"dev",
		"uninstall",
	}

	subcommands := localCmd.Commands()
	subcommandNames := make(map[string]bool)
	for _, sub := range subcommands {
		subcommandNames[sub.Name()] = true
	}

	for _, expected := range expectedSubcommands {
		if !subcommandNames[expected] {
			t.Errorf("Expected subcommand %q not found in local command", expected)
		}
	}
}

func TestLocalCommand_Usage(t *testing.T) {
	localCmd := cmd.NewLocalCmd()

	if localCmd.Use != "local" {
		t.Errorf("Expected usage 'local', got %q", localCmd.Use)
	}

	if localCmd.Short == "" {
		t.Error("Expected non-empty short description")
	}
}

func TestLocalInitCommand_Flags(t *testing.T) {
	localCmd := cmd.NewLocalCmd()

	for _, sub := range localCmd.Commands() {
		if sub.Name() == "init" {
			repoPathFlag := sub.Flag("repo-path")
			if repoPathFlag == nil {
				t.Error("Expected init command to have 'repo-path' flag")
			}

			prebuiltFlag := sub.Flag("prebuilt")
			if prebuiltFlag == nil {
				t.Error("Expected init command to have 'prebuilt' flag")
			} else if prebuiltFlag.DefValue != "false" {
				t.Errorf("Expected prebuilt default 'false', got %q", prebuiltFlag.DefValue)
			}

			return
		}
	}
	t.Error("init subcommand not found")
}

func TestLocalLogsCommand_Flags(t *testing.T) {
	localCmd := cmd.NewLocalCmd()

	for _, sub := range localCmd.Commands() {
		if sub.Name() == "logs" {
			followFlag := sub.Flag("follow")
			if followFlag == nil {
				t.Error("Expected logs command to have 'follow' flag")
			} else {
				if followFlag.Shorthand != "f" {
					t.Errorf("Expected follow flag shorthand 'f', got %q", followFlag.Shorthand)
				}
			}

			tailFlag := sub.Flag("tail")
			if tailFlag == nil {
				t.Error("Expected logs command to have 'tail' flag")
			}

			return
		}
	}
	t.Error("logs subcommand not found")
}

func TestLocalUninstallCommand_YesFlag(t *testing.T) {
	localCmd := cmd.NewLocalCmd()

	for _, sub := range localCmd.Commands() {
		if sub.Name() == "uninstall" {
			yesFlag := sub.Flag("yes")
			if yesFlag == nil {
				t.Error("Expected uninstall command to have 'yes' flag")
			} else {
				if yesFlag.Shorthand != "y" {
					t.Errorf("Expected yes flag shorthand 'y', got %q", yesFlag.Shorthand)
				}
				if yesFlag.DefValue != "false" {
					t.Errorf("Expected yes flag default 'false', got %q", yesFlag.DefValue)
				}
			}

			return
		}
	}
	t.Error("uninstall subcommand not found")
}

// ---- EnsureInitialized ----

func TestEnsureInitialized_FailsWhenNotInitialized(t *testing.T) {
	// EnsureInitialized calls LoadState which reads from ~/.the0/compose/local-state.json.
	// If the state file doesn't exist, it should return an error.
	// We can test this by temporarily pointing HOME to a temp dir.
	tmpDir, err := os.MkdirTemp("", "ensure-init-test")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	// Save and restore HOME
	origHome := os.Getenv("HOME")
	os.Setenv("HOME", tmpDir)
	defer os.Setenv("HOME", origHome)

	_, err = local.EnsureInitialized()
	if err == nil {
		t.Fatal("Expected error when not initialized, got nil")
	}
	if !strings.Contains(err.Error(), "not initialized") {
		t.Errorf("Expected 'not initialized' in error, got: %s", err.Error())
	}
}

// ---- SaveState / LoadState round-trip ----

func TestSaveLoadState_RoundTrip(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "state-test")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	// Point HOME to temp dir so state files go there
	origHome := os.Getenv("HOME")
	os.Setenv("HOME", tmpDir)
	defer os.Setenv("HOME", origHome)

	// Create the compose directory
	composeDir := filepath.Join(tmpDir, ".the0", "compose")
	if err := os.MkdirAll(composeDir, 0755); err != nil {
		t.Fatalf("Failed to create compose dir: %v", err)
	}

	now := time.Now().Truncate(time.Second)
	original := &local.LocalState{
		RepoPath: "/home/testuser/the0",
		Prebuilt: false,
		InitAt:   now,
	}

	if err := local.SaveState(original); err != nil {
		t.Fatalf("SaveState failed: %v", err)
	}

	// Verify file was created
	stateFile := filepath.Join(composeDir, "local-state.json")
	if _, err := os.Stat(stateFile); os.IsNotExist(err) {
		t.Fatal("State file was not created")
	}

	// Verify file permissions
	info, err := os.Stat(stateFile)
	if err != nil {
		t.Fatalf("Failed to stat state file: %v", err)
	}
	if info.Mode().Perm() != 0600 {
		t.Errorf("Expected permissions 0600, got %o", info.Mode().Perm())
	}

	// Load it back
	loaded, err := local.LoadState()
	if err != nil {
		t.Fatalf("LoadState failed: %v", err)
	}

	if loaded.RepoPath != original.RepoPath {
		t.Errorf("RepoPath: expected %q, got %q", original.RepoPath, loaded.RepoPath)
	}
	if loaded.Prebuilt != original.Prebuilt {
		t.Errorf("Prebuilt: expected %v, got %v", original.Prebuilt, loaded.Prebuilt)
	}
	// Compare truncated to second since JSON marshaling may lose sub-second precision
	if !loaded.InitAt.Truncate(time.Second).Equal(original.InitAt.Truncate(time.Second)) {
		t.Errorf("InitAt: expected %v, got %v", original.InitAt, loaded.InitAt)
	}
}

func TestEnsureInitialized_SucceedsWhenInitialized(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "ensure-init-test")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	origHome := os.Getenv("HOME")
	os.Setenv("HOME", tmpDir)
	defer os.Setenv("HOME", origHome)

	// Create compose dir and state file
	composeDir := filepath.Join(tmpDir, ".the0", "compose")
	if err := os.MkdirAll(composeDir, 0755); err != nil {
		t.Fatalf("Failed to create compose dir: %v", err)
	}

	state := &local.LocalState{
		RepoPath: "/test/path",
		Prebuilt: false,
		InitAt:   time.Now(),
	}

	if err := local.SaveState(state); err != nil {
		t.Fatalf("SaveState failed: %v", err)
	}

	loaded, err := local.EnsureInitialized()
	if err != nil {
		t.Fatalf("EnsureInitialized should succeed, got error: %v", err)
	}
	if loaded.RepoPath != state.RepoPath {
		t.Errorf("RepoPath: expected %q, got %q", state.RepoPath, loaded.RepoPath)
	}
}

// ---- CleanupComposeDir ----

func TestCleanupComposeDir(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "cleanup-test")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	origHome := os.Getenv("HOME")
	os.Setenv("HOME", tmpDir)
	defer os.Setenv("HOME", origHome)

	// Create compose directory with a file
	composeDir := filepath.Join(tmpDir, ".the0", "compose")
	if err := os.MkdirAll(composeDir, 0755); err != nil {
		t.Fatalf("Failed to create compose dir: %v", err)
	}
	if err := os.WriteFile(filepath.Join(composeDir, "test.txt"), []byte("test"), 0644); err != nil {
		t.Fatalf("Failed to write test file: %v", err)
	}

	err = local.CleanupComposeDir()
	if err != nil {
		t.Fatalf("CleanupComposeDir failed: %v", err)
	}

	// Verify directory is gone
	if _, err := os.Stat(composeDir); !os.IsNotExist(err) {
		t.Error("Expected compose directory to be removed")
	}
}

func TestCleanupComposeDir_NoErrorWhenMissing(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "cleanup-test")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	origHome := os.Getenv("HOME")
	os.Setenv("HOME", tmpDir)
	defer os.Setenv("HOME", origHome)

	// Don't create the compose dir - cleanup should not error
	err = local.CleanupComposeDir()
	if err != nil {
		t.Errorf("CleanupComposeDir should not error on missing dir, got: %v", err)
	}
}
