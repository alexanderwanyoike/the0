name: "CLI Self-Update Mechanism PRP v1"
description: |

## Goal
Implement a comprehensive self-update system for the THE0 CLI that automatically detects platform architecture, checks for latest versions, and seamlessly updates the binary from Google Cloud Storage buckets with full cross-platform support.

## Why
- **Developer Productivity**: Eliminates manual CLI update process for users, ensuring they always have the latest features and bug fixes
- **Operational Excellence**: Reduces support burden by automatically keeping users on supported versions
- **Feature Velocity**: Enables rapid feature rollout and hotfix deployment without requiring manual intervention
- **Development Workflow**: Supports staging/development channels for internal testing and validation

## What
Implement a self-update mechanism that provides:
- Auto-detection of updates during CLI startup (non-blocking)
- Manual update checking via `the0 check-update` command
- Seamless binary replacement via `the0 self-update` command
- Cross-platform support (macOS Intel/ARM, Linux x86/ARM, Windows)
- Development channel override via environment variables
- Checksum verification and rollback capabilities
- Progress indication for downloads

### Success Criteria
- [ ] CLI detects and notifies users of new versions during startup (< 2 second delay)
- [ ] Manual update checking shows current vs latest version with changelog
- [ ] Self-update replaces binary correctly on all platforms without requiring restart
- [ ] Development channel switching works via environment variable
- [ ] All operations work offline gracefully (cached version info)
- [ ] Comprehensive test coverage including platform-specific scenarios
- [ ] Checksum verification prevents corrupted updates

## All Needed Context

### Documentation & References
```yaml
# MUST READ - Include these in your context window
- url: https://pkg.go.dev/runtime
  why: Platform detection using runtime.GOOS and runtime.GOARCH constants

- url: https://github.com/inconshreveable/go-update
  why: Battle-tested Go library for binary self-updating with Windows file lock handling

- url: https://pkg.go.dev/github.com/creativeprojects/go-selfupdate
  why: Modern self-update library with GitHub releases integration and checksum verification

- url: https://gabrieleromanato.name/go-how-to-create-a-download-progress-indicator-for-an-http-request
  why: HTTP download progress indication using io.TeeReader patterns

- url: https://pkg.go.dev/github.com/Masterminds/semver/v3
  why: Semantic version comparison already used in the codebase

- url: https://pkg.go.dev/crypto/sha256
  why: Checksum verification for download integrity

- file: main.go
  why: Current version constant and CLI entry point for startup integration

- file: internal/api.go
  why: HTTP client patterns, timeout handling, and error handling conventions

- file: internal/auth.go
  why: File storage patterns and user home directory handling

- file: cmd/auth.go
  why: Command structure patterns and user-friendly output formatting

- file: tests/api_test.go
  why: HTTP testing patterns using httptest.NewServer

- file: tests/cli_test.go
  why: Command testing patterns and table-driven test structure

- file: .github/workflows/the0-cli.yml
  why: Current build process, version naming, bucket structure, and platform binaries

- file: Makefile
  why: Build patterns and platform compilation targets

- file: go.mod
  why: Existing dependencies including semver for version comparison
```

### Current Codebase tree
```bash
apps/the0/
├── CLAUDE.md
├── Makefile                    # Cross-platform build targets
├── cmd/
│   ├── auth.go                 # Command patterns and user output
│   ├── bot.go                  # Complex command with subcommands
│   ├── custom_bot.go          # File operations and validation
│   └── user_bot.go            # API integration patterns
├── go.mod                     # Dependencies (semver already included)
├── internal/
│   ├── api.go                 # HTTP client patterns and error handling
│   ├── auth.go                # File storage and home directory patterns
│   ├── config.go              # Configuration validation patterns
│   ├── vendor.go              # Download and file operations
│   └── zip.go                 # File packaging utilities
├── main.go                    # Entry point with VERSION constant
└── tests/                     # Testing patterns
    ├── api_test.go            # HTTP mocking patterns
    ├── cli_test.go            # Command testing patterns
    └── ...
```

### Desired Codebase tree with files to be added
```bash
apps/the0/
├── cmd/
│   ├── auth.go
│   ├── bot.go
│   ├── custom_bot.go
│   ├── user_bot.go
│   ├── check_update.go        # Manual update checking command
│   └── self_update.go         # Self-update command
├── internal/
│   ├── api.go
│   ├── auth.go
│   ├── config.go
│   ├── vendor.go
│   ├── zip.go
│   ├── updater.go             # Core update logic and version management
│   └── version.go             # Version checking and comparison
├── main.go                    # Modified to include startup update check
└── tests/
    ├── ...existing tests...
    ├── updater_test.go         # Update mechanism testing
    ├── version_test.go         # Version comparison testing
    └── integration_update_test.go  # End-to-end update testing
```

### Known Gotchas & Library Quirks
```go
// CRITICAL: Windows file locking prevents direct binary replacement
// Solution: Use rename strategy - rename current binary, then rename new binary
// Pattern: current.exe -> current-old.exe, new.exe -> current.exe

// CRITICAL: Cobra commands need to be added to root in init() functions
// Pattern: rootCmd.AddCommand(newCommand) in cmd files

// CRITICAL: HTTP timeout handling required for large binary downloads
// Pattern: Use context.WithTimeout and custom http.Client with extended timeout

// CRITICAL: Semantic version comparison must handle both production and develop formats
// Production: v2024.01.15-123, Development: develop-2024.01.15-123
// Use semver.NewVersion with preprocessing to handle develop prefix

// CRITICAL: GCS public URLs have specific format
// Production: https://storage.googleapis.com/the0-cli-releases/
// Staging: https://storage.googleapis.com/the0-cli-releases-staging/

// CRITICAL: Platform detection maps to binary naming convention
// runtime.GOOS-runtime.GOARCH maps to the0-{goos}-{goarch}[.exe]

// CRITICAL: Startup update check must be non-blocking and fast
// Use goroutine with timeout, graceful fallback on failure

// CRITICAL: versions.json structure from GitHub Actions workflow
// Contains: version, branch, commit, date, environment, platforms array
```

## Implementation Blueprint

### Data models and structure
```go
// Core types for version management and update operations
type ReleaseInfo struct {
    Version     string    `json:"version"`
    Branch      string    `json:"branch"`
    Commit      string    `json:"commit"`
    Date        time.Time `json:"date"`
    Environment string    `json:"environment"`
    Platforms   []string  `json:"platforms"`
}

type VersionManifest struct {
    Versions []ReleaseInfo `json:"versions"`
}

type UpdateChannel string
const (
    ProductionChannel UpdateChannel = "production"
    StagingChannel    UpdateChannel = "staging"
)

type PlatformInfo struct {
    OS           string // runtime.GOOS
    Arch         string // runtime.GOARCH
    BinaryName   string // the0-darwin-arm64 or the0-windows-amd64.exe
    DownloadURL  string // Full GCS URL for binary
    ChecksumURL  string // URL for SHA256 checksum file
}

type UpdateConfig struct {
    Channel         UpdateChannel
    CurrentVersion  string
    CheckTimeout    time.Duration
    DownloadTimeout time.Duration
    BaseURL         string
}

type ProgressWriter struct {
    Total    int64
    Written  int64
    OnUpdate func(written, total int64)
}
```

### List of tasks to be completed in order

```yaml
Task 1: Create Core Version Management
CREATE internal/version.go:
  - MIRROR pattern from: internal/config.go (validation patterns)
  - ADD semantic version comparison with develop/production format handling
  - IMPLEMENT GetCurrentVersion() to extract from main.go VERSION constant
  - ADD version string preprocessing for semver compatibility
  - PRESERVE existing error handling patterns

Task 2: Create Update Configuration Management  
CREATE internal/updater.go:
  - MIRROR pattern from: internal/api.go (HTTP client and timeout handling)
  - ADD UpdateConfig struct with channel detection from environment
  - IMPLEMENT platform detection using runtime.GOOS/runtime.GOARCH
  - ADD GCS URL construction for different channels
  - PRESERVE existing HTTP client timeout patterns

Task 3: Implement Version Checking Logic
MODIFY internal/updater.go:
  - ADD FetchVersionManifest() method using HTTP patterns from api.go
  - IMPLEMENT CheckForUpdates() with version comparison
  - ADD platform compatibility checking against manifest platforms
  - PRESERVE existing error wrapping patterns with fmt.Errorf

Task 4: Implement Binary Download with Progress
MODIFY internal/updater.go:
  - ADD DownloadBinary() method with progress indication
  - IMPLEMENT ProgressWriter using io.TeeReader pattern
  - ADD checksum verification using crypto/sha256
  - PRESERVE existing timeout and error handling patterns

Task 5: Implement Binary Replacement Logic
MODIFY internal/updater.go:
  - ADD ReplaceBinary() method with platform-specific handling
  - IMPLEMENT Windows rename strategy for file lock handling
  - ADD Unix atomic replacement with permission preservation
  - PRESERVE existing file operation patterns from vendor.go

Task 6: Create Manual Update Check Command
CREATE cmd/check_update.go:
  - MIRROR pattern from: cmd/auth.go (command structure and output)
  - ADD cobra.Command with version comparison display
  - IMPLEMENT flag handling for channel override
  - PRESERVE existing command naming and help text conventions

Task 7: Create Self-Update Command
CREATE cmd/self_update.go:
  - MIRROR pattern from: cmd/custom_bot.go (complex operations with progress)
  - ADD cobra.Command with flags for check-only, force, yes modes
  - IMPLEMENT channel override flag handling
  - PRESERVE existing progress display patterns

Task 8: Integrate Startup Update Check
MODIFY main.go:
  - FIND main() function after rootCmd setup
  - ADD non-blocking goroutine for update checking
  - IMPLEMENT timeout-based version check with graceful fallback
  - PRESERVE existing error handling and exit patterns

Task 9: Update Root Command Registration
MODIFY main.go:
  - FIND command registration section (AddCommand calls)
  - ADD new update commands to root command
  - PRESERVE existing command ordering patterns

Task 10: Create Comprehensive Tests
CREATE tests/version_test.go:
  - MIRROR pattern from: tests/api_test.go (table-driven HTTP tests)
  - ADD version comparison test cases for all formats
  - IMPLEMENT platform detection testing
  - PRESERVE existing test structure and assertion patterns

CREATE tests/updater_test.go:
  - MIRROR pattern from: tests/api_test.go (httptest server mocking)
  - ADD update check, download, and replacement test scenarios
  - IMPLEMENT checksum verification testing
  - PRESERVE existing mock server patterns

CREATE tests/integration_update_test.go:
  - MIRROR pattern from: tests/cli_test.go (command integration testing)
  - ADD end-to-end update command testing
  - IMPLEMENT platform-specific binary replacement testing
  - PRESERVE existing integration test patterns
```

### Per task pseudocode with CRITICAL details

```go
// Task 1: Version Management Core
package internal

import (
    "fmt"
    "strings"
    "github.com/Masterminds/semver/v3"
)

// CRITICAL: Handle both production and develop version formats
func ParseVersion(versionStr string) (*semver.Version, error) {
    // Preprocess develop versions: develop-2024.01.15-123 -> 0.0.0-develop.2024.01.15.123
    if strings.HasPrefix(versionStr, "develop-") {
        cleaned := strings.TrimPrefix(versionStr, "develop-")
        versionStr = "0.0.0-develop." + strings.ReplaceAll(cleaned, "-", ".")
    }
    
    return semver.NewVersion(versionStr)
}

func CompareVersions(current, latest string) (bool, error) {
    currentVer, err := ParseVersion(current)
    if err != nil {
        return false, fmt.Errorf("invalid current version: %w", err)
    }
    
    latestVer, err := ParseVersion(latest)
    if err != nil {
        return false, fmt.Errorf("invalid latest version: %w", err)
    }
    
    return latestVer.GreaterThan(currentVer), nil
}

// Task 2: Platform Detection and URL Construction
func GetPlatformInfo(channel UpdateChannel) *PlatformInfo {
    goos := runtime.GOOS
    goarch := runtime.GOARCH
    
    // CRITICAL: Binary naming matches GitHub Actions workflow
    binaryName := fmt.Sprintf("the0-%s-%s", goos, goarch)
    if goos == "windows" {
        binaryName += ".exe"
    }
    
    // CRITICAL: URL construction matches GCS bucket structure
    baseURL := "https://storage.googleapis.com/the0-cli-releases"
    if channel == StagingChannel {
        baseURL += "-staging"
    }
    
    return &PlatformInfo{
        OS:         goos,
        Arch:       goarch,
        BinaryName: binaryName,
        DownloadURL: fmt.Sprintf("%s/latest/%s", baseURL, binaryName),
        ChecksumURL: fmt.Sprintf("%s/latest/checksums.txt", baseURL),
    }
}

// Task 3: Version Manifest Fetching
func (u *Updater) FetchVersionManifest(ctx context.Context) (*VersionManifest, error) {
    // PATTERN: Use existing HTTP client patterns from api.go
    client := &http.Client{
        Timeout: u.config.CheckTimeout, // CRITICAL: Short timeout for startup
    }
    
    url := fmt.Sprintf("%s/versions.json", u.getBaseURL())
    req, err := http.NewRequestWithContext(ctx, "GET", url, nil)
    if err != nil {
        return nil, fmt.Errorf("failed to create request: %w", err)
    }
    
    resp, err := client.Do(req)
    if err != nil {
        return nil, fmt.Errorf("failed to fetch version manifest: %w", err)
    }
    defer resp.Body.Close()
    
    if resp.StatusCode != 200 {
        return nil, fmt.Errorf("version manifest request failed: HTTP %d", resp.StatusCode)
    }
    
    var manifest VersionManifest
    if err := json.NewDecoder(resp.Body).Decode(&manifest); err != nil {
        return nil, fmt.Errorf("failed to decode version manifest: %w", err)
    }
    
    return &manifest, nil
}

// Task 4: Binary Download with Progress
func (u *Updater) DownloadBinary(ctx context.Context, url string, progressCallback func(int64, int64)) ([]byte, error) {
    // CRITICAL: Extended timeout for large binary downloads
    client := &http.Client{
        Timeout: u.config.DownloadTimeout,
    }
    
    req, err := http.NewRequestWithContext(ctx, "GET", url, nil)
    if err != nil {
        return nil, fmt.Errorf("failed to create download request: %w", err)
    }
    
    resp, err := client.Do(req)
    if err != nil {
        return nil, fmt.Errorf("download failed: %w", err)
    }
    defer resp.Body.Close()
    
    if resp.StatusCode != 200 {
        return nil, fmt.Errorf("download failed: HTTP %d", resp.StatusCode)
    }
    
    // PATTERN: Progress indication using io.TeeReader
    var buf bytes.Buffer
    progressWriter := &ProgressWriter{
        Total:    resp.ContentLength,
        OnUpdate: progressCallback,
    }
    
    _, err = io.Copy(&buf, io.TeeReader(resp.Body, progressWriter))
    if err != nil {
        return nil, fmt.Errorf("failed to download binary: %w", err)
    }
    
    return buf.Bytes(), nil
}

// Task 5: Binary Replacement with Platform Handling
func (u *Updater) ReplaceBinary(newBinaryData []byte) error {
    currentExePath, err := os.Executable()
    if err != nil {
        return fmt.Errorf("failed to get current executable path: %w", err)
    }
    
    if runtime.GOOS == "windows" {
        // CRITICAL: Windows file lock handling via rename strategy
        return u.replaceBinaryWindows(currentExePath, newBinaryData)
    }
    
    return u.replaceBinaryUnix(currentExePath, newBinaryData)
}

func (u *Updater) replaceBinaryWindows(currentPath string, newData []byte) error {
    // Create temporary file for new binary
    tempPath := currentPath + ".new"
    if err := os.WriteFile(tempPath, newData, 0755); err != nil {
        return fmt.Errorf("failed to write new binary: %w", err)
    }
    
    // Rename current binary (works even if file is locked)
    oldPath := currentPath + ".old"
    if err := os.Rename(currentPath, oldPath); err != nil {
        os.Remove(tempPath) // Cleanup
        return fmt.Errorf("failed to rename current binary: %w", err)
    }
    
    // Rename new binary to current name
    if err := os.Rename(tempPath, currentPath); err != nil {
        os.Rename(oldPath, currentPath) // Rollback
        return fmt.Errorf("failed to install new binary: %w", err)
    }
    
    // Cleanup old binary (may fail if still locked, that's ok)
    os.Remove(oldPath)
    
    return nil
}

func (u *Updater) replaceBinaryUnix(currentPath string, newData []byte) error {
    // CRITICAL: Atomic replacement with permission preservation
    stat, err := os.Stat(currentPath)
    if err != nil {
        return fmt.Errorf("failed to stat current binary: %w", err)
    }
    
    // Create temporary file in same directory for atomic move
    tempPath := currentPath + ".new"
    if err := os.WriteFile(tempPath, newData, stat.Mode()); err != nil {
        return fmt.Errorf("failed to write new binary: %w", err)
    }
    
    // Atomic replacement
    if err := os.Rename(tempPath, currentPath); err != nil {
        os.Remove(tempPath) // Cleanup
        return fmt.Errorf("failed to replace binary: %w", err)
    }
    
    return nil
}

// Task 6: Check Update Command
var checkUpdateCmd = &cobra.Command{
    Use:   "check-update",
    Short: "Check for available updates",
    Long:  "Check if a newer version of the CLI is available without installing it.",
    RunE: func(cmd *cobra.Command, args []string) error {
        // PATTERN: Flag handling like existing commands
        channel, _ := cmd.Flags().GetString("channel")
        
        updater := NewUpdater(UpdateConfig{
            Channel:        UpdateChannel(channel),
            CurrentVersion: VERSION, // From main.go
            CheckTimeout:   10 * time.Second,
        })
        
        ctx := cmd.Context()
        updateAvailable, latestVersion, err := updater.CheckForUpdates(ctx)
        if err != nil {
            return fmt.Errorf("failed to check for updates: %w", err)
        }
        
        if !updateAvailable {
            fmt.Printf("✅ You're running the latest version: %s\n", VERSION)
            return nil
        }
        
        // PATTERN: User-friendly output like auth.go
        fmt.Printf("⚠️  Update available!\n")
        fmt.Printf("   Current: %s\n", VERSION)
        fmt.Printf("   Latest:  %s\n", latestVersion)
        fmt.Printf("\n   Run 'the0 self-update' to update now.\n")
        
        return nil
    },
}

func init() {
    checkUpdateCmd.Flags().String("channel", "production", "Update channel (production|staging)")
    // CRITICAL: Add to root command - this will be done in main.go
}

// Task 8: Startup Integration
func main() {
    var rootCmd = &cobra.Command{
        // ... existing setup ...
    }
    
    // Add commands
    rootCmd.AddCommand(cmd.NewCustomBotCmd())
    rootCmd.AddCommand(cmd.NewBotCmd())
    rootCmd.AddCommand(cmd.NewUserBotCmd())
    rootCmd.AddCommand(cmd.NewAuthCmd())
    rootCmd.AddCommand(cmd.NewCheckUpdateCmd()) // NEW
    rootCmd.AddCommand(cmd.NewSelfUpdateCmd())  // NEW
    
    // CRITICAL: Non-blocking startup update check
    go func() {
        // Skip if quiet mode or if we're running a help command
        if os.Getenv("THE0_QUIET") != "" || len(os.Args) > 1 && (os.Args[1] == "help" || os.Args[1] == "--help") {
            return
        }
        
        updater := internal.NewUpdater(internal.UpdateConfig{
            Channel:        internal.GetUpdateChannel(), // From env var
            CurrentVersion: VERSION,
            CheckTimeout:   2 * time.Second, // CRITICAL: Fast timeout
        })
        
        ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
        defer cancel()
        
        updateAvailable, latestVersion, err := updater.CheckForUpdates(ctx)
        if err != nil {
            // CRITICAL: Fail silently on startup check errors
            return
        }
        
        if updateAvailable {
            fmt.Fprintf(os.Stderr, "⚠️  A new version of the0 CLI is available!\n")
            fmt.Fprintf(os.Stderr, "   Current: %s\n", VERSION)
            fmt.Fprintf(os.Stderr, "   Latest:  %s\n", latestVersion)
            fmt.Fprintf(os.Stderr, "\n   Run 'the0 self-update' to update or 'the0 check-update --help' for options.\n\n")
        }
    }()
    
    if err := rootCmd.Execute(); err != nil {
        fmt.Fprintf(os.Stderr, "Error: %v\n", err)
        os.Exit(1)
    }
}
```

## Validation Loop

### Level 1: Syntax & Style
```bash
# Run these FIRST - fix any errors before proceeding
go fmt ./...                         # Format code
go vet ./...                        # Static analysis  
go mod tidy                         # Clean up dependencies
make build                          # Test compilation

# Expected: No errors. If errors, READ the error and fix.
```

### Level 2: Unit Tests
```bash
# Create comprehensive test suite
go test -v ./internal/              # Test core update logic
go test -v ./cmd/                   # Test command implementations
go test -race ./...                 # Check for race conditions
go test -cover ./...                # Check test coverage (aim for >80%)

# Integration tests with mock GCS responses
go test -v ./tests/updater_test.go
go test -v ./tests/version_test.go

# Platform-specific testing
GOOS=windows go test ./internal/updater_test.go
GOOS=darwin go test ./internal/updater_test.go
GOOS=linux go test ./internal/updater_test.go
```

### Level 3: Manual Integration Testing
```bash
# Test manual commands
./bin/the0 check-update
./bin/the0 check-update --channel=staging  
./bin/the0 self-update --check-only
./bin/the0 self-update --help

# Test environment variable override
THE0_CLI_UPDATE_CHANNEL=staging ./bin/the0 check-update

# Test startup integration (should show update notification if available)
./bin/the0 help

# Test quiet mode (should not show notifications)
THE0_QUIET=1 ./bin/the0 help
```

## Final Validation Checklist
- [ ] All tests pass: `go test ./...`
- [ ] No race conditions: `go test -race ./...`
- [ ] Build succeeds on all platforms: `make release`
- [ ] Startup check completes in <2 seconds
- [ ] Update commands work offline gracefully
- [ ] Binary replacement works on Windows/macOS/Linux
- [ ] Checksum verification prevents corrupted updates
- [ ] Environment variable channel switching works
- [ ] Progress indication shows during downloads
- [ ] Error messages are user-friendly and actionable

---

## Anti-Patterns to Avoid
- ❌ Don't block CLI startup with slow update checks
- ❌ Don't ignore platform-specific file locking issues
- ❌ Don't skip checksum verification for security
- ❌ Don't hardcode GCS URLs or version formats
- ❌ Don't use global state for update configuration
- ❌ Don't ignore network timeout scenarios
- ❌ Don't forget to handle partial download interruptions

## Quality Score: 9/10

**Confidence Level**: High confidence in one-pass implementation success due to:
- ✅ Comprehensive external library research with proven patterns
- ✅ Complete codebase analysis revealing all necessary patterns
- ✅ Platform-specific implementation details researched and documented
- ✅ Clear task breakdown with specific file modifications
- ✅ Validation strategy covering unit, integration, and manual testing
- ✅ Known gotchas identified and solutions provided

**Potential Risk Areas** (hence 9/10, not 10/10):
- Binary replacement edge cases on different platforms may require iteration
- Network error handling edge cases may need refinement during testing