package internal

import (
	"bytes"
	"context"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"runtime"
	"strings"
	"time"
)

// UpdateChannel represents the update channel
type UpdateChannel string

const (
	ProductionChannel UpdateChannel = "production"
	StagingChannel    UpdateChannel = "staging"
)

// ReleaseInfo represents a single release version
type ReleaseInfo struct {
	Version     string    `json:"version"`
	Branch      string    `json:"branch"`
	Commit      string    `json:"commit"`
	Date        time.Time `json:"date"`
	Environment string    `json:"environment"`
	Platforms   []string  `json:"platforms"`
}

// VersionManifest represents the version manifest structure
type VersionManifest struct {
	Versions []ReleaseInfo `json:"versions"`
}

// PlatformInfo contains platform-specific information
type PlatformInfo struct {
	OS          string // runtime.GOOS
	Arch        string // runtime.GOARCH
	BinaryName  string // the0-darwin-arm64 or the0-windows-amd64.exe
	DownloadURL string // Full GCS URL for binary
	ChecksumURL string // URL for SHA256 checksum file
}

// UpdateConfig contains configuration for the updater
type UpdateConfig struct {
	Channel         UpdateChannel
	CurrentVersion  string
	CheckTimeout    time.Duration
	DownloadTimeout time.Duration
	BaseURL         string
}

// ProgressWriter tracks download progress
type ProgressWriter struct {
	Total    int64
	Written  int64
	OnUpdate func(written, total int64)
}

func (pw *ProgressWriter) Write(p []byte) (int, error) {
	n := len(p)
	pw.Written += int64(n)
	if pw.OnUpdate != nil {
		pw.OnUpdate(pw.Written, pw.Total)
	}
	return n, nil
}

// Updater handles the update process
type Updater struct {
	config       UpdateConfig
	platformInfo *PlatformInfo
	httpClient   *http.Client
}

// NewUpdater creates a new updater instance
func NewUpdater(config UpdateConfig) *Updater {
	// Set default timeouts if not provided
	if config.CheckTimeout == 0 {
		config.CheckTimeout = 10 * time.Second
	}
	if config.DownloadTimeout == 0 {
		config.DownloadTimeout = 5 * time.Minute
	}

	// Set base URL if not provided
	if config.BaseURL == "" {
		config.BaseURL = "https://storage.googleapis.com/the0-cli-releases"
		if config.Channel == StagingChannel {
			config.BaseURL = "https://storage.googleapis.com/the0-cli-releases-staging"
		}
	}

	// Create platform info with custom base URL
	platformInfo := GetPlatformInfo(config.Channel)
	if config.BaseURL != "" {
		// Override URLs with custom base URL
		platformInfo.DownloadURL = fmt.Sprintf("%s/latest/%s", config.BaseURL, platformInfo.BinaryName)
		platformInfo.ChecksumURL = fmt.Sprintf("%s/latest/checksums.txt", config.BaseURL)
	}

	return &Updater{
		config:       config,
		platformInfo: platformInfo,
		httpClient: &http.Client{
			Timeout: config.CheckTimeout,
		},
	}
}

// GetPlatformInfo returns platform-specific information
func GetPlatformInfo(channel UpdateChannel) *PlatformInfo {
	goos := runtime.GOOS
	goarch := runtime.GOARCH

	// Binary naming matches GitHub Actions workflow
	binaryName := fmt.Sprintf("the0-%s-%s", goos, goarch)
	if goos == "windows" {
		binaryName += ".exe"
	}

	// URL construction matches GCS bucket structure
	baseURL := "https://storage.googleapis.com/the0-cli-releases"
	if channel == StagingChannel {
		baseURL += "-staging"
	}

	return &PlatformInfo{
		OS:          goos,
		Arch:        goarch,
		BinaryName:  binaryName,
		DownloadURL: fmt.Sprintf("%s/latest/%s", baseURL, binaryName),
		ChecksumURL: fmt.Sprintf("%s/latest/checksums.txt", baseURL),
	}
}

// GetUpdateChannel returns the update channel from environment variable
func GetUpdateChannel() UpdateChannel {
	channel := os.Getenv("THE0_CLI_UPDATE_CHANNEL")
	if channel == "staging" {
		return StagingChannel
	}
	return ProductionChannel
}

// FetchVersionManifest fetches the version manifest from GCS
func (u *Updater) FetchVersionManifest(ctx context.Context) (*VersionManifest, error) {
	url := fmt.Sprintf("%s/versions.json", u.config.BaseURL)

	req, err := http.NewRequestWithContext(ctx, "GET", url, nil)
	if err != nil {
		return nil, fmt.Errorf("failed to create request: %w", err)
	}

	// Add cache-busting headers to ensure fresh data
	req.Header.Set("Cache-Control", "no-cache")
	req.Header.Set("Pragma", "no-cache")

	// Add timestamp to query string for additional cache busting
	if strings.Contains(url, "?") {
		url += "&"
	} else {
		url += "?"
	}
	url += fmt.Sprintf("t=%d", time.Now().Unix())

	// Recreate request with cache-busted URL
	req, err = http.NewRequestWithContext(ctx, "GET", url, nil)
	if err != nil {
		return nil, fmt.Errorf("failed to create request: %w", err)
	}
	req.Header.Set("Cache-Control", "no-cache")
	req.Header.Set("Pragma", "no-cache")

	resp, err := u.httpClient.Do(req)
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

// CheckForUpdates checks if an update is available
func (u *Updater) CheckForUpdates(ctx context.Context) (bool, string, error) {
	manifest, err := u.FetchVersionManifest(ctx)
	if err != nil {
		return false, "", err
	}

	if len(manifest.Versions) == 0 {
		return false, "", fmt.Errorf("no versions found in manifest")
	}

	// Get the latest version (first in the list)
	latestVersion := manifest.Versions[0].Version

	// Check if our platform is supported
	// Extract platform part from binary name (remove "the0-" prefix)
	currentPlatform := strings.TrimPrefix(u.platformInfo.BinaryName, "the0-")
	// Remove .exe suffix if present
	currentPlatform = strings.TrimSuffix(currentPlatform, ".exe")

	platformSupported := false
	for _, platform := range manifest.Versions[0].Platforms {
		if platform == currentPlatform {
			platformSupported = true
			break
		}
	}

	if !platformSupported {
		return false, "", fmt.Errorf("current platform (%s) not supported in latest version", u.platformInfo.BinaryName)
	}

	// Compare versions
	updateAvailable, err := CompareVersions(u.config.CurrentVersion, latestVersion)
	if err != nil {
		return false, "", fmt.Errorf("failed to compare versions: %w", err)
	}

	return updateAvailable, latestVersion, nil
}

// DownloadBinary downloads the binary with progress indication
func (u *Updater) DownloadBinary(ctx context.Context, progressCallback func(int64, int64)) ([]byte, error) {
	// Use extended timeout for downloads
	client := &http.Client{
		Timeout: u.config.DownloadTimeout,
	}

	req, err := http.NewRequestWithContext(ctx, "GET", u.platformInfo.DownloadURL, nil)
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

	// Progress indication using io.TeeReader
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

// VerifyChecksum verifies the downloaded binary checksum
func (u *Updater) VerifyChecksum(ctx context.Context, binaryData []byte) error {
	// Download checksums file
	client := &http.Client{
		Timeout: u.config.CheckTimeout,
	}

	req, err := http.NewRequestWithContext(ctx, "GET", u.platformInfo.ChecksumURL, nil)
	if err != nil {
		return fmt.Errorf("failed to create checksum request: %w", err)
	}

	resp, err := client.Do(req)
	if err != nil {
		return fmt.Errorf("failed to download checksums: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != 200 {
		return fmt.Errorf("checksum download failed: HTTP %d", resp.StatusCode)
	}

	checksumData, err := io.ReadAll(resp.Body)
	if err != nil {
		return fmt.Errorf("failed to read checksums: %w", err)
	}

	// Parse checksums file to find our binary
	checksumLines := strings.Split(string(checksumData), "\n")
	var expectedChecksum string

	for _, line := range checksumLines {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}

		parts := strings.Fields(line)
		if len(parts) >= 2 {
			checksum := parts[0]
			filename := parts[1]
			if filename == u.platformInfo.BinaryName {
				expectedChecksum = checksum
				break
			}
		}
	}

	if expectedChecksum == "" {
		return fmt.Errorf("checksum not found for binary: %s", u.platformInfo.BinaryName)
	}

	// Calculate actual checksum
	hash := sha256.Sum256(binaryData)
	actualChecksum := hex.EncodeToString(hash[:])

	if actualChecksum != expectedChecksum {
		return fmt.Errorf("checksum mismatch: expected %s, got %s", expectedChecksum, actualChecksum)
	}

	return nil
}

// ReplaceBinary replaces the current binary with the new one
func (u *Updater) ReplaceBinary(newBinaryData []byte) error {
	currentExePath, err := os.Executable()
	if err != nil {
		return fmt.Errorf("failed to get current executable path: %w", err)
	}

	if runtime.GOOS == "windows" {
		// Windows file lock handling via rename strategy
		return u.replaceBinaryWindows(currentExePath, newBinaryData)
	}

	return u.replaceBinaryUnix(currentExePath, newBinaryData)
}

// replaceBinaryWindows handles Windows-specific binary replacement
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

// replaceBinaryUnix handles Unix-specific binary replacement
func (u *Updater) replaceBinaryUnix(currentPath string, newData []byte) error {
	// Atomic replacement with permission preservation
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

// SelfUpdate performs a complete self-update
func (u *Updater) SelfUpdate(ctx context.Context, progressCallback func(int64, int64)) error {
	// Check for updates first
	updateAvailable, latestVersion, err := u.CheckForUpdates(ctx)
	if err != nil {
		return fmt.Errorf("failed to check for updates: %w", err)
	}

	if !updateAvailable {
		return fmt.Errorf("no update available")
	}

	// Download binary
	binaryData, err := u.DownloadBinary(ctx, progressCallback)
	if err != nil {
		return fmt.Errorf("failed to download binary: %w", err)
	}

	// Verify checksum
	if err := u.VerifyChecksum(ctx, binaryData); err != nil {
		return fmt.Errorf("failed to verify checksum: %w", err)
	}

	// Replace binary
	if err := u.ReplaceBinary(binaryData); err != nil {
		return fmt.Errorf("failed to replace binary: %w", err)
	}

	fmt.Printf("✅ Successfully updated to version %s\n", latestVersion)
	return nil
}
