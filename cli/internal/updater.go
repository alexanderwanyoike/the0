package internal

import (
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"time"
)

const (
	GitHubOwner       = "alexanderwanyoike"
	GitHubRepo        = "the0"
	CLITagPrefix      = "cli/v"
	VersionCacheFile  = ".the0/version-check.json"
	CacheStaleDays    = 7
	GitHubAPIBase     = "https://api.github.com"
	ChecksumsFileName = "checksums.txt"
)

type Updater struct {
	CurrentVersion string
	GitHubOwner    string
	GitHubRepo     string
	HTTPClient     *http.Client
}

type GitHubRelease struct {
	TagName string        `json:"tag_name"`
	Name    string        `json:"name"`
	Assets  []GitHubAsset `json:"assets"`
}

type GitHubAsset struct {
	Name               string `json:"name"`
	BrowserDownloadURL string `json:"browser_download_url"`
}

type VersionCache struct {
	LastCheck     time.Time `json:"last_check"`
	LatestVersion string    `json:"latest_version"`
}

func NewUpdater(currentVersion string) *Updater {
	return &Updater{
		CurrentVersion: currentVersion,
		GitHubOwner:    GitHubOwner,
		GitHubRepo:     GitHubRepo,
		HTTPClient: &http.Client{
			Timeout: 10 * time.Second,
		},
	}
}

// CheckLatestRelease fetches GitHub releases and returns the latest CLI release.
func (u *Updater) CheckLatestRelease() (*GitHubRelease, error) {
	url := fmt.Sprintf("%s/repos/%s/%s/releases?per_page=20", GitHubAPIBase, u.GitHubOwner, u.GitHubRepo)

	req, err := http.NewRequest("GET", url, nil)
	if err != nil {
		return nil, fmt.Errorf("failed to create request: %w", err)
	}
	req.Header.Set("Accept", "application/vnd.github.v3+json")
	req.Header.Set("User-Agent", "the0-cli/"+u.CurrentVersion)

	resp, err := u.HTTPClient.Do(req)
	if err != nil {
		return nil, fmt.Errorf("failed to fetch releases: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("GitHub API returned status %d", resp.StatusCode)
	}

	var releases []GitHubRelease
	if err := json.NewDecoder(resp.Body).Decode(&releases); err != nil {
		return nil, fmt.Errorf("failed to parse releases: %w", err)
	}

	for _, release := range releases {
		if strings.HasPrefix(release.TagName, CLITagPrefix) {
			return &release, nil
		}
	}

	return nil, fmt.Errorf("no CLI releases found")
}

// GetPlatformBinaryName returns the expected binary name for the current platform.
func GetPlatformBinaryName() string {
	name := fmt.Sprintf("the0-%s-%s", runtime.GOOS, runtime.GOARCH)
	if runtime.GOOS == "windows" {
		name += ".exe"
	}
	return name
}

// DownloadAsset downloads a binary from the given URL.
func (u *Updater) DownloadAsset(url string) ([]byte, error) {
	req, err := http.NewRequest("GET", url, nil)
	if err != nil {
		return nil, fmt.Errorf("failed to create download request: %w", err)
	}
	req.Header.Set("User-Agent", "the0-cli/"+u.CurrentVersion)

	resp, err := u.HTTPClient.Do(req)
	if err != nil {
		return nil, fmt.Errorf("failed to download asset: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("download returned status %d", resp.StatusCode)
	}

	// Safety valve: cap downloads at 200 MB to avoid unbounded memory usage
	// if the server returns an unexpectedly large response.
	const maxDownloadSize = 200 << 20 // 200 MB
	data, err := io.ReadAll(io.LimitReader(resp.Body, maxDownloadSize+1))
	if err != nil {
		return nil, fmt.Errorf("failed to read download: %w", err)
	}
	if len(data) > maxDownloadSize {
		return nil, fmt.Errorf("download exceeds maximum size of %d bytes", maxDownloadSize)
	}

	return data, nil
}

// VerifyChecksum verifies the SHA256 checksum of binary data against a checksums file.
func VerifyChecksum(data []byte, checksumFile []byte, binaryName string) error {
	actualHash := sha256.Sum256(data)
	actualHex := hex.EncodeToString(actualHash[:])

	lines := strings.Split(string(checksumFile), "\n")
	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}
		parts := strings.Fields(line)
		if len(parts) != 2 {
			continue
		}
		if parts[1] == binaryName {
			if parts[0] == actualHex {
				return nil
			}
			return fmt.Errorf("checksum mismatch: expected %s, got %s", parts[0], actualHex)
		}
	}

	return fmt.Errorf("no checksum found for %s", binaryName)
}

// ReplaceBinary replaces the current running binary with new data.
func ReplaceBinary(newBinaryData []byte) error {
	execPath, err := os.Executable()
	if err != nil {
		return fmt.Errorf("failed to get executable path: %w", err)
	}

	execPath, err = filepath.EvalSymlinks(execPath)
	if err != nil {
		return fmt.Errorf("failed to resolve symlinks: %w", err)
	}

	// Write new binary to a temp file in the same directory
	dir := filepath.Dir(execPath)
	tmpFile, err := os.CreateTemp(dir, "the0-update-*")
	if err != nil {
		return fmt.Errorf("failed to create temp file: %w", err)
	}
	tmpPath := tmpFile.Name()

	if _, err := tmpFile.Write(newBinaryData); err != nil {
		tmpFile.Close()
		os.Remove(tmpPath)
		return fmt.Errorf("failed to write new binary: %w", err)
	}
	tmpFile.Close()

	// Make executable
	if err := os.Chmod(tmpPath, 0755); err != nil {
		os.Remove(tmpPath)
		return fmt.Errorf("failed to set permissions: %w", err)
	}

	// On Windows, rename the old binary out of the way first since
	// os.Rename cannot atomically replace a running executable.
	if runtime.GOOS == "windows" {
		oldPath := execPath + ".old"
		_ = os.Remove(oldPath) // clean up any previous leftover
		if err := os.Rename(execPath, oldPath); err != nil {
			os.Remove(tmpPath)
			return fmt.Errorf("failed to rename old binary: %w", err)
		}
	}

	// Atomic rename (or simple rename on Windows after moving the old binary)
	if err := os.Rename(tmpPath, execPath); err != nil {
		os.Remove(tmpPath)
		return fmt.Errorf("failed to replace binary: %w", err)
	}

	return nil
}

// ExtractVersionFromTag extracts the version from a tag like "cli/v1.4.0" -> "1.4.0".
func ExtractVersionFromTag(tag string) string {
	if strings.HasPrefix(tag, CLITagPrefix) {
		return strings.TrimPrefix(tag, CLITagPrefix)
	}
	return strings.TrimPrefix(tag, "v")
}

// ReadVersionCache reads the cached version check result.
func ReadVersionCache() (*VersionCache, error) {
	homeDir, err := os.UserHomeDir()
	if err != nil {
		return nil, err
	}

	cachePath := filepath.Join(homeDir, VersionCacheFile)
	data, err := os.ReadFile(cachePath)
	if err != nil {
		return nil, err
	}

	var cache VersionCache
	if err := json.Unmarshal(data, &cache); err != nil {
		return nil, err
	}

	return &cache, nil
}

// WriteVersionCache writes the version check result to cache.
func WriteVersionCache(cache *VersionCache) error {
	homeDir, err := os.UserHomeDir()
	if err != nil {
		return err
	}

	cacheDir := filepath.Join(homeDir, ".the0")
	if err := os.MkdirAll(cacheDir, 0700); err != nil {
		return err
	}

	cachePath := filepath.Join(homeDir, VersionCacheFile)
	data, err := json.MarshalIndent(cache, "", "  ")
	if err != nil {
		return err
	}

	return os.WriteFile(cachePath, data, 0600)
}

// IsCacheStale returns true if the cache is older than CacheStaleDays.
func IsCacheStale(cache *VersionCache) bool {
	return time.Since(cache.LastCheck) > time.Duration(CacheStaleDays)*24*time.Hour
}

// FindAssetByName finds an asset in a release by filename.
func FindAssetByName(release *GitHubRelease, name string) *GitHubAsset {
	for _, asset := range release.Assets {
		if asset.Name == name {
			return &asset
		}
	}
	return nil
}
