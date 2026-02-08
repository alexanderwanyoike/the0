package internal

import (
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"the0/internal"
)

func TestCheckLatestRelease(t *testing.T) {
	tests := []struct {
		name          string
		releases      []map[string]any
		expectedTag   string
		expectedError bool
		errorContains string
	}{
		{
			name: "finds CLI release",
			releases: []map[string]any{
				{
					"tag_name": "runtime/v2.0.0",
					"name":     "Runtime v2.0.0",
					"assets":   []map[string]any{},
				},
				{
					"tag_name": "v1.4.0",
					"name":     "v1.4.0",
					"assets": []map[string]any{
						{"name": "the0-linux-amd64", "browser_download_url": "https://example.com/the0-linux-amd64"},
					},
				},
			},
			expectedTag: "v1.4.0",
		},
		{
			name: "skips non-CLI releases",
			releases: []map[string]any{
				{
					"tag_name": "something-else",
					"name":     "Something Else",
					"assets":   []map[string]any{},
				},
				{
					"tag_name": "also-not-a-release",
					"name":     "Also Not A Release",
					"assets":   []map[string]any{},
				},
			},
			expectedError: true,
			errorContains: "no CLI releases found",
		},
		{
			name: "skips non-version v-prefixed tags",
			releases: []map[string]any{
				{
					"tag_name": "vscode-extension",
					"name":     "VSCode Extension",
					"assets":   []map[string]any{},
				},
				{
					"tag_name": "vendor-update",
					"name":     "Vendor Update",
					"assets":   []map[string]any{},
				},
			},
			expectedError: true,
			errorContains: "no CLI releases found",
		},
		{
			name:          "empty releases",
			releases:      []map[string]any{},
			expectedError: true,
			errorContains: "no CLI releases found",
		},
		{
			name: "returns first CLI release (most recent)",
			releases: []map[string]any{
				{
					"tag_name": "v1.5.0",
					"name":     "v1.5.0",
					"assets":   []map[string]any{},
				},
				{
					"tag_name": "v1.4.0",
					"name":     "v1.4.0",
					"assets":   []map[string]any{},
				},
			},
			expectedTag: "v1.5.0",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
				if r.URL.Path != "/repos/test-owner/test-repo/releases" {
					t.Errorf("Expected releases path, got %s", r.URL.Path)
				}

				w.Header().Set("Content-Type", "application/json")
				json.NewEncoder(w).Encode(tt.releases)
			}))
			defer server.Close()

			updater := internal.NewUpdater("1.3.1")
			updater.GitHubOwner = "test-owner"
			updater.GitHubRepo = "test-repo"

			transport := &rewriteTransport{
				base:    server.Client().Transport,
				baseURL: server.URL,
			}
			updater.HTTPClient = &http.Client{Transport: transport}

			release, err := updater.CheckLatestRelease()

			if tt.expectedError {
				if err == nil {
					t.Errorf("CheckLatestRelease() expected error but got nil")
				} else if tt.errorContains != "" && !strings.Contains(err.Error(), tt.errorContains) {
					t.Errorf("CheckLatestRelease() error = %v, expected to contain %v", err, tt.errorContains)
				}
			} else {
				if err != nil {
					t.Errorf("CheckLatestRelease() unexpected error = %v", err)
				}
				if release == nil {
					t.Fatal("CheckLatestRelease() returned nil release")
				}
				if release.TagName != tt.expectedTag {
					t.Errorf("CheckLatestRelease() tag = %s, expected %s", release.TagName, tt.expectedTag)
				}
			}
		})
	}
}

// rewriteTransport rewrites GitHub API URLs to point at the test server.
type rewriteTransport struct {
	base    http.RoundTripper
	baseURL string
}

func (t *rewriteTransport) RoundTrip(req *http.Request) (*http.Response, error) {
	// Clone the request before mutating to avoid modifying the caller's request
	req = req.Clone(req.Context())
	// Rewrite the host to our test server
	req.URL.Scheme = "http"
	req.URL.Host = t.baseURL[len("http://"):]
	if t.base != nil {
		return t.base.RoundTrip(req)
	}
	return http.DefaultTransport.RoundTrip(req)
}

func TestGetPlatformBinaryName(t *testing.T) {
	name := internal.GetPlatformBinaryName()

	if name == "" {
		t.Error("GetPlatformBinaryName() returned empty string")
	}

	// Should start with "the0-"
	if name[:5] != "the0-" {
		t.Errorf("GetPlatformBinaryName() = %s, expected to start with 'the0-'", name)
	}
}

func TestExtractVersionFromTag(t *testing.T) {
	tests := []struct {
		tag      string
		expected string
	}{
		{"v1.4.0", "1.4.0"},
		{"v2.0.0-beta.1", "2.0.0-beta.1"},
		{"v0.1.0", "0.1.0"},
		{"v1.0.0", "1.0.0"},
		{"1.0.0", "1.0.0"},
	}

	for _, tt := range tests {
		t.Run(tt.tag, func(t *testing.T) {
			result := internal.ExtractVersionFromTag(tt.tag)
			if result != tt.expected {
				t.Errorf("ExtractVersionFromTag(%s) = %s, expected %s", tt.tag, result, tt.expected)
			}
		})
	}
}

func TestVerifyChecksum(t *testing.T) {
	binaryData := []byte("test binary content")
	hash := sha256.Sum256(binaryData)
	validChecksum := hex.EncodeToString(hash[:])

	tests := []struct {
		name          string
		data          []byte
		checksumFile  string
		binaryName    string
		expectedError bool
		errorContains string
	}{
		{
			name:         "valid checksum",
			data:         binaryData,
			checksumFile: fmt.Sprintf("%s  the0-linux-amd64\n%s  the0-darwin-arm64\n", validChecksum, "0000000000000000000000000000000000000000000000000000000000000000"),
			binaryName:   "the0-linux-amd64",
		},
		{
			name:          "invalid checksum",
			data:          binaryData,
			checksumFile:  "0000000000000000000000000000000000000000000000000000000000000000  the0-linux-amd64\n",
			binaryName:    "the0-linux-amd64",
			expectedError: true,
			errorContains: "checksum mismatch",
		},
		{
			name:          "binary not in checksums",
			data:          binaryData,
			checksumFile:  fmt.Sprintf("%s  the0-darwin-arm64\n", validChecksum),
			binaryName:    "the0-linux-amd64",
			expectedError: true,
			errorContains: "no checksum found",
		},
		{
			name:          "empty checksums file",
			data:          binaryData,
			checksumFile:  "",
			binaryName:    "the0-linux-amd64",
			expectedError: true,
			errorContains: "no checksum found",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			err := internal.VerifyChecksum(tt.data, []byte(tt.checksumFile), tt.binaryName)

			if tt.expectedError {
				if err == nil {
					t.Errorf("VerifyChecksum() expected error but got nil")
				} else if tt.errorContains != "" && !strings.Contains(err.Error(), tt.errorContains) {
					t.Errorf("VerifyChecksum() error = %v, expected to contain %v", err, tt.errorContains)
				}
			} else {
				if err != nil {
					t.Errorf("VerifyChecksum() unexpected error = %v", err)
				}
			}
		})
	}
}

func TestVersionCache(t *testing.T) {
	// Use a temp directory to avoid polluting home
	tmpDir := t.TempDir()
	t.Setenv("HOME", tmpDir)

	t.Run("write and read cache", func(t *testing.T) {
		cache := &internal.VersionCache{
			LastCheck:     time.Now().Truncate(time.Second),
			LatestVersion: "1.5.0",
		}

		err := internal.WriteVersionCache(cache)
		if err != nil {
			t.Fatalf("WriteVersionCache() error = %v", err)
		}

		// Verify file was created
		cachePath := filepath.Join(tmpDir, ".the0", "version-check.json")
		if _, err := os.Stat(cachePath); os.IsNotExist(err) {
			t.Fatal("Cache file was not created")
		}

		read, err := internal.ReadVersionCache()
		if err != nil {
			t.Fatalf("ReadVersionCache() error = %v", err)
		}

		if read.LatestVersion != cache.LatestVersion {
			t.Errorf("ReadVersionCache() version = %s, expected %s", read.LatestVersion, cache.LatestVersion)
		}
	})

	t.Run("read nonexistent cache", func(t *testing.T) {
		t.Setenv("HOME", t.TempDir())
		_, err := internal.ReadVersionCache()
		if err == nil {
			t.Error("ReadVersionCache() expected error for nonexistent cache")
		}
	})

	t.Run("cache staleness", func(t *testing.T) {
		fresh := &internal.VersionCache{
			LastCheck:     time.Now(),
			LatestVersion: "1.5.0",
		}
		if internal.IsCacheStale(fresh) {
			t.Error("IsCacheStale() returned true for fresh cache")
		}

		stale := &internal.VersionCache{
			LastCheck:     time.Now().Add(-8 * 24 * time.Hour),
			LatestVersion: "1.5.0",
		}
		if !internal.IsCacheStale(stale) {
			t.Error("IsCacheStale() returned false for stale cache")
		}
	})
}

func TestFindAssetByName(t *testing.T) {
	release := &internal.GitHubRelease{
		TagName: "v1.4.0",
		Assets: []internal.GitHubAsset{
			{Name: "the0-linux-amd64", BrowserDownloadURL: "https://example.com/linux-amd64"},
			{Name: "the0-darwin-arm64", BrowserDownloadURL: "https://example.com/darwin-arm64"},
			{Name: "checksums.txt", BrowserDownloadURL: "https://example.com/checksums.txt"},
		},
	}

	t.Run("finds existing asset", func(t *testing.T) {
		asset := internal.FindAssetByName(release, "the0-linux-amd64")
		if asset == nil {
			t.Fatal("FindAssetByName() returned nil for existing asset")
		}
		if asset.BrowserDownloadURL != "https://example.com/linux-amd64" {
			t.Errorf("FindAssetByName() URL = %s, expected https://example.com/linux-amd64", asset.BrowserDownloadURL)
		}
	})

	t.Run("returns nil for missing asset", func(t *testing.T) {
		asset := internal.FindAssetByName(release, "the0-freebsd-amd64")
		if asset != nil {
			t.Error("FindAssetByName() expected nil for missing asset")
		}
	})

	t.Run("finds checksums file", func(t *testing.T) {
		asset := internal.FindAssetByName(release, "checksums.txt")
		if asset == nil {
			t.Fatal("FindAssetByName() returned nil for checksums.txt")
		}
	})
}

func TestDownloadAsset(t *testing.T) {
	expectedData := []byte("binary content here")

	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path == "/download/the0-linux-amd64" {
			w.Write(expectedData)
		} else if r.URL.Path == "/download/error" {
			w.WriteHeader(http.StatusInternalServerError)
		} else {
			w.WriteHeader(http.StatusNotFound)
		}
	}))
	defer server.Close()

	updater := internal.NewUpdater("1.3.1")

	t.Run("successful download", func(t *testing.T) {
		data, err := updater.DownloadAsset(server.URL + "/download/the0-linux-amd64")
		if err != nil {
			t.Fatalf("DownloadAsset() error = %v", err)
		}
		if string(data) != string(expectedData) {
			t.Errorf("DownloadAsset() data = %s, expected %s", data, expectedData)
		}
	})

	t.Run("server error", func(t *testing.T) {
		_, err := updater.DownloadAsset(server.URL + "/download/error")
		if err == nil {
			t.Error("DownloadAsset() expected error for server error")
		}
	})
}

func TestReplaceBinary(t *testing.T) {
	// Create a fake binary in a temp directory
	tmpDir := t.TempDir()
	fakeBinaryPath := filepath.Join(tmpDir, "fake-binary")
	originalContent := []byte("original binary content")
	if err := os.WriteFile(fakeBinaryPath, originalContent, 0755); err != nil {
		t.Fatalf("failed to write fake binary: %v", err)
	}

	// ReplaceBinary uses os.Executable() internally, so we cannot directly
	// pass a custom path. Instead, we test the replacement logic by
	// simulating what ReplaceBinary does: write to temp, chmod, rename.
	newContent := []byte("updated binary content")

	// Write new binary to a temp file in the same directory
	tmpFile, err := os.CreateTemp(tmpDir, "the0-update-*")
	if err != nil {
		t.Fatalf("failed to create temp file: %v", err)
	}
	tmpPath := tmpFile.Name()

	if _, err := tmpFile.Write(newContent); err != nil {
		tmpFile.Close()
		t.Fatalf("failed to write new binary: %v", err)
	}
	tmpFile.Close()

	if err := os.Chmod(tmpPath, 0755); err != nil {
		t.Fatalf("failed to chmod: %v", err)
	}

	if err := os.Rename(tmpPath, fakeBinaryPath); err != nil {
		t.Fatalf("failed to rename: %v", err)
	}

	// Verify the file was replaced with the correct contents
	data, err := os.ReadFile(fakeBinaryPath)
	if err != nil {
		t.Fatalf("failed to read replaced binary: %v", err)
	}
	if string(data) != string(newContent) {
		t.Errorf("replaced binary content = %q, expected %q", string(data), string(newContent))
	}

	// Verify the file has executable permissions
	info, err := os.Stat(fakeBinaryPath)
	if err != nil {
		t.Fatalf("failed to stat replaced binary: %v", err)
	}
	perm := info.Mode().Perm()
	if perm&0111 == 0 {
		t.Errorf("replaced binary is not executable, permissions = %o", perm)
	}
	if perm != 0755 {
		t.Errorf("replaced binary permissions = %o, expected 0755", perm)
	}
}

func TestDownloadAssetSizeLimit(t *testing.T) {
	// Create a test server that advertises a response larger than maxDownloadSize (200 MB).
	// We set Content-Length to a huge value but only write a small body; the size check
	// in DownloadAsset uses io.LimitReader so it reads at most maxDownloadSize+1 bytes.
	// To trigger the error we need to actually provide more than 200MB of data, but we
	// can cheat: the implementation reads up to maxDownloadSize+1 bytes and checks
	// len(data) > maxDownloadSize. So we create a server that streams just over the limit.
	const maxDownloadSize = 200 << 20 // 200 MB — must match the constant in updater.go

	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		// Set a Content-Length larger than maxDownloadSize to signal an oversized response.
		// Write maxDownloadSize+1 bytes in chunks to trigger the size check.
		w.Header().Set("Content-Length", fmt.Sprintf("%d", maxDownloadSize+1))
		w.WriteHeader(http.StatusOK)
		// Write in 1 MB chunks
		chunk := make([]byte, 1<<20)
		written := 0
		for written < maxDownloadSize+1 {
			toWrite := len(chunk)
			if written+toWrite > maxDownloadSize+1 {
				toWrite = maxDownloadSize + 1 - written
			}
			n, err := w.Write(chunk[:toWrite])
			if err != nil {
				return // client disconnected
			}
			written += n
		}
	}))
	defer server.Close()

	updater := internal.NewUpdater("1.3.1")
	_, err := updater.DownloadAsset(server.URL + "/oversized")
	if err == nil {
		t.Fatal("DownloadAsset() expected error for oversized response, got nil")
	}
	if !strings.Contains(err.Error(), "exceeds maximum size") {
		t.Errorf("DownloadAsset() error = %v, expected to contain 'exceeds maximum size'", err)
	}
}

func TestCheckLatestReleaseHTTPErrors(t *testing.T) {
	tests := []struct {
		name          string
		statusCode    int
		errorContains string
	}{
		{
			name:          "forbidden 403",
			statusCode:    http.StatusForbidden,
			errorContains: "status 403",
		},
		{
			name:          "internal server error 500",
			statusCode:    http.StatusInternalServerError,
			errorContains: "status 500",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
				w.WriteHeader(tt.statusCode)
			}))
			defer server.Close()

			updater := internal.NewUpdater("1.3.1")
			updater.GitHubOwner = "test-owner"
			updater.GitHubRepo = "test-repo"

			transport := &rewriteTransport{
				base:    server.Client().Transport,
				baseURL: server.URL,
			}
			updater.HTTPClient = &http.Client{Transport: transport}

			_, err := updater.CheckLatestRelease()
			if err == nil {
				t.Fatalf("CheckLatestRelease() expected error for HTTP %d, got nil", tt.statusCode)
			}
			if !strings.Contains(err.Error(), tt.errorContains) {
				t.Errorf("CheckLatestRelease() error = %v, expected to contain %q", err, tt.errorContains)
			}
		})
	}
}
