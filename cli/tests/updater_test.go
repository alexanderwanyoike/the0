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
	"testing"
	"time"

	"the0/internal"
)

func TestCheckLatestRelease(t *testing.T) {
	tests := []struct {
		name          string
		releases      []map[string]interface{}
		expectedTag   string
		expectedError bool
		errorContains string
	}{
		{
			name: "finds CLI release",
			releases: []map[string]interface{}{
				{
					"tag_name": "runtime/v2.0.0",
					"name":     "Runtime v2.0.0",
					"assets":   []map[string]interface{}{},
				},
				{
					"tag_name": "cli/v1.4.0",
					"name":     "CLI v1.4.0",
					"assets": []map[string]interface{}{
						{"name": "the0-linux-amd64", "browser_download_url": "https://example.com/the0-linux-amd64"},
					},
				},
			},
			expectedTag: "cli/v1.4.0",
		},
		{
			name: "skips non-CLI releases",
			releases: []map[string]interface{}{
				{
					"tag_name": "api/v3.0.0",
					"name":     "API v3.0.0",
					"assets":   []map[string]interface{}{},
				},
				{
					"tag_name": "frontend/v2.0.0",
					"name":     "Frontend v2.0.0",
					"assets":   []map[string]interface{}{},
				},
			},
			expectedError: true,
			errorContains: "no CLI releases found",
		},
		{
			name:          "empty releases",
			releases:      []map[string]interface{}{},
			expectedError: true,
			errorContains: "no CLI releases found",
		},
		{
			name: "returns first CLI release (most recent)",
			releases: []map[string]interface{}{
				{
					"tag_name": "cli/v1.5.0",
					"name":     "CLI v1.5.0",
					"assets":   []map[string]interface{}{},
				},
				{
					"tag_name": "cli/v1.4.0",
					"name":     "CLI v1.4.0",
					"assets":   []map[string]interface{}{},
				},
			},
			expectedTag: "cli/v1.5.0",
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
			updater.HTTPClient = server.Client()

			// Override the API base by using a custom HTTP client that rewrites URLs
			// Instead, we need to actually hit the test server. Let's use a transport.
			transport := &rewriteTransport{
				base:    server.Client().Transport,
				baseURL: server.URL,
			}
			updater.HTTPClient = &http.Client{Transport: transport}

			release, err := updater.CheckLatestRelease()

			if tt.expectedError {
				if err == nil {
					t.Errorf("CheckLatestRelease() expected error but got nil")
				} else if tt.errorContains != "" && !contains(err.Error(), tt.errorContains) {
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
	// Rewrite the host to our test server
	req.URL.Scheme = "http"
	req.URL.Host = t.baseURL[len("http://"):]
	if t.base != nil {
		return t.base.RoundTrip(req)
	}
	return http.DefaultTransport.RoundTrip(req)
}

func contains(s, substr string) bool {
	return len(s) >= len(substr) && (s == substr || len(s) > 0 && containsStr(s, substr))
}

func containsStr(s, substr string) bool {
	for i := 0; i <= len(s)-len(substr); i++ {
		if s[i:i+len(substr)] == substr {
			return true
		}
	}
	return false
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
		{"cli/v1.4.0", "1.4.0"},
		{"cli/v2.0.0-beta.1", "2.0.0-beta.1"},
		{"cli/v0.1.0", "0.1.0"},
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
				} else if tt.errorContains != "" && !containsStr(err.Error(), tt.errorContains) {
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
	origHome := os.Getenv("HOME")
	os.Setenv("HOME", tmpDir)
	defer os.Setenv("HOME", origHome)

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
		os.Setenv("HOME", t.TempDir())
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
		TagName: "cli/v1.4.0",
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
