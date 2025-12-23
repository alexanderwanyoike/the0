package internal

import (
	"testing"
	"the0/internal"
)

func TestParseVersion(t *testing.T) {
	testCases := []struct {
		name        string
		version     string
		expectError bool
	}{
		{
			name:        "Production version with v prefix",
			version:     "v2024.01.15-123",
			expectError: false,
		},
		{
			name:        "Production version without v prefix",
			version:     "2024.01.15-123",
			expectError: false,
		},
		{
			name:        "Development version",
			version:     "develop-2024.01.15-123",
			expectError: false,
		},
		{
			name:        "Simple version",
			version:     "1.0.0",
			expectError: false,
		},
		{
			name:        "Version with v prefix",
			version:     "v1.0.0",
			expectError: false,
		},
		{
			name:        "Invalid version",
			version:     "invalid.version",
			expectError: true,
		},
		{
			name:        "Empty version",
			version:     "",
			expectError: true,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			_, err := internal.ParseVersion(tc.version)
			if tc.expectError && err == nil {
				t.Errorf("Expected error for version %s, but got none", tc.version)
			}
			if !tc.expectError && err != nil {
				t.Errorf("Expected no error for version %s, but got: %v", tc.version, err)
			}
		})
	}
}

func TestCompareVersions(t *testing.T) {
	testCases := []struct {
		name         string
		current      string
		latest       string
		expectUpdate bool
		expectError  bool
	}{
		{
			name:         "Same version",
			current:      "1.0.0",
			latest:       "1.0.0",
			expectUpdate: false,
			expectError:  false,
		},
		{
			name:         "Update available",
			current:      "1.0.0",
			latest:       "1.0.1",
			expectUpdate: true,
			expectError:  false,
		},
		{
			name:         "Current is newer",
			current:      "1.0.1",
			latest:       "1.0.0",
			expectUpdate: false,
			expectError:  false,
		},
		{
			name:         "Production versions",
			current:      "2024.01.15-123",
			latest:       "2024.01.16-124",
			expectUpdate: true,
			expectError:  false,
		},
		{
			name:         "Develop vs production",
			current:      "develop-2024.01.15-123",
			latest:       "2024.01.16-124",
			expectUpdate: true,
			expectError:  false,
		},
		{
			name:         "Both develop versions",
			current:      "develop-2024.01.15-123",
			latest:       "develop-2024.01.16-124",
			expectUpdate: true,
			expectError:  false,
		},
		{
			name:         "Invalid current version",
			current:      "invalid",
			latest:       "1.0.0",
			expectUpdate: false,
			expectError:  true,
		},
		{
			name:         "Invalid latest version",
			current:      "1.0.0",
			latest:       "invalid",
			expectUpdate: false,
			expectError:  true,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			updateAvailable, err := internal.CompareVersions(tc.current, tc.latest)

			if tc.expectError && err == nil {
				t.Errorf("Expected error for current=%s, latest=%s, but got none", tc.current, tc.latest)
			}
			if !tc.expectError && err != nil {
				t.Errorf("Expected no error for current=%s, latest=%s, but got: %v", tc.current, tc.latest, err)
			}
			if !tc.expectError && updateAvailable != tc.expectUpdate {
				t.Errorf("Expected update=%v for current=%s, latest=%s, but got %v", tc.expectUpdate, tc.current, tc.latest, updateAvailable)
			}
		})
	}
}

func TestNormalizeVersion(t *testing.T) {
	testCases := []struct {
		name     string
		version  string
		expected string
	}{
		{
			name:     "Version with v prefix",
			version:  "v1.0.0",
			expected: "1.0.0",
		},
		{
			name:     "Version without v prefix",
			version:  "1.0.0",
			expected: "1.0.0",
		},
		{
			name:     "Develop version",
			version:  "develop-2024.01.15-123",
			expected: "develop-2024.01.15-123",
		},
		{
			name:     "Production version with v",
			version:  "v2024.01.15-123",
			expected: "2024.01.15-123",
		},
		{
			name:     "Empty version",
			version:  "",
			expected: "",
		},
		{
			name:     "Version with whitespace",
			version:  "  v1.0.0  ",
			expected: "1.0.0",
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			result := internal.NormalizeVersion(tc.version)
			if result != tc.expected {
				t.Errorf("Expected %s, but got %s", tc.expected, result)
			}
		})
	}
}
