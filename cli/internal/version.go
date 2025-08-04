package internal

import (
	"fmt"
	"strings"

	"github.com/Masterminds/semver/v3"
)

// ParseVersion handles both production and develop version formats
// Production format: v2024.01.15-123 or 2024.01.15-123
// Development format: develop-2024.01.15-123
func ParseVersion(versionStr string) (*semver.Version, error) {
	// Clean version string
	cleaned := strings.TrimSpace(versionStr)

	// Handle develop versions: develop-2024.01.15-123 -> 2024.01.15-123
	if strings.HasPrefix(cleaned, "develop-") {
		cleaned = strings.TrimPrefix(cleaned, "develop-")
	} else {
		// Handle production versions - ensure they start with 'v'
		if !strings.HasPrefix(cleaned, "v") {
			cleaned = "v" + cleaned
		}
	}

	return semver.NewVersion(cleaned)
}

// CompareVersions compares two version strings and returns true if latest > current
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

// GetCurrentVersion extracts the current version from the VERSION constant
func GetCurrentVersion() string {
	// This will be imported from main package when needed
	// For now, we'll make it configurable
	return "1.0.0"
}

// NormalizeVersion normalizes a version string for display
func NormalizeVersion(version string) string {
	cleaned := strings.TrimSpace(version)

	// If it's a develop version, show it as-is
	if strings.HasPrefix(cleaned, "develop-") {
		return cleaned
	}

	// Remove 'v' prefix for display if present
	if strings.HasPrefix(cleaned, "v") {
		return strings.TrimPrefix(cleaned, "v")
	}

	return cleaned
}
