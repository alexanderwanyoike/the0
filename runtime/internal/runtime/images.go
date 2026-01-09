// Package runtime provides shared runtime configuration for both Docker and Kubernetes modes.
// This package is the single source of truth for runtime-to-image mappings, validation,
// and resource defaults.
package runtime

import (
	"fmt"
	"os"
	"strings"
)

// UniversalRuntimeImage is the single Docker image that contains all language runtimes.
// Build with: cd docker/images && make runtime
const UniversalRuntimeImage = "the0/runtime:latest"

// supportedRuntimes lists all valid runtime identifiers.
// All runtimes use the universal image (the0/runtime:latest).
var supportedRuntimes = map[string]bool{
	"python3.11":  true,
	"nodejs20":    true,
	"rust-stable": true,
	"dotnet8":     true,
	"gcc13":       true,
	"cpp-gcc13":   true, // Alias for gcc13
	"scala3":      true,
	"ghc96":       true,
}

// GetDockerImage returns the Docker image for the given runtime.
// All runtimes use the universal image (the0/runtime:latest) which contains all language runtimes.
// Returns an error if the runtime is not supported.
func GetDockerImage(runtime string) (string, error) {
	if !supportedRuntimes[runtime] {
		return "", fmt.Errorf("unsupported runtime: %q (supported: %s)", runtime, strings.Join(SupportedRuntimes(), ", "))
	}

	// Allow override via environment variable (useful for development/testing)
	if override := os.Getenv("RUNTIME_IMAGE"); override != "" {
		return override, nil
	}

	return UniversalRuntimeImage, nil
}
