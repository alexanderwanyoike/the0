// Package runtime provides shared runtime configuration for both Docker and Kubernetes modes.
// This package is the single source of truth for runtime-to-image mappings, validation,
// and resource defaults.
package runtime

import (
	"fmt"
	"strings"
)

// runtimeImages maps runtime identifiers to their Docker images.
// These images include the the0 daemon binary for state/log synchronization.
// Build with: cd docker/images && make build-all
var runtimeImages = map[string]string{
	"python3.11":  "the0/python311:latest",
	"nodejs20":    "the0/nodejs20:latest",
	"rust-stable": "the0/rust-stable:latest",
	"dotnet8":     "the0/dotnet8:latest",
	"gcc13":       "the0/gcc13:latest",
	"cpp-gcc13":   "the0/gcc13:latest", // Alias for gcc13
	"scala3":      "the0/scala3:latest",
	"ghc96":       "the0/ghc96:latest",
}

// GetDockerImage returns the Docker image for the given runtime.
// Returns an error if the runtime is not supported.
func GetDockerImage(runtime string) (string, error) {
	if image, ok := runtimeImages[runtime]; ok {
		return image, nil
	}
	return "", fmt.Errorf("unsupported runtime: %q (supported: %s)", runtime, strings.Join(SupportedRuntimes(), ", "))
}
