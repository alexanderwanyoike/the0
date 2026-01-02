// Package runtime provides shared runtime configuration for both Docker and Kubernetes modes.
// This package is the single source of truth for runtime-to-image mappings, validation,
// and resource defaults.
package runtime

import (
	"fmt"
	"strings"
)

// runtimeImages maps runtime identifiers to their Docker images.
// Pin images to specific versions for reproducibility and security.
var runtimeImages = map[string]string{
	"python3.11": "python:3.11-slim",
	"nodejs20":   "node:20-alpine",
	"rust-stable": "rust:1.83-slim",   // Pinned from rust:latest
	"dotnet8":    "mcr.microsoft.com/dotnet/runtime:8.0",
	"gcc13":      "gcc:13",
	"cpp-gcc13":  "gcc:13", // Alias for gcc13
	"scala3":     "eclipse-temurin:21-jre",
	"ghc96":      "haskell:9.6-slim",
}

// GetDockerImage returns the Docker image for the given runtime.
// Returns the Python 3.11 image as fallback for unknown runtimes.
// Use GetDockerImageOrError if you need explicit error handling.
func GetDockerImage(runtime string) string {
	if image, ok := runtimeImages[runtime]; ok {
		return image
	}
	return "python:3.11-slim" // Default fallback
}

// GetDockerImageOrError returns the Docker image for the given runtime.
// Returns an error if the runtime is not supported.
func GetDockerImageOrError(runtime string) (string, error) {
	if image, ok := runtimeImages[runtime]; ok {
		return image, nil
	}
	return "", fmt.Errorf("unsupported runtime: %q (supported: %s)", runtime, strings.Join(SupportedRuntimes(), ", "))
}
