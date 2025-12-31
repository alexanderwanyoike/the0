// Package imagebuilder provides container image building for Kubernetes-native
// bot execution. It generates Dockerfiles and orchestrates Kaniko jobs to build
// pre-baked container images for each custom bot version.
package imagebuilder

import (
	"fmt"
	"regexp"
	"strings"
)

// entrypointFileRegex validates entrypoint filenames to prevent command injection.
// Allows alphanumeric characters, dots, hyphens, underscores, and forward slashes.
var entrypointFileRegex = regexp.MustCompile(`^[a-zA-Z0-9._/-]+$`)

// validateEntrypointFile checks that the entrypoint filename is safe.
// Returns an error if the filename contains potentially dangerous characters.
func validateEntrypointFile(entrypointFile string) error {
	if entrypointFile == "" {
		return fmt.Errorf("entrypoint file cannot be empty")
	}
	if !entrypointFileRegex.MatchString(entrypointFile) {
		return fmt.Errorf("invalid entrypoint file: contains disallowed characters")
	}
	// Check for path traversal
	if strings.Contains(entrypointFile, "..") {
		return fmt.Errorf("invalid entrypoint file: path traversal not allowed")
	}
	return nil
}

// RuntimeConfig holds configuration for a specific runtime.
type RuntimeConfig struct {
	// BaseImage is the Docker base image to use.
	BaseImage string
	// InstallCommand is the command to install dependencies.
	InstallCommand string
	// EntrypointCommand is the command to run the bot.
	EntrypointCommand string
	// DependencyFile is the file that contains dependencies (e.g., requirements.txt).
	DependencyFile string
}

// runtimeConfigs maps runtime names to their configurations.
var runtimeConfigs = map[string]RuntimeConfig{
	"python3.11": {
		BaseImage:         "python:3.11-slim",
		InstallCommand:    "pip install --no-cache-dir -r requirements.txt",
		EntrypointCommand: "/bin/bash /bot/entrypoint.sh",
		DependencyFile:    "requirements.txt",
	},
	"nodejs20": {
		BaseImage:         "node:20-alpine",
		InstallCommand:    "npm install --production",
		EntrypointCommand: "/bin/sh /bot/entrypoint.sh",
		DependencyFile:    "package.json",
	},
	"rust-stable": {
		BaseImage:         "debian:bookworm-slim",
		InstallCommand:    "", // CLI pre-builds, just run binary
		EntrypointCommand: "/bin/bash /bot/entrypoint.sh",
		DependencyFile:    "",
	},
	"dotnet8": {
		BaseImage:         "mcr.microsoft.com/dotnet/runtime:8.0",
		InstallCommand:    "", // CLI pre-builds, just run binary
		EntrypointCommand: "/bin/bash /bot/entrypoint.sh",
		DependencyFile:    "",
	},
	"gcc13": {
		BaseImage:         "debian:bookworm-slim",
		InstallCommand:    "", // CLI pre-builds, just run binary
		EntrypointCommand: "/bin/bash /bot/entrypoint.sh",
		DependencyFile:    "",
	},
	"scala3": {
		BaseImage:         "eclipse-temurin:21-jre",
		InstallCommand:    "", // CLI pre-builds, just run JAR
		EntrypointCommand: "/bin/bash /bot/entrypoint.sh",
		DependencyFile:    "",
	},
	"ghc96": {
		BaseImage:         "debian:bookworm-slim",
		InstallCommand:    "", // CLI pre-builds, just run binary
		EntrypointCommand: "/bin/bash /bot/entrypoint.sh",
		DependencyFile:    "",
	},
}

// DockerfileGenerator generates Dockerfiles for bot container images.
type DockerfileGenerator struct{}

// NewDockerfileGenerator creates a new DockerfileGenerator.
func NewDockerfileGenerator() *DockerfileGenerator {
	return &DockerfileGenerator{}
}

// GenerateDockerfile generates a Dockerfile for the given runtime and entrypoint.
// The generated Dockerfile:
// 1. Uses the appropriate base image for the runtime
// 2. Sets up the working directory at /bot
// 3. Copies all bot code
// 4. Installs dependencies if dependency file exists
// 5. Sets up the entrypoint script
func (g *DockerfileGenerator) GenerateDockerfile(runtime, entrypointFile string) (string, error) {
	// Validate entrypoint file to prevent command injection
	if err := validateEntrypointFile(entrypointFile); err != nil {
		return "", err
	}

	config, ok := runtimeConfigs[runtime]
	if !ok {
		// Fallback to Python for unknown runtimes
		config = runtimeConfigs["python3.11"]
	}

	var dockerfile strings.Builder

	// Base image
	dockerfile.WriteString(fmt.Sprintf("FROM %s\n\n", config.BaseImage))

	// Set working directory
	dockerfile.WriteString("WORKDIR /bot\n\n")

	// Copy all bot code
	dockerfile.WriteString("# Copy bot code\n")
	dockerfile.WriteString("COPY . /bot/\n\n")

	// Install dependencies (only for interpreted languages - compiled languages are pre-built by CLI)
	if config.InstallCommand != "" && config.DependencyFile != "" {
		dockerfile.WriteString("# Install dependencies (if dependency file exists)\n")
		dockerfile.WriteString(fmt.Sprintf("RUN if [ -f \"%s\" ]; then %s; fi\n\n",
			config.DependencyFile, config.InstallCommand))
	} else {
		dockerfile.WriteString("# Pre-built by CLI, no dependency installation needed\n\n")
	}

	// Set entrypoint file as environment variable
	dockerfile.WriteString("# Set entrypoint configuration\n")
	dockerfile.WriteString(fmt.Sprintf("ENV SCRIPT_PATH=%s\n", entrypointFile))
	dockerfile.WriteString("ENV CODE_MOUNT_DIR=bot\n\n")

	// Set entrypoint command - use /bin/sh for Alpine-based images
	dockerfile.WriteString("# Run the bot\n")
	if runtime == "nodejs20" {
		dockerfile.WriteString("CMD [\"/bin/sh\", \"/bot/entrypoint.sh\"]\n")
	} else {
		dockerfile.WriteString("CMD [\"/bin/bash\", \"/bot/entrypoint.sh\"]\n")
	}

	return dockerfile.String(), nil
}

// GenerateEntrypointScript generates the bash entrypoint script that wraps
// the runtime-specific entrypoint. This script is included in the container
// image alongside the bot code.
func (g *DockerfileGenerator) GenerateEntrypointScript(runtime, entrypointFile string) (string, error) {
	// Validate entrypoint file to prevent command injection
	if err := validateEntrypointFile(entrypointFile); err != nil {
		return "", err
	}

	switch runtime {
	case "python3.11":
		return g.generatePythonEntrypoint(entrypointFile), nil
	case "nodejs20":
		return g.generateNodeJSEntrypoint(entrypointFile), nil
	case "rust-stable":
		return g.generateRustEntrypoint(entrypointFile), nil
	case "dotnet8":
		return g.generateDotNetEntrypoint(entrypointFile), nil
	case "gcc13":
		return g.generateCppEntrypoint(entrypointFile), nil
	case "scala3":
		return g.generateScalaEntrypoint(entrypointFile), nil
	case "ghc96":
		return g.generateHaskellEntrypoint(entrypointFile), nil
	default:
		// Fallback to Python
		return g.generatePythonEntrypoint(entrypointFile), nil
	}
}

func (g *DockerfileGenerator) generatePythonEntrypoint(entrypointFile string) string {
	return fmt.Sprintf(`#!/bin/bash
set -e

# Python bot entrypoint
cd /bot

# Install dependencies if requirements.txt exists
if [ -f "requirements.txt" ]; then
    pip install --no-cache-dir -r requirements.txt 2>/dev/null || true
fi

# Run the Python bot
exec python3 %s
`, entrypointFile)
}

func (g *DockerfileGenerator) generateNodeJSEntrypoint(entrypointFile string) string {
	return fmt.Sprintf(`#!/bin/sh
set -e

# Node.js bot entrypoint
cd /bot

# Install dependencies if package.json exists
if [ -f "package.json" ]; then
    npm install --production 2>/dev/null || true
fi

# Run the Node.js bot
exec node %s
`, entrypointFile)
}

func (g *DockerfileGenerator) generateRustEntrypoint(entrypointFile string) string {
	return fmt.Sprintf(`#!/bin/bash
set -e

# Rust bot entrypoint (pre-built by CLI)
cd /bot

# Run the pre-built binary
exec ./%s
`, entrypointFile)
}

func (g *DockerfileGenerator) generateDotNetEntrypoint(entrypointFile string) string {
	return fmt.Sprintf(`#!/bin/bash
set -e

# .NET bot entrypoint (pre-built by CLI)
cd /bot

# Run the pre-built binary
exec ./%s
`, entrypointFile)
}

func (g *DockerfileGenerator) generateCppEntrypoint(entrypointFile string) string {
	return fmt.Sprintf(`#!/bin/bash
set -e

# C++ bot entrypoint (pre-built by CLI)
cd /bot

# Run the pre-built binary
exec ./%s
`, entrypointFile)
}

func (g *DockerfileGenerator) generateScalaEntrypoint(entrypointFile string) string {
	return fmt.Sprintf(`#!/bin/bash
set -e

# Scala bot entrypoint (pre-built by CLI)
cd /bot

# Run the pre-built JAR
exec java -jar %s
`, entrypointFile)
}

func (g *DockerfileGenerator) generateHaskellEntrypoint(entrypointFile string) string {
	return fmt.Sprintf(`#!/bin/bash
set -e

# Haskell bot entrypoint (pre-built by CLI)
cd /bot

# Run the pre-built binary
exec ./%s
`, entrypointFile)
}

// GetBaseImage returns the base Docker image for a given runtime.
func GetBaseImage(runtime string) string {
	if config, ok := runtimeConfigs[runtime]; ok {
		return config.BaseImage
	}
	return runtimeConfigs["python3.11"].BaseImage
}

// GetSupportedRuntimes returns a list of supported runtime names.
func GetSupportedRuntimes() []string {
	runtimes := make([]string, 0, len(runtimeConfigs))
	for runtime := range runtimeConfigs {
		runtimes = append(runtimes, runtime)
	}
	return runtimes
}

// IsRuntimeSupported returns true if the given runtime is supported.
func IsRuntimeSupported(runtime string) bool {
	_, ok := runtimeConfigs[runtime]
	return ok
}
