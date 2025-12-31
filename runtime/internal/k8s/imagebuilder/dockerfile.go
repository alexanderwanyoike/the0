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
		BaseImage:         "rust:latest",
		InstallCommand:    "cargo build --release",
		EntrypointCommand: "/bin/bash /bot/entrypoint.sh",
		DependencyFile:    "Cargo.toml",
	},
	"dotnet8": {
		BaseImage:         "mcr.microsoft.com/dotnet/sdk:8.0",
		InstallCommand:    "dotnet restore && dotnet build -c Release",
		EntrypointCommand: "/bin/bash /bot/entrypoint.sh",
		DependencyFile:    "project.csproj", // Will use glob pattern in Dockerfile
	},
	"gcc13": {
		BaseImage:         "gcc:13",
		InstallCommand:    "make build || cmake . && make",
		EntrypointCommand: "/bin/bash /bot/entrypoint.sh",
		DependencyFile:    "Makefile",
	},
	"scala3": {
		BaseImage:         "eclipse-temurin:21-jre",
		InstallCommand:    "sbt compile",
		EntrypointCommand: "/bin/bash /bot/entrypoint.sh",
		DependencyFile:    "build.sbt",
	},
	"ghc96": {
		BaseImage:         "haskell:9.6-slim",
		InstallCommand:    "cabal update && cabal build",
		EntrypointCommand: "/bin/bash /bot/entrypoint.sh",
		DependencyFile:    "package.cabal", // Will use glob pattern in Dockerfile
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

	// Install dependencies (if dependency file exists)
	// Use glob-safe check for runtimes that use glob patterns
	dockerfile.WriteString("# Install dependencies (if dependency file exists)\n")
	switch runtime {
	case "dotnet8":
		// Use shell glob expansion for .csproj files
		dockerfile.WriteString(fmt.Sprintf("RUN if ls *.csproj 1>/dev/null 2>&1; then %s; fi\n\n", config.InstallCommand))
	case "ghc96":
		// Use shell glob expansion for .cabal files
		dockerfile.WriteString(fmt.Sprintf("RUN if ls *.cabal 1>/dev/null 2>&1; then %s; fi\n\n", config.InstallCommand))
	default:
		dockerfile.WriteString(fmt.Sprintf("RUN if [ -f \"%s\" ]; then %s; fi\n\n",
			config.DependencyFile, config.InstallCommand))
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

# Rust bot entrypoint
cd /bot

# Build if not already built
if [ -f "Cargo.toml" ] && [ ! -f "target/release/%s" ]; then
    cargo build --release 2>/dev/null || true
fi

# Run the Rust binary
if [ -f "target/release/%s" ]; then
    exec ./target/release/%s
else
    # Fallback: try to run main.rs directly with cargo run
    exec cargo run --release
fi
`, entrypointFile, entrypointFile, entrypointFile)
}

func (g *DockerfileGenerator) generateDotNetEntrypoint(entrypointFile string) string {
	return `#!/bin/bash
set -e

# .NET bot entrypoint
cd /bot

# Restore and build if not already done
if [ -f "*.csproj" ] || [ -f "*.sln" ]; then
    dotnet restore 2>/dev/null || true
    dotnet build -c Release 2>/dev/null || true
fi

# Run the .NET project
exec dotnet run -c Release
`
}

func (g *DockerfileGenerator) generateCppEntrypoint(entrypointFile string) string {
	return fmt.Sprintf(`#!/bin/bash
set -e

# C++ bot entrypoint
cd /bot

# Build if Makefile exists
if [ -f "Makefile" ]; then
    make build 2>/dev/null || make 2>/dev/null || true
fi

# Try to run the binary
if [ -f "./%s" ]; then
    exec ./%s
elif [ -f "./build/%s" ]; then
    exec ./build/%s
else
    # Fallback: try main
    exec ./main || ./a.out
fi
`, entrypointFile, entrypointFile, entrypointFile, entrypointFile)
}

func (g *DockerfileGenerator) generateScalaEntrypoint(entrypointFile string) string {
	return `#!/bin/bash
set -e

# Scala bot entrypoint
cd /bot

# Compile if build.sbt exists
if [ -f "build.sbt" ]; then
    sbt compile 2>/dev/null || true
fi

# Run via sbt
exec sbt run
`
}

func (g *DockerfileGenerator) generateHaskellEntrypoint(entrypointFile string) string {
	return `#!/bin/bash
set -e

# Haskell bot entrypoint
cd /bot

# Build if cabal file exists
if ls *.cabal 1>/dev/null 2>&1; then
    cabal update 2>/dev/null || true
    cabal build 2>/dev/null || true
fi

# Run via cabal
exec cabal run
`
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
