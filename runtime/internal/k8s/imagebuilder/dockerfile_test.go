package imagebuilder

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestDockerfileGenerator_GenerateDockerfile_Python(t *testing.T) {
	generator := NewDockerfileGenerator()

	dockerfile, err := generator.GenerateDockerfile("python3.11", "main.py")

	require.NoError(t, err)
	assert.Contains(t, dockerfile, "FROM python:3.11-slim")
	assert.Contains(t, dockerfile, "WORKDIR /bot")
	assert.Contains(t, dockerfile, "COPY . /bot/")
	assert.Contains(t, dockerfile, "requirements.txt")
	assert.Contains(t, dockerfile, "pip install")
	assert.Contains(t, dockerfile, "ENV SCRIPT_PATH=main.py")
	assert.Contains(t, dockerfile, "CMD [\"/bin/bash\", \"/bot/entrypoint.sh\"]")
}

func TestDockerfileGenerator_GenerateDockerfile_NodeJS(t *testing.T) {
	generator := NewDockerfileGenerator()

	dockerfile, err := generator.GenerateDockerfile("nodejs20", "index.js")

	require.NoError(t, err)
	assert.Contains(t, dockerfile, "FROM node:20-alpine")
	assert.Contains(t, dockerfile, "WORKDIR /bot")
	assert.Contains(t, dockerfile, "package.json")
	assert.Contains(t, dockerfile, "npm install")
	assert.Contains(t, dockerfile, "ENV SCRIPT_PATH=index.js")
}

func TestDockerfileGenerator_GenerateDockerfile_Rust(t *testing.T) {
	generator := NewDockerfileGenerator()

	dockerfile, err := generator.GenerateDockerfile("rust-stable", "sma_bot")

	require.NoError(t, err)
	assert.Contains(t, dockerfile, "FROM rust:latest")
	assert.Contains(t, dockerfile, "Cargo.toml")
	assert.Contains(t, dockerfile, "cargo build --release")
	assert.Contains(t, dockerfile, "ENV SCRIPT_PATH=sma_bot")
}

func TestDockerfileGenerator_GenerateDockerfile_DotNet(t *testing.T) {
	generator := NewDockerfileGenerator()

	dockerfile, err := generator.GenerateDockerfile("dotnet8", "SmaBot")

	require.NoError(t, err)
	assert.Contains(t, dockerfile, "FROM mcr.microsoft.com/dotnet/sdk:8.0")
	assert.Contains(t, dockerfile, "dotnet restore")
	assert.Contains(t, dockerfile, "dotnet build")
}

func TestDockerfileGenerator_GenerateDockerfile_UnknownRuntime(t *testing.T) {
	generator := NewDockerfileGenerator()

	// Unknown runtime should fall back to Python
	dockerfile, err := generator.GenerateDockerfile("unknown-runtime", "main.py")

	require.NoError(t, err)
	assert.Contains(t, dockerfile, "FROM python:3.11-slim")
}

func TestDockerfileGenerator_GenerateEntrypointScript_Python(t *testing.T) {
	generator := NewDockerfileGenerator()

	script, err := generator.GenerateEntrypointScript("python3.11", "main.py")

	require.NoError(t, err)
	assert.Contains(t, script, "#!/bin/bash")
	assert.Contains(t, script, "cd /bot")
	assert.Contains(t, script, "python3 main.py")
	assert.Contains(t, script, "requirements.txt")
}

func TestDockerfileGenerator_GenerateEntrypointScript_NodeJS(t *testing.T) {
	generator := NewDockerfileGenerator()

	script, err := generator.GenerateEntrypointScript("nodejs20", "index.js")

	require.NoError(t, err)
	assert.Contains(t, script, "#!/bin/bash")
	assert.Contains(t, script, "node index.js")
	assert.Contains(t, script, "npm install")
}

func TestDockerfileGenerator_GenerateEntrypointScript_Rust(t *testing.T) {
	generator := NewDockerfileGenerator()

	script, err := generator.GenerateEntrypointScript("rust-stable", "sma_bot")

	require.NoError(t, err)
	assert.Contains(t, script, "#!/bin/bash")
	assert.Contains(t, script, "cargo build --release")
	assert.Contains(t, script, "target/release/sma_bot")
}

func TestDockerfileGenerator_GenerateEntrypointScript_DotNet(t *testing.T) {
	generator := NewDockerfileGenerator()

	script, err := generator.GenerateEntrypointScript("dotnet8", "SmaBot")

	require.NoError(t, err)
	assert.Contains(t, script, "#!/bin/bash")
	assert.Contains(t, script, "dotnet run")
}

func TestDockerfileGenerator_GenerateEntrypointScript_Cpp(t *testing.T) {
	generator := NewDockerfileGenerator()

	script, err := generator.GenerateEntrypointScript("gcc13", "main")

	require.NoError(t, err)
	assert.Contains(t, script, "#!/bin/bash")
	assert.Contains(t, script, "make")
	assert.Contains(t, script, "./main")
}

func TestDockerfileGenerator_GenerateEntrypointScript_Scala(t *testing.T) {
	generator := NewDockerfileGenerator()

	script, err := generator.GenerateEntrypointScript("scala3", "Main")

	require.NoError(t, err)
	assert.Contains(t, script, "#!/bin/bash")
	assert.Contains(t, script, "sbt")
}

func TestDockerfileGenerator_GenerateEntrypointScript_Haskell(t *testing.T) {
	generator := NewDockerfileGenerator()

	script, err := generator.GenerateEntrypointScript("ghc96", "Main")

	require.NoError(t, err)
	assert.Contains(t, script, "#!/bin/bash")
	assert.Contains(t, script, "cabal")
}

func TestDockerfileGenerator_GenerateEntrypointScript_Unknown(t *testing.T) {
	generator := NewDockerfileGenerator()

	// Unknown runtime should fall back to Python
	script, err := generator.GenerateEntrypointScript("unknown", "main.py")

	require.NoError(t, err)
	assert.Contains(t, script, "python3 main.py")
}

func TestGetBaseImage(t *testing.T) {
	tests := []struct {
		runtime  string
		expected string
	}{
		{"python3.11", "python:3.11-slim"},
		{"nodejs20", "node:20-alpine"},
		{"rust-stable", "rust:latest"},
		{"dotnet8", "mcr.microsoft.com/dotnet/sdk:8.0"},
		{"gcc13", "gcc:13"},
		{"scala3", "eclipse-temurin:21-jre"},
		{"ghc96", "haskell:9.6-slim"},
		{"unknown", "python:3.11-slim"}, // fallback
	}

	for _, tt := range tests {
		t.Run(tt.runtime, func(t *testing.T) {
			result := GetBaseImage(tt.runtime)
			assert.Equal(t, tt.expected, result)
		})
	}
}

func TestGetSupportedRuntimes(t *testing.T) {
	runtimes := GetSupportedRuntimes()

	assert.Contains(t, runtimes, "python3.11")
	assert.Contains(t, runtimes, "nodejs20")
	assert.Contains(t, runtimes, "rust-stable")
	assert.Contains(t, runtimes, "dotnet8")
	assert.Contains(t, runtimes, "gcc13")
	assert.Contains(t, runtimes, "scala3")
	assert.Contains(t, runtimes, "ghc96")
}

func TestIsRuntimeSupported(t *testing.T) {
	assert.True(t, IsRuntimeSupported("python3.11"))
	assert.True(t, IsRuntimeSupported("nodejs20"))
	assert.True(t, IsRuntimeSupported("rust-stable"))
	assert.False(t, IsRuntimeSupported("unknown"))
	assert.False(t, IsRuntimeSupported(""))
}

func TestDockerfileGenerator_DockerfileStructure(t *testing.T) {
	generator := NewDockerfileGenerator()

	dockerfile, err := generator.GenerateDockerfile("python3.11", "main.py")
	require.NoError(t, err)

	lines := strings.Split(dockerfile, "\n")

	// First non-empty line should be FROM
	firstLine := ""
	for _, line := range lines {
		if strings.TrimSpace(line) != "" {
			firstLine = line
			break
		}
	}
	assert.True(t, strings.HasPrefix(firstLine, "FROM "), "Dockerfile should start with FROM")

	// Last non-empty line should be CMD or ENTRYPOINT
	lastLine := ""
	for i := len(lines) - 1; i >= 0; i-- {
		if strings.TrimSpace(lines[i]) != "" {
			lastLine = lines[i]
			break
		}
	}
	assert.True(t,
		strings.HasPrefix(lastLine, "CMD ") || strings.HasPrefix(lastLine, "ENTRYPOINT "),
		"Dockerfile should end with CMD or ENTRYPOINT")
}
