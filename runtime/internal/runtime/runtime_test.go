package runtime

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestGetDockerImage(t *testing.T) {
	tests := []struct {
		runtime  string
		expected string
	}{
		{"python3.11", "python:3.11-slim"},
		{"nodejs20", "node:20-alpine"},
		{"rust-stable", "rust:latest"},
		{"dotnet8", "mcr.microsoft.com/dotnet/runtime:8.0"},
		{"gcc13", "gcc:13"},
		{"cpp-gcc13", "gcc:13"},
		{"scala3", "eclipse-temurin:21-jre"},
		{"ghc96", "haskell:9.6-slim"},
		{"unknown", "python:3.11-slim"}, // fallback
	}

	for _, tt := range tests {
		t.Run(tt.runtime, func(t *testing.T) {
			result := GetDockerImage(tt.runtime)
			assert.Equal(t, tt.expected, result)
		})
	}
}

func TestIsValidRuntime(t *testing.T) {
	validRuntimes := []string{
		"python3.11",
		"nodejs20",
		"rust-stable",
		"dotnet8",
		"gcc13",
		"cpp-gcc13",
		"scala3",
		"ghc96",
	}

	for _, rt := range validRuntimes {
		t.Run(rt+"_valid", func(t *testing.T) {
			assert.True(t, IsValidRuntime(rt))
		})
	}

	invalidRuntimes := []string{
		"python3.10",
		"nodejs18",
		"unknown",
		"",
	}

	for _, rt := range invalidRuntimes {
		t.Run(rt+"_invalid", func(t *testing.T) {
			assert.False(t, IsValidRuntime(rt))
		})
	}
}

func TestSupportedRuntimes(t *testing.T) {
	runtimes := SupportedRuntimes()
	assert.NotEmpty(t, runtimes)
	assert.Contains(t, runtimes, "python3.11")
	assert.Contains(t, runtimes, "nodejs20")
}
