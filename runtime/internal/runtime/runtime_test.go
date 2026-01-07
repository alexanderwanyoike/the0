package runtime

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestGetDockerImage(t *testing.T) {
	// All valid runtimes now return the universal runtime image
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
		t.Run(rt, func(t *testing.T) {
			result, err := GetDockerImage(rt)
			assert.NoError(t, err)
			assert.Equal(t, UniversalRuntimeImage, result)
		})
	}

	// Test invalid runtimes return error
	invalidRuntimes := []string{"unknown", "python3.10", ""}
	for _, rt := range invalidRuntimes {
		name := rt
		if name == "" {
			name = "empty"
		}
		t.Run(name+"_invalid", func(t *testing.T) {
			_, err := GetDockerImage(rt)
			assert.Error(t, err)
			assert.Contains(t, err.Error(), "unsupported runtime")
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

	// Verify expected count - all runtimes in runtimeImages
	expectedRuntimes := []string{
		"cpp-gcc13",
		"dotnet8",
		"gcc13",
		"ghc96",
		"nodejs20",
		"python3.11",
		"rust-stable",
		"scala3",
	}
	assert.Len(t, runtimes, len(expectedRuntimes))

	// Verify all expected runtimes are present
	for _, expected := range expectedRuntimes {
		assert.Contains(t, runtimes, expected, "missing runtime: %s", expected)
	}

	// Verify SupportedRuntimes is consistent with IsValidRuntime
	for _, rt := range runtimes {
		assert.True(t, IsValidRuntime(rt), "SupportedRuntimes contains %s but IsValidRuntime returns false", rt)
	}
}
