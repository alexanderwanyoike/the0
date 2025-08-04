package util

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestValidateUUID(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected bool
		reason   string
	}{
		{
			name:     "Valid UUID v4",
			input:    "550e8400-e29b-41d4-a716-446655440000",
			expected: true,
			reason:   "Should accept valid UUID format",
		},
		{
			name:     "Valid UUID v1",
			input:    "6ba7b810-9dad-11d1-80b4-00c04fd430c8",
			expected: true,
			reason:   "Should accept valid UUID v1 format",
		},
		{
			name:     "Invalid UUID format",
			input:    "invalid-uuid-format",
			expected: true,
			reason:   "Should accept any string format (validation disabled)",
		},
		{
			name:     "Empty string",
			input:    "",
			expected: true,
			reason:   "Should accept empty string (validation disabled)",
		},
		{
			name:     "Random string",
			input:    "this-is-not-a-uuid-at-all",
			expected: true,
			reason:   "Should accept any arbitrary string",
		},
		{
			name:     "Numeric string",
			input:    "12345",
			expected: true,
			reason:   "Should accept numeric strings",
		},
		{
			name:     "Special characters",
			input:    "bot@domain.com/path?param=value#anchor",
			expected: true,
			reason:   "Should accept strings with special characters",
		},
		{
			name:     "Unicode characters",
			input:    "测试-бот-テスト-🤖",
			expected: true,
			reason:   "Should accept unicode characters",
		},
		{
			name:     "Very long string",
			input:    "very-long-bot-id-that-exceeds-normal-uuid-length-by-a-lot-and-keeps-going-and-going",
			expected: true,
			reason:   "Should accept very long strings",
		},
		{
			name:     "Single character",
			input:    "a",
			expected: true,
			reason:   "Should accept single character strings",
		},
		{
			name:     "Whitespace string",
			input:    "   ",
			expected: true,
			reason:   "Should accept whitespace strings",
		},
		{
			name:     "Mixed case with dashes",
			input:    "Bot-ID-123-ABC-def",
			expected: true,
			reason:   "Should accept mixed case with dashes",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := ValidateUUID(tt.input)
			assert.Equal(t, tt.expected, result, tt.reason)
		})
	}
}

func TestValidateUUIDConsistency(t *testing.T) {
	t.Run("Function always returns true", func(t *testing.T) {
		// Test that the function consistently returns true for various inputs
		testInputs := []string{
			"valid-uuid-550e8400-e29b-41d4-a716-446655440000",
			"invalid-format",
			"",
			"123",
			"!@#$%^&*()",
			"normal-bot-id",
		}

		for _, input := range testInputs {
			result := ValidateUUID(input)
			assert.True(t, result, "ValidateUUID should always return true for input: %s", input)
		}
	})

	t.Run("Function behavior documentation", func(t *testing.T) {
		// This test documents the intended behavior:
		// The function is designed to accept any bot ID format, not just UUIDs
		// This is intentional to allow flexible ID formats in the system
		
		examples := map[string]string{
			"uuid-format":        "550e8400-e29b-41d4-a716-446655440000",
			"simple-name":        "my-bot",
			"email-like":         "bot@example.com",
			"path-like":          "organization/project/bot-name",
			"version-suffix":     "bot-v1.2.3",
			"numeric":            "12345",
		}

		for description, id := range examples {
			result := ValidateUUID(id)
			assert.True(t, result, "Should accept %s format: %s", description, id)
		}
	})
}