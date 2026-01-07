package main

import (
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestGetEnv_WithValue(t *testing.T) {
	os.Setenv("TEST_KEY", "test-value")
	defer os.Unsetenv("TEST_KEY")

	result := getEnv("TEST_KEY", "default")
	assert.Equal(t, "test-value", result)
}

func TestGetEnv_WithDefault(t *testing.T) {
	os.Unsetenv("NON_EXISTENT_KEY")

	result := getEnv("NON_EXISTENT_KEY", "default-value")
	assert.Equal(t, "default-value", result)
}

func TestGetEnvInt_WithValue(t *testing.T) {
	os.Setenv("TEST_INT_KEY", "42")
	defer os.Unsetenv("TEST_INT_KEY")

	result := getEnvInt("TEST_INT_KEY", 10)
	assert.Equal(t, 42, result)
}

func TestGetEnvInt_WithDefault(t *testing.T) {
	os.Unsetenv("NON_EXISTENT_INT_KEY")

	result := getEnvInt("NON_EXISTENT_INT_KEY", 99)
	assert.Equal(t, 99, result)
}

func TestGetEnvInt_InvalidValue(t *testing.T) {
	os.Setenv("INVALID_INT_KEY", "not-a-number")
	defer os.Unsetenv("INVALID_INT_KEY")

	result := getEnvInt("INVALID_INT_KEY", 50)
	// Should return default when parsing fails
	assert.Equal(t, 50, result)
}

func TestGetEnvInt_EmptyValue(t *testing.T) {
	os.Setenv("EMPTY_INT_KEY", "")
	defer os.Unsetenv("EMPTY_INT_KEY")

	result := getEnvInt("EMPTY_INT_KEY", 75)
	// Should return default when value is empty
	assert.Equal(t, 75, result)
}
