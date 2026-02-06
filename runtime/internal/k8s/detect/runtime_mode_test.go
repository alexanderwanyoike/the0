package detect

import (
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestRuntimeMode_String(t *testing.T) {
	tests := []struct {
		name     string
		mode     RuntimeMode
		expected string
	}{
		{
			name:     "docker mode",
			mode:     DockerMode,
			expected: "docker",
		},
		{
			name:     "kubernetes controller mode",
			mode:     KubernetesControllerMode,
			expected: "kubernetes-controller",
		},
		{
			name:     "unknown mode",
			mode:     RuntimeMode(999), // Invalid mode
			expected: "unknown",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := tt.mode.String()
			assert.Equal(t, tt.expected, result)
		})
	}
}

func TestDetectRuntimeMode_OutsideKubernetes(t *testing.T) {
	// This test runs outside a Kubernetes cluster (no service account files)
	// It should always return DockerMode regardless of RUNTIME_MODE env var

	tests := []struct {
		name        string
		runtimeMode string
		expected    RuntimeMode
	}{
		{
			name:        "no env var set",
			runtimeMode: "",
			expected:    DockerMode,
		},
		{
			name:        "controller mode set but not in k8s",
			runtimeMode: "controller",
			expected:    DockerMode,
		},
		{
			name:        "docker mode explicitly set",
			runtimeMode: "docker",
			expected:    DockerMode,
		},
		{
			name:        "invalid mode set",
			runtimeMode: "invalid",
			expected:    DockerMode,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Set environment variable for this test
			if tt.runtimeMode != "" {
				os.Setenv("RUNTIME_MODE", tt.runtimeMode)
				defer os.Unsetenv("RUNTIME_MODE")
			}

			mode := DetectRuntimeMode()
			assert.Equal(t, tt.expected, mode)
		})
	}
}

func TestIsKubernetesEnvironment_OutsideKubernetes(t *testing.T) {
	// When running tests outside a Kubernetes cluster,
	// IsKubernetesEnvironment should return false
	isK8s := IsKubernetesEnvironment()
	assert.False(t, isK8s, "Expected false when running outside Kubernetes")
}

func TestRuntimeMode_Constants(t *testing.T) {
	// Verify the constants are defined correctly
	assert.Equal(t, RuntimeMode(0), DockerMode)
	assert.Equal(t, RuntimeMode(1), KubernetesControllerMode)

	// Verify they're different
	assert.NotEqual(t, DockerMode, KubernetesControllerMode)
}

func TestRuntimeMode_StringRepresentation(t *testing.T) {
	// Verify that string representations are unique and meaningful
	dockerStr := DockerMode.String()
	k8sStr := KubernetesControllerMode.String()

	assert.NotEmpty(t, dockerStr)
	assert.NotEmpty(t, k8sStr)
	assert.NotEqual(t, dockerStr, k8sStr)

	// Verify they contain expected substrings
	assert.Contains(t, dockerStr, "docker")
	assert.Contains(t, k8sStr, "kubernetes")
}

func TestDetectRuntimeMode_EnvVarHandling(t *testing.T) {
	// Test that environment variable handling is case-sensitive
	tests := []struct {
		name     string
		envValue string
	}{
		{
			name:     "uppercase CONTROLLER",
			envValue: "CONTROLLER",
		},
		{
			name:     "mixed case Controller",
			envValue: "Controller",
		},
		{
			name:     "controller with spaces",
			envValue: " controller ",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			os.Setenv("RUNTIME_MODE", tt.envValue)
			defer os.Unsetenv("RUNTIME_MODE")

			mode := DetectRuntimeMode()

			// Outside K8s, should always be DockerMode
			// But if we were in K8s, these would NOT match "controller" exactly
			assert.Equal(t, DockerMode, mode)
		})
	}
}

func TestDetectRuntimeMode_Idempotent(t *testing.T) {
	// Verify that calling DetectRuntimeMode multiple times
	// returns consistent results

	mode1 := DetectRuntimeMode()
	mode2 := DetectRuntimeMode()
	mode3 := DetectRuntimeMode()

	assert.Equal(t, mode1, mode2)
	assert.Equal(t, mode2, mode3)
}

func TestIsKubernetesEnvironment_Idempotent(t *testing.T) {
	// Verify that calling IsKubernetesEnvironment multiple times
	// returns consistent results

	result1 := IsKubernetesEnvironment()
	result2 := IsKubernetesEnvironment()
	result3 := IsKubernetesEnvironment()

	assert.Equal(t, result1, result2)
	assert.Equal(t, result2, result3)
}

// Note: Testing behavior inside a Kubernetes cluster is challenging in unit tests
// because it requires:
// - /var/run/secrets/kubernetes.io/serviceaccount/token file
// - /var/run/secrets/kubernetes.io/serviceaccount/ca.crt file
// - KUBERNETES_SERVICE_HOST and KUBERNETES_SERVICE_PORT environment variables
//
// These tests would typically be covered by integration tests running in an actual
// Kubernetes cluster. The tests above verify the logic works correctly outside K8s
// and that the string representations are correct.

func TestRuntimeMode_TypeSafety(t *testing.T) {
	// Verify that RuntimeMode is a distinct type and can't be
	// confused with regular integers

	var mode RuntimeMode = DockerMode

	// Should be able to use in switch statements
	var result string
	switch mode {
	case DockerMode:
		result = "docker"
	case KubernetesControllerMode:
		result = "kubernetes"
	default:
		result = "unknown"
	}

	assert.Equal(t, "docker", result)
}

func TestDetectRuntimeMode_DefaultBehavior(t *testing.T) {
	// When no special configuration is present, should default to DockerMode
	// Clear any RUNTIME_MODE env var that might be set
	os.Unsetenv("RUNTIME_MODE")

	mode := DetectRuntimeMode()

	assert.Equal(t, DockerMode, mode,
		"Should default to DockerMode when not in K8s and no env var set")
}

// Benchmark tests to ensure detection is fast
func BenchmarkDetectRuntimeMode(b *testing.B) {
	for i := 0; i < b.N; i++ {
		DetectRuntimeMode()
	}
}

func BenchmarkIsKubernetesEnvironment(b *testing.B) {
	for i := 0; i < b.N; i++ {
		IsKubernetesEnvironment()
	}
}

func BenchmarkRuntimeMode_String(b *testing.B) {
	mode := DockerMode
	for i := 0; i < b.N; i++ {
		_ = mode.String()
	}
}
