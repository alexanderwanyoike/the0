// Package detect provides runtime mode detection for the0 runtime.
// It determines whether the runtime should operate in Docker mode
// (master-worker architecture) or Kubernetes controller mode.
package detect

import (
	"os"

	"k8s.io/client-go/rest"
)

// RuntimeMode represents the execution mode of the runtime.
type RuntimeMode int

const (
	// DockerMode is the default mode using master-worker architecture
	// with Docker containers for bot execution.
	DockerMode RuntimeMode = iota

	// KubernetesControllerMode uses Kubernetes-native controllers
	// to manage bot Pods directly.
	KubernetesControllerMode
)

// String returns a human-readable name for the runtime mode.
func (m RuntimeMode) String() string {
	switch m {
	case DockerMode:
		return "docker"
	case KubernetesControllerMode:
		return "kubernetes-controller"
	default:
		return "unknown"
	}
}

// DetectRuntimeMode determines the appropriate runtime mode based on
// the environment. It checks:
// 1. If running inside a Kubernetes cluster (InClusterConfig available)
// 2. If RUNTIME_MODE environment variable is set to "controller"
//
// If both conditions are met, returns KubernetesControllerMode.
// Otherwise, returns DockerMode (the default).
func DetectRuntimeMode() RuntimeMode {
	// Check if we're running inside a Kubernetes cluster
	_, err := rest.InClusterConfig()
	if err != nil {
		// Not in K8s, use Docker mode
		return DockerMode
	}

	// Inside K8s - check if controller mode is explicitly requested
	mode := os.Getenv("RUNTIME_MODE")
	if mode == "controller" {
		return KubernetesControllerMode
	}

	// Inside K8s but not controller mode - still use Docker mode
	// (e.g., running as a worker in master-worker setup within K8s)
	return DockerMode
}

// IsKubernetesEnvironment returns true if running inside a Kubernetes cluster,
// regardless of the runtime mode.
func IsKubernetesEnvironment() bool {
	_, err := rest.InClusterConfig()
	return err == nil
}
