package runtime

// Default resource limits and requests for bot containers.
const (
	// DefaultMemoryLimit is the default memory limit for bot containers.
	DefaultMemoryLimit = "512Mi"

	// DefaultCPULimit is the default CPU limit for bot containers.
	DefaultCPULimit = "500m"

	// DefaultMemoryRequest is the default memory request for bot containers.
	DefaultMemoryRequest = "128Mi"

	// DefaultCPURequest is the default CPU request for bot containers.
	DefaultCPURequest = "100m"
)
