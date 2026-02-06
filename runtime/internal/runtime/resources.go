package runtime

// Default resource limits and requests for bot containers.
// Note: The Go runtime wrapper uses ~500MB base memory for MinIO client,
// fsnotify, and zip extraction. Limits must account for this overhead.
const (
	// DefaultMemoryLimit is the default memory limit for bot containers.
	// Increased from 512Mi to 768Mi to account for Go runtime overhead (~500MB base).
	DefaultMemoryLimit = "768Mi"

	// DefaultCPULimit is the default CPU limit for bot containers.
	DefaultCPULimit = "500m"

	// DefaultMemoryRequest is the default memory request for bot containers.
	DefaultMemoryRequest = "256Mi"

	// DefaultCPURequest is the default CPU request for bot containers.
	DefaultCPURequest = "100m"
)
