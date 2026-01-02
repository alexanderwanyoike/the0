package runtime

import "sort"

// SupportedRuntimes returns the list of all supported runtime identifiers.
// This is derived from the runtimeImages map to maintain a single source of truth.
func SupportedRuntimes() []string {
	runtimes := make([]string, 0, len(runtimeImages))
	for rt := range runtimeImages {
		runtimes = append(runtimes, rt)
	}
	sort.Strings(runtimes) // Consistent ordering
	return runtimes
}

// IsValidRuntime returns true if the runtime is supported.
func IsValidRuntime(runtime string) bool {
	_, ok := runtimeImages[runtime]
	return ok
}
