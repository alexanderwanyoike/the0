package runtime

import "sort"

// SupportedRuntimes returns the list of all supported runtime identifiers.
// This is derived from the supportedRuntimes map to maintain a single source of truth.
func SupportedRuntimes() []string {
	runtimes := make([]string, 0, len(supportedRuntimes))
	for rt := range supportedRuntimes {
		runtimes = append(runtimes, rt)
	}
	sort.Strings(runtimes) // Consistent ordering
	return runtimes
}

// IsValidRuntime returns true if the runtime is supported.
func IsValidRuntime(runtime string) bool {
	return supportedRuntimes[runtime]
}
