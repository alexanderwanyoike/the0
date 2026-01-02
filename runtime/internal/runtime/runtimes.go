package runtime

// SupportedRuntimes returns the list of all supported runtime identifiers.
func SupportedRuntimes() []string {
	return []string{
		"python3.11",
		"nodejs20",
		"rust-stable",
		"dotnet8",
		"gcc13",
		"cpp-gcc13",
		"scala3",
		"ghc96",
	}
}

// IsValidRuntime returns true if the runtime is supported.
func IsValidRuntime(runtime string) bool {
	_, ok := runtimeImages[runtime]
	return ok
}
