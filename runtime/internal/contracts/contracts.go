// Package contracts loads the examples in the repository's contracts/
// folder for tests. The api's tests load the same files, so a test here
// failing against one means the runtime no longer agrees with the api.
package contracts

import (
	"encoding/json"
	"os"
	"path/filepath"
	"testing"
)

// Dir returns the contracts folder, found from the test's working
// directory, which is the package under test at any depth below runtime/.
func Dir(t testing.TB) string {
	t.Helper()
	dir, err := os.Getwd()
	if err != nil {
		t.Fatalf("contracts: %v", err)
	}
	for {
		candidate := filepath.Join(dir, "contracts")
		if _, err := os.Stat(filepath.Join(candidate, "README.md")); err == nil {
			return candidate
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			t.Fatalf("contracts: no contracts/README.md above the working directory")
		}
		dir = parent
	}
}

// Read returns the raw bytes of a contract file, e.g. "events/bot-created.json".
func Read(t testing.TB, name string) []byte {
	t.Helper()
	content, err := os.ReadFile(filepath.Join(Dir(t), filepath.FromSlash(name)))
	if err != nil {
		t.Fatalf("contracts: %v", err)
	}
	return content
}

// Load decodes a contract file into v.
func Load(t testing.TB, name string, v any) {
	t.Helper()
	if err := json.Unmarshal(Read(t, name), v); err != nil {
		t.Fatalf("contracts: %s: %v", name, err)
	}
}
