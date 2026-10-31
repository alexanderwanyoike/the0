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

// dir returns the contracts folder, found from the test's working
// directory, which is the package under test at any depth below runtime/.
func dir(t testing.TB) string {
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

// read returns the raw bytes of a contract file, e.g. "events/bot-created.json".
func read(t testing.TB, name string) []byte {
	t.Helper()
	content, err := os.ReadFile(filepath.Join(dir(t), filepath.FromSlash(name)))
	if err != nil {
		t.Fatalf("contracts: %v", err)
	}
	return content
}

// Load decodes a contract file into v.
func Load(t testing.TB, name string, v any) {
	t.Helper()
	if err := json.Unmarshal(read(t, name), v); err != nil {
		t.Fatalf("contracts: %s: %v", name, err)
	}
}

// BotIDs is bot-ids.json.
type BotIDs struct {
	Valid   []string `json:"valid"`
	Invalid []string `json:"invalid"`
}

// LogLines is log-lines.json without the api's entries.
type LogLines struct {
	BotID   string `json:"botId"`
	Subject string `json:"subject"`
	Chunk   struct {
		At        string `json:"at"`
		Raw       string `json:"raw"`
		Formatted string `json:"formatted"`
	} `json:"chunk"`
}

// StorageKey is one example of storage-keys.json; only log keys have At and
// Date.
type StorageKey struct {
	BotID string `json:"botId"`
	At    string `json:"at"`
	Date  string `json:"date"`
	Key   string `json:"key"`
}

// StorageKeys is storage-keys.json.
type StorageKeys struct {
	Logs  []StorageKey `json:"logs"`
	State []StorageKey `json:"state"`
}
