package cmd

import (
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"the0/internal"
)

func newMockAPI(t *testing.T, validKey string) *httptest.Server {
	t.Helper()
	return httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path != "/auth/validate-api-key" {
			http.NotFound(w, r)
			return
		}
		auth := r.Header.Get("Authorization")
		if auth != "ApiKey "+validKey {
			w.WriteHeader(http.StatusUnauthorized)
			return
		}
		w.Header().Set("Content-Type", "application/json")
		_ = json.NewEncoder(w).Encode(map[string]any{
			"success": true,
			"data":    map[string]any{"valid": true, "userId": "u1", "keyId": "k1"},
		})
	}))
}

func TestAddEnvironment_ValidKey(t *testing.T) {
	internal.SetConfigDir(t.TempDir())
	t.Cleanup(func() { internal.SetConfigDir("") })

	srv := newMockAPI(t, "good_key")
	defer srv.Close()

	if err := addEnvironment("local", srv.URL, "good_key"); err != nil {
		t.Fatalf("addEnvironment() error = %v", err)
	}

	envs, err := internal.LoadEnvironments()
	if err != nil {
		t.Fatalf("LoadEnvironments: %v", err)
	}
	if envs.Active != "local" {
		t.Errorf("Active = %q, want local", envs.Active)
	}
	got, ok := envs.Environments["local"]
	if !ok {
		t.Fatal("local env not persisted")
	}
	if got.APIKey != "good_key" {
		t.Errorf("APIKey = %q", got.APIKey)
	}
	if got.URL != strings.TrimRight(srv.URL, "/") {
		t.Errorf("URL = %q, want %q", got.URL, srv.URL)
	}
}

func TestAddEnvironment_InvalidKey_NothingSaved(t *testing.T) {
	internal.SetConfigDir(t.TempDir())
	t.Cleanup(func() { internal.SetConfigDir("") })

	srv := newMockAPI(t, "good_key")
	defer srv.Close()

	err := addEnvironment("local", srv.URL, "wrong_key")
	if err == nil {
		t.Fatal("expected validation error for bad key")
	}

	envs, loadErr := internal.LoadEnvironments()
	if loadErr != nil {
		t.Fatalf("LoadEnvironments: %v", loadErr)
	}
	if len(envs.Environments) != 0 {
		t.Errorf("expected no environments persisted, got %v", envs.Environments)
	}
	if envs.Active != "" {
		t.Errorf("expected empty Active, got %q", envs.Active)
	}
}

func TestAddEnvironment_NetworkError_NothingSaved(t *testing.T) {
	internal.SetConfigDir(t.TempDir())
	t.Cleanup(func() { internal.SetConfigDir("") })

	// Point at a URL nothing is listening on.
	err := addEnvironment("local", "http://127.0.0.1:1", "anything")
	if err == nil {
		t.Fatal("expected network error")
	}

	envs, _ := internal.LoadEnvironments()
	if len(envs.Environments) != 0 {
		t.Errorf("expected no environments persisted")
	}
}

func TestAddEnvironment_SecondEnv_KeepsFirst(t *testing.T) {
	internal.SetConfigDir(t.TempDir())
	t.Cleanup(func() { internal.SetConfigDir("") })

	srv := newMockAPI(t, "good_key")
	defer srv.Close()

	if err := addEnvironment("local", srv.URL, "good_key"); err != nil {
		t.Fatalf("first add: %v", err)
	}
	if err := addEnvironment("prod", srv.URL, "good_key"); err != nil {
		t.Fatalf("second add: %v", err)
	}

	envs, _ := internal.LoadEnvironments()
	if len(envs.Environments) != 2 {
		t.Fatalf("expected 2 envs, got %d", len(envs.Environments))
	}
	if envs.Active != "local" {
		t.Errorf("Active should remain 'local' after second add, got %q", envs.Active)
	}
}

func TestAddEnvironment_RemoveLastThenAdd_DoesNotReimportLegacy(t *testing.T) {
	dir := t.TempDir()
	internal.SetConfigDir(dir)
	t.Cleanup(func() { internal.SetConfigDir("") })

	// Plant a legacy auth.json that would, under the old buggy logic, be
	// re-imported every time the environments map is empty.
	if err := os.WriteFile(filepath.Join(dir, "auth.json"),
		[]byte(`{"api_key":"legacy_key","created_at":"2025-01-01T00:00:00Z"}`), 0600); err != nil {
		t.Fatalf("seed legacy auth.json: %v", err)
	}

	srv := newMockAPI(t, "good_key")
	defer srv.Close()

	// First add: legitimate migration of the legacy key into "default".
	if err := addEnvironment("local", srv.URL, "good_key"); err != nil {
		t.Fatalf("first add: %v", err)
	}

	// Remove every environment.
	envs, err := internal.LoadEnvironments()
	if err != nil {
		t.Fatalf("LoadEnvironments: %v", err)
	}
	for name := range envs.Environments {
		if err := envs.Remove(name); err != nil {
			t.Fatalf("remove %q: %v", name, err)
		}
	}
	if err := internal.SaveEnvironments(envs); err != nil {
		t.Fatalf("SaveEnvironments: %v", err)
	}

	// Now add again. The legacy auth.json must NOT be re-imported because
	// the user has already opted into named environments.
	if err := addEnvironment("staging", srv.URL, "good_key"); err != nil {
		t.Fatalf("second add: %v", err)
	}
	envs, _ = internal.LoadEnvironments()
	if _, ok := envs.Environments["default"]; ok {
		t.Errorf("legacy auth was re-imported after removing all envs; got default=%+v", envs.Environments["default"])
	}
	if len(envs.Environments) != 1 {
		t.Errorf("expected exactly 1 env (staging), got %d: %v", len(envs.Environments), envs.Environments)
	}
}

func TestAddEnvironment_Duplicate(t *testing.T) {
	internal.SetConfigDir(t.TempDir())
	t.Cleanup(func() { internal.SetConfigDir("") })

	srv := newMockAPI(t, "good_key")
	defer srv.Close()

	if err := addEnvironment("local", srv.URL, "good_key"); err != nil {
		t.Fatalf("first add: %v", err)
	}
	err := addEnvironment("local", srv.URL, "good_key")
	if err == nil {
		t.Fatal("expected duplicate error")
	}
}
