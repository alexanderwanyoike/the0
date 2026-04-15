package internal

import (
	"encoding/json"
	"os"
	"path/filepath"
	"runtime"
	"testing"
	"time"
)

func withTempConfigDir(t *testing.T) string {
	t.Helper()
	dir := t.TempDir()
	prevCfg := configDirOverride
	prevOverride := activeEnvOverride
	prevEnv := os.Getenv("THE0_API_URL")
	SetConfigDir(dir)
	SetActiveEnvOverride("")
	os.Unsetenv("THE0_API_URL")
	t.Cleanup(func() {
		SetConfigDir(prevCfg)
		SetActiveEnvOverride(prevOverride)
		if prevEnv != "" {
			os.Setenv("THE0_API_URL", prevEnv)
		} else {
			os.Unsetenv("THE0_API_URL")
		}
	})
	return dir
}

func TestLoadEnvironments_FileMissing(t *testing.T) {
	withTempConfigDir(t)

	envs, err := LoadEnvironments()
	if err != nil {
		t.Fatalf("LoadEnvironments() with missing file returned error: %v", err)
	}
	if envs == nil {
		t.Fatal("LoadEnvironments() returned nil envs")
	}
	if envs.Active != "" {
		t.Errorf("expected empty Active, got %q", envs.Active)
	}
	if len(envs.Environments) != 0 {
		t.Errorf("expected empty Environments map, got %v", envs.Environments)
	}
}

func TestSaveAndLoadEnvironments_RoundTrip(t *testing.T) {
	dir := withTempConfigDir(t)

	original := &Environments{
		Active: "local",
		Environments: map[string]Environment{
			"local": {URL: "http://localhost:3000", APIKey: "the0_local", CreatedAt: time.Now().UTC().Truncate(time.Second)},
			"prod":  {URL: "https://api.the0.app", APIKey: "the0_prod", CreatedAt: time.Now().UTC().Truncate(time.Second)},
		},
	}

	if err := SaveEnvironments(original); err != nil {
		t.Fatalf("SaveEnvironments() error = %v", err)
	}

	path := filepath.Join(dir, "environments.json")
	info, err := os.Stat(path)
	if err != nil {
		t.Fatalf("stat on saved file: %v", err)
	}
	if runtime.GOOS != "windows" {
		if mode := info.Mode().Perm(); mode != 0600 {
			t.Errorf("expected file mode 0600, got %v", mode)
		}
	}

	loaded, err := LoadEnvironments()
	if err != nil {
		t.Fatalf("LoadEnvironments() error = %v", err)
	}
	if loaded.Active != original.Active {
		t.Errorf("Active = %q, want %q", loaded.Active, original.Active)
	}
	if len(loaded.Environments) != 2 {
		t.Fatalf("expected 2 environments, got %d", len(loaded.Environments))
	}
	if loaded.Environments["prod"].URL != "https://api.the0.app" {
		t.Errorf("prod URL mismatch: got %q", loaded.Environments["prod"].URL)
	}
	if loaded.Environments["prod"].APIKey != "the0_prod" {
		t.Errorf("prod APIKey mismatch: got %q", loaded.Environments["prod"].APIKey)
	}
}

func TestEnvironments_Add(t *testing.T) {
	envs := &Environments{Environments: map[string]Environment{}}

	err := envs.Add("local", Environment{URL: "http://localhost:3000", APIKey: "k"})
	if err != nil {
		t.Fatalf("Add() error = %v", err)
	}
	if _, ok := envs.Environments["local"]; !ok {
		t.Fatal("expected local env to be present")
	}
	if envs.Environments["local"].CreatedAt.IsZero() {
		t.Error("expected CreatedAt to be set by Add()")
	}
}

func TestEnvironments_Add_Duplicate(t *testing.T) {
	envs := &Environments{Environments: map[string]Environment{
		"local": {URL: "http://localhost:3000"},
	}}

	err := envs.Add("local", Environment{URL: "http://other"})
	if err == nil {
		t.Fatal("expected error on duplicate name, got nil")
	}
}

func TestEnvironments_Use(t *testing.T) {
	envs := &Environments{Environments: map[string]Environment{
		"local": {URL: "http://localhost:3000"},
		"prod":  {URL: "https://api.the0.app"},
	}}

	if err := envs.Use("prod"); err != nil {
		t.Fatalf("Use() error = %v", err)
	}
	if envs.Active != "prod" {
		t.Errorf("Active = %q, want prod", envs.Active)
	}
}

func TestEnvironments_Use_Unknown(t *testing.T) {
	envs := &Environments{Environments: map[string]Environment{}}
	if err := envs.Use("ghost"); err == nil {
		t.Fatal("expected error using unknown env")
	}
}

func TestEnvironments_Remove(t *testing.T) {
	envs := &Environments{
		Active: "local",
		Environments: map[string]Environment{
			"local": {URL: "http://localhost:3000"},
			"prod":  {URL: "https://api.the0.app"},
		},
	}

	if err := envs.Remove("prod"); err != nil {
		t.Fatalf("Remove() error = %v", err)
	}
	if _, ok := envs.Environments["prod"]; ok {
		t.Error("expected prod to be gone")
	}
	if envs.Active != "local" {
		t.Errorf("Active = %q, want local (unchanged)", envs.Active)
	}
}

func TestEnvironments_Remove_Active_UnsetsActive(t *testing.T) {
	envs := &Environments{
		Active: "local",
		Environments: map[string]Environment{
			"local": {URL: "http://localhost:3000"},
		},
	}

	if err := envs.Remove("local"); err != nil {
		t.Fatalf("Remove() error = %v", err)
	}
	if envs.Active != "" {
		t.Errorf("expected Active to be cleared, got %q", envs.Active)
	}
}

func TestEnvironments_Remove_Unknown(t *testing.T) {
	envs := &Environments{Environments: map[string]Environment{}}
	if err := envs.Remove("ghost"); err == nil {
		t.Fatal("expected error removing unknown env")
	}
}

func TestResolveActive_OverrideFlagWins(t *testing.T) {
	withTempConfigDir(t)

	envs := &Environments{
		Active: "local",
		Environments: map[string]Environment{
			"local": {URL: "http://localhost:3000", APIKey: "local_key"},
			"prod":  {URL: "https://api.the0.app", APIKey: "prod_key"},
		},
	}
	if err := SaveEnvironments(envs); err != nil {
		t.Fatalf("SaveEnvironments: %v", err)
	}

	got, err := ResolveActive("prod")
	if err != nil {
		t.Fatalf("ResolveActive(prod) error = %v", err)
	}
	if got.URL != "https://api.the0.app" || got.APIKey != "prod_key" {
		t.Errorf("ResolveActive(prod) = %+v", got)
	}
}

func TestResolveActive_OverrideFlag_Unknown(t *testing.T) {
	withTempConfigDir(t)
	envs := &Environments{Environments: map[string]Environment{}}
	_ = SaveEnvironments(envs)

	if _, err := ResolveActive("ghost"); err == nil {
		t.Fatal("expected error for unknown override")
	}
}

func TestResolveActive_GlobalOverride(t *testing.T) {
	withTempConfigDir(t)
	envs := &Environments{
		Active: "local",
		Environments: map[string]Environment{
			"local": {URL: "http://localhost:3000", APIKey: "local_key"},
			"prod":  {URL: "https://api.the0.app", APIKey: "prod_key"},
		},
	}
	_ = SaveEnvironments(envs)

	SetActiveEnvOverride("prod")
	got, err := ResolveActive("")
	if err != nil {
		t.Fatalf("ResolveActive error = %v", err)
	}
	if got.URL != "https://api.the0.app" {
		t.Errorf("expected prod URL, got %q", got.URL)
	}
}

func TestResolveActive_ActiveEnv(t *testing.T) {
	withTempConfigDir(t)
	envs := &Environments{
		Active: "local",
		Environments: map[string]Environment{
			"local": {URL: "http://localhost:3000", APIKey: "local_key"},
		},
	}
	_ = SaveEnvironments(envs)

	got, err := ResolveActive("")
	if err != nil {
		t.Fatalf("ResolveActive error = %v", err)
	}
	if got.URL != "http://localhost:3000" || got.APIKey != "local_key" {
		t.Errorf("ResolveActive = %+v", got)
	}
}

func TestResolveActive_LegacyAuthAndEnvVar(t *testing.T) {
	dir := withTempConfigDir(t)
	// No environments.json saved. Write legacy auth.json directly.
	auth := &Auth{APIKey: "legacy_key", CreatedAt: time.Now()}
	data, _ := json.MarshalIndent(auth, "", "  ")
	if err := os.WriteFile(filepath.Join(dir, "auth.json"), data, 0600); err != nil {
		t.Fatalf("write legacy auth.json: %v", err)
	}
	os.Setenv("THE0_API_URL", "http://legacy.example.com")

	got, err := ResolveActive("")
	if err != nil {
		t.Fatalf("ResolveActive error = %v", err)
	}
	if got.URL != "http://legacy.example.com" {
		t.Errorf("expected legacy URL, got %q", got.URL)
	}
	if got.APIKey != "legacy_key" {
		t.Errorf("expected legacy key, got %q", got.APIKey)
	}
}

func TestResolveActive_ActiveEnvMissingFromMap_FailsFast(t *testing.T) {
	withTempConfigDir(t)
	envs := &Environments{
		Active:       "ghost",
		Environments: map[string]Environment{},
	}
	if err := SaveEnvironments(envs); err != nil {
		t.Fatalf("SaveEnvironments: %v", err)
	}
	_, err := ResolveActive("")
	if err == nil {
		t.Fatal("expected error when active env is set but missing from map")
	}
}

func TestResolveActive_DefaultWhenEmpty(t *testing.T) {
	withTempConfigDir(t)
	got, err := ResolveActive("")
	if err != nil {
		t.Fatalf("ResolveActive error = %v", err)
	}
	if got.URL != DEFAULT_API_URL {
		t.Errorf("expected default URL, got %q", got.URL)
	}
	if got.APIKey != "" {
		t.Errorf("expected empty APIKey, got %q", got.APIKey)
	}
}

func TestMigrateLegacyAuth(t *testing.T) {
	dir := withTempConfigDir(t)
	auth := &Auth{APIKey: "legacy_key", CreatedAt: time.Now()}
	data, _ := json.MarshalIndent(auth, "", "  ")
	if err := os.WriteFile(filepath.Join(dir, "auth.json"), data, 0600); err != nil {
		t.Fatalf("write legacy auth.json: %v", err)
	}
	os.Setenv("THE0_API_URL", "http://legacy.example.com")

	envs, err := MigrateLegacyAuth()
	if err != nil {
		t.Fatalf("MigrateLegacyAuth error = %v", err)
	}
	if envs.Active != "default" {
		t.Errorf("expected Active=default, got %q", envs.Active)
	}
	dflt, ok := envs.Environments["default"]
	if !ok {
		t.Fatal("expected default env to be present")
	}
	if dflt.URL != "http://legacy.example.com" {
		t.Errorf("default URL = %q", dflt.URL)
	}
	if dflt.APIKey != "legacy_key" {
		t.Errorf("default APIKey = %q", dflt.APIKey)
	}

	// Verify it was persisted
	loaded, err := LoadEnvironments()
	if err != nil {
		t.Fatalf("LoadEnvironments after migrate: %v", err)
	}
	if loaded.Active != "default" {
		t.Errorf("persisted Active = %q", loaded.Active)
	}
}

func TestMigrateLegacyAuth_NoAuthFile(t *testing.T) {
	withTempConfigDir(t)
	// No legacy auth. Migration should still produce a valid empty/default env.
	envs, err := MigrateLegacyAuth()
	if err != nil {
		t.Fatalf("MigrateLegacyAuth error = %v", err)
	}
	if envs == nil {
		t.Fatal("expected non-nil envs")
	}
	// Either zero environments or a default with empty key is acceptable; we just
	// require it is save-loadable without error.
	loaded, err := LoadEnvironments()
	if err != nil {
		t.Fatalf("LoadEnvironments: %v", err)
	}
	if loaded == nil {
		t.Fatal("expected non-nil loaded envs")
	}
}
