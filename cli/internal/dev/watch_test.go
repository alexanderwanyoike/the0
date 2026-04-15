package dev

import (
	"context"
	"os"
	"path/filepath"
	"sync/atomic"
	"testing"
	"time"
)

func TestWatchConfig_MatchesExtension(t *testing.T) {
	cfg := WatchConfigFor(RuntimePython)
	if !cfg.matches("src/main.py") {
		t.Error(".py should match")
	}
	if cfg.matches("README.md") {
		t.Error(".md should not match")
	}
}

func TestWatchConfig_MatchesNamedFile(t *testing.T) {
	cfg := WatchConfigFor(RuntimeRust)
	if !cfg.matches("Cargo.toml") {
		t.Error("Cargo.toml should match even without .toml in IncludeExt")
	}
}

func TestWatchConfig_Exclude(t *testing.T) {
	cfg := WatchConfigFor(RuntimePython)
	if !isExcluded("/proj/node_modules", cfg.ExcludeDirs) {
		t.Error("node_modules should be excluded")
	}
}

func TestWatcher_DebouncesAndFires(t *testing.T) {
	dir := t.TempDir()
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	var fires int32
	go func() {
		_ = Watcher(ctx, dir, WatchConfig{
			IncludeExt: []string{".py"},
			Debounce:   50 * time.Millisecond,
		}, func() { atomic.AddInt32(&fires, 1) })
	}()

	// Let the watcher initialise.
	time.Sleep(30 * time.Millisecond)

	// Write three files quickly; debounce should collapse into one fire.
	for _, name := range []string{"a.py", "b.py", "c.py"} {
		if err := os.WriteFile(filepath.Join(dir, name), []byte("x"), 0644); err != nil {
			t.Fatal(err)
		}
	}
	// Wait long enough for the debounced timer to fire.
	time.Sleep(200 * time.Millisecond)

	if atomic.LoadInt32(&fires) != 1 {
		t.Errorf("fires = %d, want 1 (debounced)", atomic.LoadInt32(&fires))
	}

	// Another burst after quiet period should fire once more.
	if err := os.WriteFile(filepath.Join(dir, "d.py"), []byte("y"), 0644); err != nil {
		t.Fatal(err)
	}
	time.Sleep(200 * time.Millisecond)
	if atomic.LoadInt32(&fires) != 2 {
		t.Errorf("fires = %d, want 2 after second burst", atomic.LoadInt32(&fires))
	}
}

func TestWatcher_IgnoresUnrelatedFiles(t *testing.T) {
	dir := t.TempDir()
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	var fires int32
	go func() {
		_ = Watcher(ctx, dir, WatchConfig{
			IncludeExt: []string{".py"},
			Debounce:   50 * time.Millisecond,
		}, func() { atomic.AddInt32(&fires, 1) })
	}()
	time.Sleep(30 * time.Millisecond)

	if err := os.WriteFile(filepath.Join(dir, "README.md"), []byte("x"), 0644); err != nil {
		t.Fatal(err)
	}
	time.Sleep(150 * time.Millisecond)
	if atomic.LoadInt32(&fires) != 0 {
		t.Errorf("fires = %d, want 0", atomic.LoadInt32(&fires))
	}
}
