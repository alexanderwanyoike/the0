package dev

import (
	"context"
	"os"
	"path/filepath"
	"sync/atomic"
	"testing"
	"time"
)

// waitFor polls cond at 5ms intervals until it returns true or timeout
// elapses. Replaces fixed time.Sleep calls which were flaky on loaded
// CI runners.
func waitFor(t *testing.T, cond func() bool, timeout time.Duration, msg string) {
	t.Helper()
	deadline := time.Now().Add(timeout)
	for time.Now().Before(deadline) {
		if cond() {
			return
		}
		time.Sleep(5 * time.Millisecond)
	}
	t.Fatalf("condition not met within %v: %s", timeout, msg)
}

// neverBecomes asserts that cond stays false for the full duration.
func neverBecomes(t *testing.T, cond func() bool, duration time.Duration, msg string) {
	t.Helper()
	deadline := time.Now().Add(duration)
	for time.Now().Before(deadline) {
		if cond() {
			t.Fatalf("condition became true unexpectedly: %s", msg)
		}
		time.Sleep(5 * time.Millisecond)
	}
}

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
	// Node's WatchConfigFor includes package.json as a named file that
	// triggers restart even though .json is not in IncludeExt.
	cfg := WatchConfigFor(RuntimeNode)
	if !cfg.matches("package.json") {
		t.Error("package.json should match even without .json in IncludeExt")
	}
}

func TestWatchConfig_Exclude(t *testing.T) {
	cfg := WatchConfigFor(RuntimePython)
	if !isExcluded("/proj/node_modules", cfg.ExcludeDirs) {
		t.Error("node_modules should be excluded")
	}
}

// TestWatcher_DebouncesAndFires verifies that a burst of file writes
// collapses into a single fire, and that a second burst after a quiet
// period produces a second fire. Uses polling with errCh capture to
// avoid flakiness under load.
func TestWatcher_DebouncesAndFires(t *testing.T) {
	dir := t.TempDir()
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	var fires int32
	errCh := make(chan error, 1)
	go func() {
		errCh <- Watcher(ctx, dir, WatchConfig{
			IncludeExt: []string{".py"},
			Debounce:   50 * time.Millisecond,
		}, func() { atomic.AddInt32(&fires, 1) })
	}()

	// Give fsnotify a moment to attach.
	time.Sleep(30 * time.Millisecond)

	// Burst 1: three files, one expected fire.
	for _, name := range []string{"a.py", "b.py", "c.py"} {
		if err := os.WriteFile(filepath.Join(dir, name), []byte("x"), 0644); err != nil {
			t.Fatal(err)
		}
	}
	waitFor(t, func() bool { return atomic.LoadInt32(&fires) == 1 }, time.Second, "first burst -> 1 fire")
	neverBecomes(t, func() bool { return atomic.LoadInt32(&fires) > 1 }, 100*time.Millisecond, "single burst must not double-fire")

	// Burst 2: another write after a quiet period.
	if err := os.WriteFile(filepath.Join(dir, "d.py"), []byte("y"), 0644); err != nil {
		t.Fatal(err)
	}
	waitFor(t, func() bool { return atomic.LoadInt32(&fires) == 2 }, time.Second, "second burst -> 2 fires")

	cancel()
	if err := <-errCh; err != nil {
		t.Errorf("Watcher returned error: %v", err)
	}
}

// TestWatcher_IgnoresUnrelatedFiles asserts that writes to files
// outside IncludeExt don't trigger a fire even after the debounce
// window elapses.
func TestWatcher_IgnoresUnrelatedFiles(t *testing.T) {
	dir := t.TempDir()
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	var fires int32
	errCh := make(chan error, 1)
	go func() {
		errCh <- Watcher(ctx, dir, WatchConfig{
			IncludeExt: []string{".py"},
			Debounce:   50 * time.Millisecond,
		}, func() { atomic.AddInt32(&fires, 1) })
	}()
	time.Sleep(30 * time.Millisecond)

	if err := os.WriteFile(filepath.Join(dir, "README.md"), []byte("x"), 0644); err != nil {
		t.Fatal(err)
	}
	neverBecomes(t, func() bool { return atomic.LoadInt32(&fires) > 0 }, 200*time.Millisecond, "unrelated file must not fire")

	cancel()
	if err := <-errCh; err != nil {
		t.Errorf("Watcher returned error: %v", err)
	}
}

// TestWatcher_NewSubdirIsRecursivelyWatched is the regression guard for
// the recursive-add bug: when a directory is created, nested edits
// inside it must also trigger a fire.
func TestWatcher_NewSubdirIsRecursivelyWatched(t *testing.T) {
	dir := t.TempDir()
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	var fires int32
	errCh := make(chan error, 1)
	go func() {
		errCh <- Watcher(ctx, dir, WatchConfig{
			IncludeExt: []string{".py"},
			Debounce:   50 * time.Millisecond,
		}, func() { atomic.AddInt32(&fires, 1) })
	}()
	time.Sleep(30 * time.Millisecond)

	// Create a nested subtree. Before the fix, w.Add would only watch
	// `sub` but not `sub/inner`, and an edit below would be missed.
	if err := os.MkdirAll(filepath.Join(dir, "sub", "inner"), 0755); err != nil {
		t.Fatal(err)
	}
	// Give fsnotify time to observe the directory creation + recursion.
	time.Sleep(80 * time.Millisecond)

	if err := os.WriteFile(filepath.Join(dir, "sub", "inner", "nested.py"), []byte("x"), 0644); err != nil {
		t.Fatal(err)
	}
	waitFor(t, func() bool { return atomic.LoadInt32(&fires) >= 1 }, time.Second, "edit in newly-created nested dir must fire")

	cancel()
	if err := <-errCh; err != nil {
		t.Errorf("Watcher returned error: %v", err)
	}
}
