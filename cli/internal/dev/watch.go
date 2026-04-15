package dev

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/fsnotify/fsnotify"
)

// WatchConfig describes what to monitor for a given runtime. Each runtime
// picks a conservative set of globs — we'd rather watch one extra file than
// miss a change that actually matters.
type WatchConfig struct {
	// IncludeExt is the set of file extensions (with the leading dot) that
	// trigger a restart. Empty means "any file".
	IncludeExt []string
	// IncludeNames is an exact-match set of filenames that trigger restart
	// even without a matching extension (e.g. Cargo.toml, build.sbt).
	IncludeNames []string
	// ExcludeDirs are directory names that are never descended into.
	ExcludeDirs []string
	// Debounce collapses bursts of events into a single restart signal.
	Debounce time.Duration
}

// WatchConfigFor returns the conventional watch set for a detected runtime.
func WatchConfigFor(rt Runtime) WatchConfig {
	base := []string{"target", "node_modules", "dist", "build", "bin", "obj", ".the0", "vendor"}
	switch rt {
	case RuntimePython:
		return WatchConfig{IncludeExt: []string{".py"}, IncludeNames: []string{"requirements.txt", "pyproject.toml"}, ExcludeDirs: base, Debounce: 500 * time.Millisecond}
	case RuntimeNode:
		return WatchConfig{IncludeExt: []string{".js", ".ts", ".mjs"}, IncludeNames: []string{"package.json", "package-lock.json"}, ExcludeDirs: base, Debounce: 500 * time.Millisecond}
	case RuntimeRust:
		return WatchConfig{IncludeExt: []string{".rs"}, IncludeNames: []string{"Cargo.toml", "Cargo.lock"}, ExcludeDirs: base, Debounce: 500 * time.Millisecond}
	case RuntimeDotnet:
		return WatchConfig{IncludeExt: []string{".cs"}, IncludeNames: []string{}, ExcludeDirs: base, Debounce: 500 * time.Millisecond}
	case RuntimeCpp:
		return WatchConfig{IncludeExt: []string{".cpp", ".c", ".h", ".hpp"}, IncludeNames: []string{"CMakeLists.txt", "Makefile"}, ExcludeDirs: base, Debounce: 500 * time.Millisecond}
	case RuntimeScala:
		return WatchConfig{IncludeExt: []string{".scala"}, IncludeNames: []string{"build.sbt"}, ExcludeDirs: base, Debounce: 500 * time.Millisecond}
	case RuntimeHaskell:
		return WatchConfig{IncludeExt: []string{".hs"}, IncludeNames: []string{}, ExcludeDirs: base, Debounce: 500 * time.Millisecond}
	}
	return WatchConfig{ExcludeDirs: base, Debounce: 500 * time.Millisecond}
}

// matches reports whether a path should trigger a restart per this config.
func (c WatchConfig) matches(path string) bool {
	base := filepath.Base(path)
	for _, n := range c.IncludeNames {
		if base == n {
			return true
		}
	}
	if len(c.IncludeExt) == 0 {
		return true
	}
	ext := strings.ToLower(filepath.Ext(path))
	for _, e := range c.IncludeExt {
		if e == ext {
			return true
		}
	}
	return false
}

// Watcher calls fn once per debounced file-change burst until ctx is done.
func Watcher(ctx context.Context, root string, cfg WatchConfig, fn func()) error {
	w, err := fsnotify.NewWatcher()
	if err != nil {
		return err
	}
	defer w.Close()

	if err := addRecursive(w, root, cfg.ExcludeDirs); err != nil {
		return err
	}

	var timer *time.Timer
	for {
		select {
		case <-ctx.Done():
			return nil
		case ev, ok := <-w.Events:
			if !ok {
				return nil
			}
			// Newly-created directories need to be added to the watcher so
			// their contents are monitored too.
			if ev.Op&fsnotify.Create != 0 {
				if fi, err := os.Stat(ev.Name); err == nil && fi.IsDir() {
					if !isExcluded(ev.Name, cfg.ExcludeDirs) {
						_ = w.Add(ev.Name)
					}
				}
			}
			if !cfg.matches(ev.Name) {
				continue
			}
			if timer != nil {
				timer.Stop()
			}
			timer = time.AfterFunc(cfg.Debounce, fn)
		case err, ok := <-w.Errors:
			if !ok {
				return nil
			}
			if err != nil {
				return err
			}
		}
	}
}

func addRecursive(w *fsnotify.Watcher, root string, exclude []string) error {
	return filepath.Walk(root, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return nil // tolerate transient errors during walk
		}
		if !info.IsDir() {
			return nil
		}
		if isExcluded(path, exclude) {
			return filepath.SkipDir
		}
		return w.Add(path)
	})
}

func isExcluded(path string, exclude []string) bool {
	base := filepath.Base(path)
	for _, x := range exclude {
		if base == x {
			return true
		}
	}
	return false
}
