package dev

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/fsnotify/fsnotify"
)

// WatchConfig describes what to monitor for a given runtime.
type WatchConfig struct {
	// IncludeExt is the set of file extensions (with the leading dot)
	// that trigger a restart. Empty means "any file".
	IncludeExt []string
	// IncludeNames is an exact-match set of filenames that trigger
	// restart even without a matching extension (e.g. package.json).
	IncludeNames []string
	// ExcludeDirs are directory names that are never descended into.
	ExcludeDirs []string
	// Debounce collapses bursts of events into a single restart signal.
	Debounce time.Duration
}

// WatchConfigFor returns the conventional watch set for a detected
// runtime. v1 supports Python and Node; other runtimes are rejected
// earlier in DetectRuntime so this function is never called for them.
func WatchConfigFor(rt Runtime) WatchConfig {
	base := []string{"node_modules", "dist", "build", "bin", ".the0", "vendor", "__pycache__", ".venv", "venv"}
	switch rt {
	case RuntimePython:
		return WatchConfig{
			IncludeExt:   []string{".py"},
			IncludeNames: []string{"requirements.txt", "pyproject.toml"},
			ExcludeDirs:  base,
			Debounce:     500 * time.Millisecond,
		}
	case RuntimeNode:
		return WatchConfig{
			IncludeExt:   []string{".js", ".ts", ".mjs"},
			IncludeNames: []string{"package.json", "package-lock.json"},
			ExcludeDirs:  base,
			Debounce:     500 * time.Millisecond,
		}
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
//
// Correctness notes:
//
//   - Newly-created directories are added recursively (via addRecursive),
//     not just the single directory path. This catches subtree creation
//     (e.g. `git checkout` of a branch with a new package dir); the prior
//     implementation called w.Add(ev.Name) directly and missed nested
//     dirs inside the new tree.
//
//   - The debouncer uses a single *time.Timer with Stop+drain+Reset on
//     each new event, and a dedicated select case on timer.C to fire
//     fn(). The prior implementation used time.AfterFunc which, when
//     Stopped mid-fire, could still deliver the callback AND schedule a
//     new one — producing back-to-back restarts for one save burst.
func Watcher(ctx context.Context, root string, cfg WatchConfig, fn func()) error {
	w, err := fsnotify.NewWatcher()
	if err != nil {
		return err
	}
	defer w.Close()

	if err := addRecursive(w, root, cfg.ExcludeDirs); err != nil {
		return err
	}

	// Single-timer debounce: create it stopped; Reset on each event;
	// consume on fire.
	timer := time.NewTimer(cfg.Debounce)
	if !timer.Stop() {
		<-timer.C
	}
	defer timer.Stop()

	for {
		select {
		case <-ctx.Done():
			return nil

		case ev, ok := <-w.Events:
			if !ok {
				return nil
			}
			if ev.Op&fsnotify.Create != 0 {
				if fi, err := os.Stat(ev.Name); err == nil && fi.IsDir() {
					if !isExcluded(ev.Name, cfg.ExcludeDirs) {
						_ = addRecursive(w, ev.Name, cfg.ExcludeDirs)
					}
				}
			}
			if !cfg.matches(ev.Name) {
				continue
			}
			if !timer.Stop() {
				select {
				case <-timer.C:
				default:
				}
			}
			timer.Reset(cfg.Debounce)

		case <-timer.C:
			fn()

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
