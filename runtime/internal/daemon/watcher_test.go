package daemon

import (
	"os"
	"path/filepath"
	"sync/atomic"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"runtime/internal/util"
)

func TestNewFileWatcher(t *testing.T) {
	t.Run("creates watcher with defaults", func(t *testing.T) {
		fw, err := NewFileWatcher(FileWatcherConfig{})
		require.NoError(t, err)
		defer fw.Stop()

		assert.NotNil(t, fw.watcher)
		assert.NotNil(t, fw.logger)
		assert.Equal(t, 500*time.Millisecond, fw.debounce)
	})

	t.Run("creates watcher with custom config", func(t *testing.T) {
		logger := &util.DefaultLogger{}
		called := false
		onChange := func() { called = true }

		fw, err := NewFileWatcher(FileWatcherConfig{
			Logger:   logger,
			OnChange: onChange,
			Debounce: 100 * time.Millisecond,
		})
		require.NoError(t, err)
		defer fw.Stop()

		assert.Equal(t, logger, fw.logger)
		assert.Equal(t, 100*time.Millisecond, fw.debounce)

		// Verify callback is set
		fw.onChange()
		assert.True(t, called)
	})
}

func TestFileWatcher_Watch(t *testing.T) {
	t.Run("watches existing directory", func(t *testing.T) {
		tmpDir := t.TempDir()

		fw, err := NewFileWatcher(FileWatcherConfig{})
		require.NoError(t, err)
		defer fw.Stop()

		err = fw.Watch(tmpDir)
		assert.NoError(t, err)
	})

	t.Run("watches parent when path does not exist", func(t *testing.T) {
		tmpDir := t.TempDir()
		nonExistentPath := filepath.Join(tmpDir, "subdir", "file.txt")

		fw, err := NewFileWatcher(FileWatcherConfig{})
		require.NoError(t, err)
		defer fw.Stop()

		// Should succeed by watching parent
		err = fw.Watch(nonExistentPath)
		assert.NoError(t, err)
	})

	t.Run("creates parent directory if needed", func(t *testing.T) {
		tmpDir := t.TempDir()
		deepPath := filepath.Join(tmpDir, "new", "deep", "path", "file.txt")

		fw, err := NewFileWatcher(FileWatcherConfig{})
		require.NoError(t, err)
		defer fw.Stop()

		err = fw.Watch(deepPath)
		assert.NoError(t, err)

		// Parent directory should exist
		parentDir := filepath.Dir(deepPath)
		_, err = os.Stat(parentDir)
		assert.NoError(t, err)
	})
}

func TestFileWatcher_DetectsFileChanges(t *testing.T) {
	tmpDir := t.TempDir()
	logFile := filepath.Join(tmpDir, "bot.log")

	// Track callback invocations
	var callCount int32

	fw, err := NewFileWatcher(FileWatcherConfig{
		Logger: &util.DefaultLogger{},
		OnChange: func() {
			atomic.AddInt32(&callCount, 1)
		},
		Debounce: 50 * time.Millisecond, // Short debounce for test
	})
	require.NoError(t, err)
	defer fw.Stop()

	// Watch the directory
	err = fw.Watch(tmpDir)
	require.NoError(t, err)

	// Start the watcher
	go fw.Start()

	// Give watcher time to start
	time.Sleep(100 * time.Millisecond)

	// Create a file - should trigger callback
	err = os.WriteFile(logFile, []byte("line 1\n"), 0644)
	require.NoError(t, err)

	// Wait for callback
	time.Sleep(200 * time.Millisecond)
	assert.GreaterOrEqual(t, atomic.LoadInt32(&callCount), int32(1), "callback should be called on file create")

	// Reset count
	atomic.StoreInt32(&callCount, 0)

	// Write to file - should trigger callback
	time.Sleep(100 * time.Millisecond) // Wait past debounce
	f, err := os.OpenFile(logFile, os.O_APPEND|os.O_WRONLY, 0644)
	require.NoError(t, err)
	_, err = f.WriteString("line 2\n")
	require.NoError(t, err)
	f.Close()

	// Wait for callback
	time.Sleep(200 * time.Millisecond)
	assert.GreaterOrEqual(t, atomic.LoadInt32(&callCount), int32(1), "callback should be called on file write")
}

func TestFileWatcher_Debounce(t *testing.T) {
	tmpDir := t.TempDir()
	logFile := filepath.Join(tmpDir, "bot.log")

	// Create the file first
	err := os.WriteFile(logFile, []byte("initial\n"), 0644)
	require.NoError(t, err)

	// Track callback invocations
	var callCount int32

	fw, err := NewFileWatcher(FileWatcherConfig{
		Logger: &util.DefaultLogger{},
		OnChange: func() {
			atomic.AddInt32(&callCount, 1)
		},
		Debounce: 200 * time.Millisecond,
	})
	require.NoError(t, err)
	defer fw.Stop()

	// Watch the directory
	err = fw.Watch(tmpDir)
	require.NoError(t, err)

	// Start the watcher
	go fw.Start()
	time.Sleep(100 * time.Millisecond)

	// Write multiple times rapidly - should be debounced
	for i := 0; i < 5; i++ {
		f, err := os.OpenFile(logFile, os.O_APPEND|os.O_WRONLY, 0644)
		require.NoError(t, err)
		_, err = f.WriteString("rapid write\n")
		require.NoError(t, err)
		f.Close()
		time.Sleep(10 * time.Millisecond)
	}

	// Wait for debounce period
	time.Sleep(300 * time.Millisecond)

	// Should have at most 2 calls (first write triggers, then one more after debounce)
	count := atomic.LoadInt32(&callCount)
	assert.LessOrEqual(t, count, int32(2), "rapid writes should be debounced")
}

func TestFileWatcher_Stop(t *testing.T) {
	fw, err := NewFileWatcher(FileWatcherConfig{})
	require.NoError(t, err)

	// Start the watcher
	go fw.Start()
	time.Sleep(50 * time.Millisecond)

	// Stop should not block
	done := make(chan struct{})
	go func() {
		err := fw.Stop()
		assert.NoError(t, err)
		close(done)
	}()

	select {
	case <-done:
		// Success
	case <-time.After(2 * time.Second):
		t.Fatal("Stop() blocked for too long")
	}
}

func TestFileWatcher_IgnoresNonWriteEvents(t *testing.T) {
	tmpDir := t.TempDir()
	testFile := filepath.Join(tmpDir, "test.txt")

	// Create file first
	err := os.WriteFile(testFile, []byte("content"), 0644)
	require.NoError(t, err)

	var callCount int32

	fw, err := NewFileWatcher(FileWatcherConfig{
		Logger: &util.DefaultLogger{},
		OnChange: func() {
			atomic.AddInt32(&callCount, 1)
		},
		Debounce: 50 * time.Millisecond,
	})
	require.NoError(t, err)
	defer fw.Stop()

	err = fw.Watch(tmpDir)
	require.NoError(t, err)

	go fw.Start()
	time.Sleep(100 * time.Millisecond)

	// Change permissions - should not trigger (chmod event)
	err = os.Chmod(testFile, 0600)
	require.NoError(t, err)

	time.Sleep(200 * time.Millisecond)

	// No callback should have been triggered for chmod
	count := atomic.LoadInt32(&callCount)
	assert.Equal(t, int32(0), count, "chmod should not trigger callback")
}
