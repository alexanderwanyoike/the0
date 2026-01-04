package daemon

import (
	"context"
	"crypto/sha256"
	"encoding/hex"
	"io"
	"os"
	"path/filepath"
	"sort"
	"time"

	"runtime/internal/runtime/storage"
	"runtime/internal/util"
)

// StateSyncer handles periodic state synchronization to MinIO.
type StateSyncer struct {
	botID        string
	statePath    string
	stateManager storage.StateManager
	logger       util.Logger
	lastHash     string
}

// NewStateSyncer creates a new state syncer.
func NewStateSyncer(botID, statePath string, stateManager storage.StateManager, logger util.Logger) *StateSyncer {
	return &StateSyncer{
		botID:        botID,
		statePath:    statePath,
		stateManager: stateManager,
		logger:       logger,
	}
}

// Sync checks for state changes and uploads if changed.
// Returns true if state was synced, false otherwise.
func (s *StateSyncer) Sync(ctx context.Context) bool {
	currentHash, err := s.hashDirectory()
	if err != nil {
		s.logger.Info("Failed to hash state directory", "error", err.Error())
		return false
	}

	// No changes
	if currentHash == s.lastHash {
		return false
	}

	// Empty directory
	if currentHash == "" {
		return false
	}

	s.logger.Info("State changed, syncing to MinIO", "bot_id", s.botID)
	syncCtx, cancel := context.WithTimeout(ctx, 60*time.Second)
	defer cancel()

	if err := s.stateManager.UploadState(syncCtx, s.botID, s.statePath); err != nil {
		s.logger.Info("Failed to sync state", "error", err.Error())
		return false
	}

	s.lastHash = currentHash
	s.logger.Info("State synced successfully", "bot_id", s.botID)
	return true
}

// hashDirectory computes a hash of all files in the state directory.
// Returns empty string if directory is empty or doesn't exist.
func (s *StateSyncer) hashDirectory() (string, error) {
	if _, err := os.Stat(s.statePath); os.IsNotExist(err) {
		return "", nil
	}

	var files []string
	err := filepath.Walk(s.statePath, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if !info.IsDir() {
			files = append(files, path)
		}
		return nil
	})
	if err != nil {
		return "", err
	}

	if len(files) == 0 {
		return "", nil
	}

	// Sort for consistent ordering
	sort.Strings(files)

	h := sha256.New()
	for _, file := range files {
		// Include relative path in hash
		relPath, _ := filepath.Rel(s.statePath, file)
		h.Write([]byte(relPath))

		// Include file content
		f, err := os.Open(file)
		if err != nil {
			return "", err
		}
		if _, err := io.Copy(h, f); err != nil {
			f.Close()
			return "", err
		}
		f.Close()

		// Include mod time for quick change detection
		info, _ := os.Stat(file)
		if info != nil {
			h.Write([]byte(info.ModTime().String()))
		}
	}

	return hex.EncodeToString(h.Sum(nil)), nil
}
