package daemon

import (
	"context"
	"io"
	"os"
	"path/filepath"
	"time"

	miniologger "runtime/internal/minio-logger"
	"runtime/internal/util"
)

// LogsSyncer handles periodic log synchronization to MinIO.
type LogsSyncer struct {
	botID          string
	logsPath       string
	logUploader    miniologger.MinIOLogger
	natsPublisher  LogPublisher
	logger         util.Logger
	lastOffset     int64
}

// NewLogsSyncer creates a new logs syncer.
// Returns nil if logUploader is nil (logs syncing disabled).
// natsPublisher is optional — pass nil to disable NATS log publishing.
func NewLogsSyncer(botID, logsPath string, logUploader miniologger.MinIOLogger, natsPublisher LogPublisher, logger util.Logger) *LogsSyncer {
	if logUploader == nil {
		return nil
	}
	return &LogsSyncer{
		botID:         botID,
		logsPath:      logsPath,
		logUploader:   logUploader,
		natsPublisher: natsPublisher,
		logger:        logger,
	}
}

// Sync reads new log content and uploads to MinIO.
// Returns true if logs were synced, false otherwise.
func (l *LogsSyncer) Sync(ctx context.Context) bool {
	logFile := filepath.Join(l.logsPath, "bot.log")
	file, err := os.Open(logFile)
	if err != nil {
		if os.IsNotExist(err) {
			// Log file doesn't exist yet, that's okay
			return false
		}
		l.logger.Info("Failed to open log file", "error", err.Error())
		return false
	}
	defer file.Close()

	// Get current file size
	info, err := file.Stat()
	if err != nil {
		l.logger.Info("Failed to stat log file", "error", err.Error())
		return false
	}

	currentSize := info.Size()

	// Handle log rotation/truncation: if file is smaller than last offset, reset
	if currentSize < l.lastOffset {
		l.logger.Info("Log file truncated/rotated, resetting offset", "old_offset", l.lastOffset, "new_size", currentSize)
		l.lastOffset = 0
	}

	// No new content
	if currentSize == l.lastOffset {
		return false
	}

	// Seek to last position
	if _, err := file.Seek(l.lastOffset, 0); err != nil {
		l.logger.Info("Failed to seek in log file", "error", err.Error())
		return false
	}

	// Read new content
	newContent := make([]byte, currentSize-l.lastOffset)
	n, err := io.ReadFull(file, newContent)
	if err == io.ErrUnexpectedEOF {
		err = nil // partial read is OK for log tailing
	}
	if err != nil {
		l.logger.Info("Failed to read log file", "error", err.Error())
		return false
	}

	if n == 0 {
		return false
	}

	// Upload to MinIO
	syncCtx, cancel := context.WithTimeout(ctx, 30*time.Second)
	defer cancel()

	if err := l.logUploader.AppendBotLogs(syncCtx, l.botID, string(newContent[:n])); err != nil {
		l.logger.Info("Failed to upload logs", "error", err.Error())
		return false
	}

	// Publish to NATS for real-time log streaming (best-effort)
	if l.natsPublisher != nil {
		if err := l.natsPublisher.Publish(l.botID, string(newContent[:n])); err != nil {
			l.logger.Info("Failed to publish logs to NATS", "error", err.Error())
		}
	}

	l.lastOffset = currentSize
	l.logger.Info("Logs synced successfully", "bot_id", l.botID, "bytes", n)
	return true
}

// Close cleans up resources.
func (l *LogsSyncer) Close() error {
	if l.logUploader != nil {
		return l.logUploader.Close()
	}
	return nil
}
