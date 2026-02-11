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

// Sync reads new log content and uploads to MinIO in bounded chunks.
// Drains all available content (up to the size at call time) so large bursts
// are fully caught up in a single Sync call.
// Returns true if any logs were synced, false otherwise.
func (l *LogsSyncer) Sync(ctx context.Context) bool {
	logFile := filepath.Join(l.logsPath, "bot.log")
	file, err := os.Open(logFile)
	if err != nil {
		if os.IsNotExist(err) {
			return false
		}
		l.logger.Info("Failed to open log file", "error", err.Error())
		return false
	}
	defer file.Close()

	info, err := file.Stat()
	if err != nil {
		l.logger.Info("Failed to stat log file", "error", err.Error())
		return false
	}

	currentSize := info.Size()

	// Handle log rotation/truncation
	if currentSize < l.lastOffset {
		l.logger.Info("Log file truncated/rotated, resetting offset", "old_offset", l.lastOffset, "new_size", currentSize)
		l.lastOffset = 0
	}

	if currentSize == l.lastOffset {
		return false
	}

	if _, err := file.Seek(l.lastOffset, 0); err != nil {
		l.logger.Info("Failed to seek in log file", "error", err.Error())
		return false
	}

	// Drain all available content in bounded chunks
	const maxChunkSize = 1024 * 1024
	synced := false

	for l.lastOffset < currentSize {
		delta := currentSize - l.lastOffset
		readSize := int(delta)
		if delta > maxChunkSize {
			readSize = maxChunkSize
		}

		buf := make([]byte, readSize)
		n, err := io.ReadFull(file, buf)
		if err == io.ErrUnexpectedEOF {
			err = nil
		}
		if err != nil {
			l.logger.Info("Failed to read log file", "error", err.Error())
			break
		}
		if n == 0 {
			break
		}

		syncCtx, cancel := context.WithTimeout(ctx, 30*time.Second)
		if err := l.logUploader.AppendBotLogs(syncCtx, l.botID, string(buf[:n])); err != nil {
			cancel()
			l.logger.Info("Failed to upload logs", "error", err.Error())
			break
		}
		cancel()

		if l.natsPublisher != nil {
			if err := l.natsPublisher.Publish(l.botID, string(buf[:n])); err != nil {
				l.logger.Info("Failed to publish logs to NATS", "error", err.Error())
			}
		}

		l.lastOffset += int64(n)
		l.logger.Info("Logs synced successfully", "bot_id", l.botID, "bytes", n)
		synced = true
	}

	return synced
}

// Close cleans up all owned resources (uploader and publisher).
func (l *LogsSyncer) Close() error {
	var firstErr error
	if l.logUploader != nil {
		if err := l.logUploader.Close(); err != nil {
			firstErr = err
		}
	}
	if l.natsPublisher != nil {
		if err := l.natsPublisher.Close(); err != nil && firstErr == nil {
			firstErr = err
		}
	}
	return firstErr
}
