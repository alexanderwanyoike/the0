package daemon

import (
	"bytes"
	"context"
	"errors"
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

// Sync reads new log content from the bot's log file and uploads it to MinIO
// in bounded chunks. It drains all available content (up to the file size at
// call time) so large bursts are fully caught up in a single Sync call.
// Each chunk is split on newline boundaries to avoid breaking log lines or
// UTF-8 runes. Returns true if any logs were synced, false otherwise.
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
	buf := make([]byte, maxChunkSize)
	totalBytes := 0
	chunks := 0

	for l.lastOffset < currentSize {
		if ctx.Err() != nil {
			break
		}
		remainingBytes := currentSize - l.lastOffset
		if remainingBytes > maxChunkSize {
			remainingBytes = maxChunkSize
		}
		readSize := int(remainingBytes)

		n, err := io.ReadFull(file, buf[:readSize])
		if err == io.ErrUnexpectedEOF {
			// File was shorter than expected (likely concurrent truncation) — process what we got
			l.logger.Info("Log file shorter than expected, processing partial read", "expected", readSize, "got", n)
			err = nil
		}
		if err != nil {
			l.logger.Info("Failed to read log file", "error", err.Error())
			break
		}
		if n == 0 {
			break
		}

		// Adjust to newline boundary to avoid splitting log lines or UTF-8 runes.
		// Only applies when there's more data remaining (not the final chunk).
		chunkSize, seekErr := adjustChunkToNewlineBoundary(file, buf, n, l.lastOffset, currentSize)
		if seekErr != nil {
			l.logger.Info("Failed to seek in log file", "error", seekErr.Error())
			break
		}
		if chunkSize != n && chunkSize == 0 {
			// No newline found — upload the full read as-is to avoid stalling
			l.logger.Info("No newline boundary in chunk, uploading as-is", "bytes", n)
			chunkSize = n
		}

		chunk := string(buf[:chunkSize])

		if uploadErr := l.uploadChunk(ctx, chunk); uploadErr != nil {
			l.logger.Info("Failed to upload logs", "error", uploadErr.Error())
			break
		}

		l.lastOffset += int64(chunkSize)
		totalBytes += chunkSize
		chunks++
	}

	if totalBytes > 0 {
		l.logger.Info("Logs synced successfully", "bot_id", l.botID, "bytes", totalBytes, "chunks", chunks)
		return true
	}
	return false
}

// adjustChunkToNewlineBoundary finds the last newline in buf[:n] and seeks the
// file back so the remainder is re-read in the next iteration. If the chunk is
// the final one (offset+n reaches currentSize), it returns n unchanged.
// Returns 0 when no newline is found in a non-final chunk (caller decides what to do).
func adjustChunkToNewlineBoundary(file *os.File, buf []byte, n int, offset int64, currentSize int64) (int, error) {
	if offset+int64(n) >= currentSize {
		// Final chunk — no adjustment needed
		return n, nil
	}
	idx := bytes.LastIndexByte(buf[:n], '\n')
	if idx < 0 {
		// No newline found in this chunk
		return 0, nil
	}
	chunkSize := idx + 1
	if _, err := file.Seek(offset+int64(chunkSize), 0); err != nil {
		return 0, err
	}
	return chunkSize, nil
}

// uploadChunk uploads a log chunk to MinIO and publishes it to NATS.
func (l *LogsSyncer) uploadChunk(ctx context.Context, content string) error {
	syncCtx, cancel := context.WithTimeout(ctx, 30*time.Second)
	defer cancel()

	if err := l.logUploader.AppendBotLogs(syncCtx, l.botID, content); err != nil {
		return err
	}

	if l.natsPublisher != nil {
		if err := l.natsPublisher.Publish(l.botID, content); err != nil {
			l.logger.Info("Failed to publish logs to NATS", "error", err.Error())
		}
	}

	return nil
}

// Close cleans up all owned resources (uploader and publisher).
func (l *LogsSyncer) Close() error {
	var uploaderErr, publisherErr error
	if l.logUploader != nil {
		uploaderErr = l.logUploader.Close()
	}
	if l.natsPublisher != nil {
		publisherErr = l.natsPublisher.Close()
	}
	return errors.Join(uploaderErr, publisherErr)
}
