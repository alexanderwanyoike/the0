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

// defaultLogsUploadTimeout bounds a single AppendBotLogs call. Comfortably
// exceeds R2 tail latency on the multipart-copy path for bots with growing
// daily logs; previously this was 30s which was causing context timeouts
// mid-upload and leaking multipart upload IDs on R2.
const defaultLogsUploadTimeout = 120 * time.Second

// LogsSyncerOption configures a LogsSyncer at construction.
type LogsSyncerOption func(*LogsSyncer)

// WithLogsUploadTimeout overrides the per-chunk upload timeout.
func WithLogsUploadTimeout(d time.Duration) LogsSyncerOption {
	return func(l *LogsSyncer) { l.uploadTimeout = d }
}

// LogsSyncer handles periodic log synchronization to MinIO.
type LogsSyncer struct {
	botID         string
	logsPath      string
	logUploader   miniologger.MinIOLogger
	natsPublisher LogPublisher
	logger        util.Logger
	lastOffset    int64
	uploadTimeout time.Duration
	// pending is a chunk whose upload failed. It is resent as formatted,
	// because reformatting would give its lines new keys and timestamps and
	// live viewers that already got it could not recognise the duplicates.
	pending *logChunk
}

// logChunk is one raw span of the log file and the NDJSON formatted from it.
type logChunk struct {
	offset  int64
	rawSize int
	at      time.Time
	content string
}

// NewLogsSyncer creates a new logs syncer.
// Returns nil if logUploader is nil (logs syncing disabled).
// natsPublisher is optional — pass nil to disable NATS log publishing.
func NewLogsSyncer(botID, logsPath string, logUploader miniologger.MinIOLogger, natsPublisher LogPublisher, logger util.Logger, opts ...LogsSyncerOption) *LogsSyncer {
	if logUploader == nil {
		return nil
	}
	l := &LogsSyncer{
		botID:         botID,
		logsPath:      logsPath,
		logUploader:   logUploader,
		natsPublisher: natsPublisher,
		logger:        logger,
		uploadTimeout: defaultLogsUploadTimeout,
	}
	for _, opt := range opts {
		opt(l)
	}
	return l
}

// FinalSyncTimeout returns the context deadline for the shutdown final sync.
func (l *LogsSyncer) FinalSyncTimeout() time.Duration {
	return l.uploadTimeout
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
		l.pending = nil
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
		chunk, ok := l.nextChunk(file, buf, currentSize)
		if !ok {
			break
		}

		if uploadErr := l.uploadChunk(ctx, chunk); uploadErr != nil {
			l.logger.Info("Failed to upload logs", "error", uploadErr.Error())
			l.pending = &chunk
			break
		}

		l.pending = nil
		l.lastOffset += int64(chunk.rawSize)
		totalBytes += chunk.rawSize
		chunks++
	}

	if totalBytes > 0 {
		l.logger.Info("Logs synced successfully", "bot_id", l.botID, "bytes", totalBytes, "chunks", chunks)
		return true
	}
	return false
}

// nextChunk returns the chunk starting at lastOffset and leaves file
// positioned after it: the pending chunk when its retry is due, otherwise the
// next raw span read from file and formatted with the current time.
func (l *LogsSyncer) nextChunk(file *os.File, buf []byte, currentSize int64) (logChunk, bool) {
	if p := l.pending; p != nil && p.offset == l.lastOffset && p.offset+int64(p.rawSize) <= currentSize {
		if _, err := file.Seek(p.offset+int64(p.rawSize), io.SeekStart); err != nil {
			l.logger.Info("Failed to seek in log file", "error", err.Error())
			return logChunk{}, false
		}
		return *p, true
	}
	l.pending = nil

	remainingBytes := currentSize - l.lastOffset
	if remainingBytes > int64(len(buf)) {
		remainingBytes = int64(len(buf))
	}
	readSize := int(remainingBytes)

	n, err := io.ReadFull(file, buf[:readSize])
	if err == io.ErrUnexpectedEOF {
		// File was shorter than expected (likely concurrent truncation), so process what we got
		l.logger.Info("Log file shorter than expected, processing partial read", "expected", readSize, "got", n)
		err = nil
	}
	if err != nil {
		l.logger.Info("Failed to read log file", "error", err.Error())
		return logChunk{}, false
	}
	if n == 0 {
		return logChunk{}, false
	}

	// Adjust to newline boundary to avoid splitting log lines or UTF-8 runes.
	// Only applies when there's more data remaining (not the final chunk).
	chunkSize, seekErr := adjustChunkToNewlineBoundary(file, buf, n, l.lastOffset, currentSize)
	if seekErr != nil {
		l.logger.Info("Failed to seek in log file", "error", seekErr.Error())
		return logChunk{}, false
	}
	if chunkSize != n && chunkSize == 0 {
		// No newline found: upload the full read as-is to avoid stalling
		l.logger.Info("No newline boundary in chunk, uploading as-is", "bytes", n)
		chunkSize = n
	}

	at := time.Now()
	return logChunk{
		offset:  l.lastOffset,
		rawSize: chunkSize,
		at:      at,
		content: miniologger.FormatLogChunk(string(buf[:chunkSize]), at),
	}, true
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

// uploadChunk publishes a log chunk to NATS and uploads it to MinIO.
// NATS goes first: live SSE viewers care about latency, while the MinIO
// append is O(day-file-size) and can fail transiently — neither should
// delay or suppress the live stream. An upload failure keeps lastOffset in
// place so the chunk is re-uploaded on the next Sync (durability never drops
// data); that retry also re-publishes the same bytes to NATS, which is
// accepted at-least-once behavior for the live stream.
func (l *LogsSyncer) uploadChunk(ctx context.Context, chunk logChunk) error {
	if chunk.content == "" {
		return nil
	}
	if l.natsPublisher != nil {
		if err := l.natsPublisher.Publish(l.botID, chunk.content); err != nil {
			l.logger.Info("Failed to publish logs to NATS", "error", err.Error())
		}
	}

	syncCtx, cancel := context.WithTimeout(ctx, l.uploadTimeout)
	defer cancel()

	return l.logUploader.AppendBotLogs(syncCtx, l.botID, chunk.content, chunk.at)
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
