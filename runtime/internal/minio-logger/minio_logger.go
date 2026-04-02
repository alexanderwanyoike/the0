package miniologger

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"strings"
	"sync"
	"time"

	"github.com/minio/minio-go/v7"
	"github.com/minio/minio-go/v7/pkg/credentials"

	"runtime/internal/util"
)

type MinIOLogger interface {
	AppendBotLogs(ctx context.Context, id string, logs string) error
	StoreFinalLogs(ctx context.Context, id string, logs string) error
	Close() error
}

type minIOLogger struct {
	client        *minio.Client
	logsBucket    string
	resultsBucket string
	mutexes       map[string]*sync.Mutex // Per-bot mutexes to prevent concurrent writes
	mutexesMu     sync.RWMutex           // Protects the mutexes map
	logger        util.Logger
}

type MinioLoggerOptions struct {
	LogsBucket    string
	ResultsBucket string
	Endpoint      string
	AccessKey     string
	SecretKey     string
	UseSSL        bool
	Logger        util.Logger // Optional logger, defaults to NopLogger
}

func NewMinIOLogger(
	ctx context.Context,
	options MinioLoggerOptions,
) (*minIOLogger, error) {
	if options.Endpoint == "" {
		return nil, fmt.Errorf("endpoint is required")
	}

	if options.AccessKey == "" {
		return nil, fmt.Errorf("access key is required")
	}

	if options.SecretKey == "" {
		return nil, fmt.Errorf("secret key is required")
	}

	if options.LogsBucket == "" {
		options.LogsBucket = "bot-logs" // Default bucket name
	}

	if options.ResultsBucket == "" {
		options.ResultsBucket = "backtest" // Use same bucket by default
	}

	useSSL := options.UseSSL

	// Initialize MinIO client
	client, err := minio.New(options.Endpoint, &minio.Options{
		Creds:  credentials.NewStaticV4(options.AccessKey, options.SecretKey, ""),
		Secure: useSSL,
	})
	if err != nil {
		return nil, fmt.Errorf("failed to create MinIO client: %v", err)
	}

	// Ensure bucket exists
	exists, err := client.BucketExists(ctx, options.LogsBucket)
	if err != nil {
		return nil, fmt.Errorf("failed to check if bucket exists: %v", err)
	}

	if !exists {
		err = client.MakeBucket(ctx, options.LogsBucket, minio.MakeBucketOptions{})
		if err != nil {
			return nil, fmt.Errorf("failed to create bucket: %v", err)
		}
	}

	// Set results bucket - same as main bucket for now, but could be different

	if options.ResultsBucket == "" {
		options.ResultsBucket = "backtest" // Use same bucket by default
	}

	// Create results bucket if it doesn't exist
	exists, err = client.BucketExists(ctx, options.ResultsBucket)
	if err != nil {
		return nil, fmt.Errorf("failed to check if results bucket exists: %v", err)
	}

	if !exists {
		err = client.MakeBucket(ctx, options.ResultsBucket, minio.MakeBucketOptions{})
		if err != nil {
			return nil, fmt.Errorf("failed to create results bucket: %v", err)
		}
	}

	// Default to NopLogger if no logger provided
	logger := options.Logger
	if logger == nil {
		logger = &util.NopLogger{}
	}

	return &minIOLogger{
		client:        client,
		logsBucket:    options.LogsBucket,
		resultsBucket: options.ResultsBucket,
		mutexes:       make(map[string]*sync.Mutex),
		mutexesMu:     sync.RWMutex{},
		logger:        logger,
	}, nil
}

// getBotMutex gets or creates a mutex for a specific bot
func (m *minIOLogger) getMutex(botID string) *sync.Mutex {
	m.mutexesMu.RLock()
	if mu, exists := m.mutexes[botID]; exists {
		m.mutexesMu.RUnlock()
		return mu
	}
	m.mutexesMu.RUnlock()

	m.mutexesMu.Lock()
	defer m.mutexesMu.Unlock()

	// Double-check pattern to avoid race condition
	if mu, exists := m.mutexes[botID]; exists {
		return mu
	}

	mu := &sync.Mutex{}
	m.mutexes[botID] = mu
	return mu
}

// composeMinPartSize is the minimum part size that S3/MinIO requires for
// non-last parts in a ComposeObject (multipart server-side copy). Parts smaller
// than this trigger an error, so we fall back to read-modify-write for daily
// log files below this threshold.
const composeMinPartSize = 5 * 1024 * 1024 // 5 MiB

func (m *minIOLogger) AppendBotLogs(ctx context.Context, id string, logs string) error {
	if logs == "" {
		return nil // No logs to append
	}

	if m.client == nil {
		return fmt.Errorf("MinIO client is not initialized")
	}

	// Get bot-specific mutex to prevent concurrent writes to the same bot's log
	botMutex := m.getMutex(id)
	botMutex.Lock()
	defer botMutex.Unlock()

	// Single clock read for the entire append to avoid midnight-boundary splits
	// where the daily key, timestamp prefix, and temp key disagree on the date.
	now := time.Now()

	// Generate file path: logs/{bot_id}/{YYYYMMDD}.log
	dailyKey := fmt.Sprintf("logs/%s/%s.log", id, now.Format("20060102"))
	m.logger.Debug("Attempting to append logs to MinIO", "path", dailyKey, "bucket", m.logsBucket)

	// Format the new log entry with a timestamp prefix.
	content := fmt.Sprintf("[%s] %s\n", now.Format("2006-01-02 15:04:05"), strings.TrimSpace(logs))

	// Upload the new chunk as a temporary object.
	tmpKey := fmt.Sprintf("logs/%s/.tmp-%d", id, now.UnixNano())
	reader := strings.NewReader(content)
	_, err := m.client.PutObject(ctx, m.logsBucket, tmpKey, reader, int64(len(content)), minio.PutObjectOptions{
		ContentType: "text/plain",
	})
	if err != nil {
		return fmt.Errorf("upload temp chunk: %w", err)
	}
	// Clean up the temp object after the compose/copy completes. Synchronous
	// so that callers can rely on temp files being gone when AppendBotLogs
	// returns, and we don't leak objects on process exit or slow storage.
	defer func() {
		if err := m.client.RemoveObject(ctx, m.logsBucket, tmpKey, minio.RemoveObjectOptions{}); err != nil {
			m.logger.Warn("Failed to clean up temp object", "key", tmpKey, "error", err)
		}
	}()

	// Check whether the daily file already exists and how large it is.
	info, statErr := m.client.StatObject(ctx, m.logsBucket, dailyKey, minio.StatObjectOptions{})

	if statErr != nil {
		// Only treat "NoSuchKey" as "file doesn't exist". Any other error
		// (network, auth, timeout) should not silently replace the daily log.
		errResponse := minio.ToErrorResponse(statErr)
		if errResponse.Code != "NoSuchKey" {
			return fmt.Errorf("stat daily log: %w", statErr)
		}
		// Daily file does not exist yet. Server-side copy the temp object
		// into the daily key so we avoid re-uploading the data.
		m.logger.Debug("Daily file does not exist, copying temp to daily key")
		dst := minio.CopyDestOptions{Bucket: m.logsBucket, Object: dailyKey}
		src := minio.CopySrcOptions{Bucket: m.logsBucket, Object: tmpKey}
		_, err = m.client.CopyObject(ctx, dst, src)
		if err != nil {
			return fmt.Errorf("copy temp to daily key: %w", err)
		}
		m.logger.Debug("Successfully created daily log file", "path", dailyKey)
		return nil
	}

	// Daily file exists. Choose the append strategy based on its size.
	if info.Size >= composeMinPartSize {
		// The existing file is large enough (>= 5 MiB) for a server-side
		// ComposeObject. Each sync stays O(chunk_size) instead of degrading
		// to O(file_size) as the daily log grows throughout the day.
		m.logger.Debug("Using ComposeObject for append", "existingSize", info.Size)
		dst := minio.CopyDestOptions{Bucket: m.logsBucket, Object: dailyKey}
		srcs := []minio.CopySrcOptions{
			{Bucket: m.logsBucket, Object: dailyKey},
			{Bucket: m.logsBucket, Object: tmpKey},
		}
		_, err = m.client.ComposeObject(ctx, dst, srcs...)
		if err != nil {
			return fmt.Errorf("compose daily log: %w", err)
		}
	} else {
		// The existing file is smaller than the 5 MiB compose threshold.
		// Fall back to read-modify-write, but this is bounded by at most
		// 5 MiB of I/O so it stays fast.
		m.logger.Debug("Using read-modify-write for small file", "existingSize", info.Size)
		obj, getErr := m.client.GetObject(ctx, m.logsBucket, dailyKey, minio.GetObjectOptions{})
		if getErr != nil {
			return fmt.Errorf("read existing daily log: %w", getErr)
		}
		existingContent, readErr := io.ReadAll(obj)
		obj.Close()
		if readErr != nil {
			return fmt.Errorf("read existing daily log body: %w", readErr)
		}

		var buffer bytes.Buffer
		buffer.Write(existingContent)
		buffer.WriteString(content)

		combined := bytes.NewReader(buffer.Bytes())
		_, err = m.client.PutObject(ctx, m.logsBucket, dailyKey, combined, int64(buffer.Len()), minio.PutObjectOptions{
			ContentType: "text/plain",
		})
		if err != nil {
			return fmt.Errorf("write combined daily log: %w", err)
		}
	}

	m.logger.Debug("Successfully appended logs to MinIO", "path", dailyKey)
	return nil
}

// StoreFinalLogs stores the final logs for a completed bot at {bot_id}/logs.txt in the bots bucket
func (m *minIOLogger) StoreFinalLogs(ctx context.Context, id string, logs string) error {
	if logs == "" {
		return nil // No logs to store
	}

	if m.client == nil {
		return fmt.Errorf("MinIO client is not initialized")
	}

	// Generate file path: {bot_id}/logs.txt (bucket is already 'bots')
	objectName := fmt.Sprintf("%s/logs.txt", id)

	// Write logs to MinIO
	reader := bytes.NewReader([]byte(logs))
	_, err := m.client.PutObject(ctx, m.resultsBucket, objectName, reader, int64(len(logs)), minio.PutObjectOptions{
		ContentType: "text/plain",
	})
	if err != nil {
		return fmt.Errorf("failed to write final logs to MinIO: %v", err)
	}

	return nil
}

func (m *minIOLogger) Close() error {
	// MinIO client doesn't need explicit close
	return nil
}

// GetLogFilePath returns the MinIO path for a bot's log file for a specific date
func (m *minIOLogger) GetLogFilePath(id string, date time.Time) string {
	timestamp := date.Format("20060102") // YYYYMMDD format
	return fmt.Sprintf("logs/%s/%s.log", id, timestamp)
}
