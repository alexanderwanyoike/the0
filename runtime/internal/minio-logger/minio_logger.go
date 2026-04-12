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

// multipartClient is the subset of minio.Core the compose-append path needs.
// Extracted so the abort-on-error contract in AppendBotLogs can be unit-tested
// with a fake that triggers failures at each step of the multipart flow.
type multipartClient interface {
	NewMultipartUpload(ctx context.Context, bucket, object string, opts minio.PutObjectOptions) (string, error)
	CopyObjectPart(ctx context.Context, srcBucket, srcObject, destBucket, destObject, uploadID string,
		partID int, startOffset, length int64, metadata map[string]string) (minio.CompletePart, error)
	CompleteMultipartUpload(ctx context.Context, bucket, object, uploadID string, parts []minio.CompletePart, opts minio.PutObjectOptions) (minio.UploadInfo, error)
	AbortMultipartUpload(ctx context.Context, bucket, object, uploadID string) error
}

// coreMultipartClient adapts minio.Core to the multipartClient interface.
type coreMultipartClient struct {
	core minio.Core
}

func (c coreMultipartClient) NewMultipartUpload(ctx context.Context, bucket, object string, opts minio.PutObjectOptions) (string, error) {
	return c.core.NewMultipartUpload(ctx, bucket, object, opts)
}

func (c coreMultipartClient) CopyObjectPart(ctx context.Context, srcBucket, srcObject, destBucket, destObject, uploadID string,
	partID int, startOffset, length int64, metadata map[string]string,
) (minio.CompletePart, error) {
	return c.core.CopyObjectPart(ctx, srcBucket, srcObject, destBucket, destObject, uploadID, partID, startOffset, length, metadata)
}

func (c coreMultipartClient) CompleteMultipartUpload(ctx context.Context, bucket, object, uploadID string, parts []minio.CompletePart, opts minio.PutObjectOptions) (minio.UploadInfo, error) {
	return c.core.CompleteMultipartUpload(ctx, bucket, object, uploadID, parts, opts)
}

func (c coreMultipartClient) AbortMultipartUpload(ctx context.Context, bucket, object, uploadID string) error {
	return c.core.AbortMultipartUpload(ctx, bucket, object, uploadID)
}

type minIOLogger struct {
	client        *minio.Client
	mpClient      multipartClient
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
		mpClient:      coreMultipartClient{core: minio.Core{Client: client}},
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

	// Format the new log entry as NDJSON with guaranteed timestamp fields.
	content := FormatLogChunk(logs, now)

	// Upload the new chunk as a temporary object.
	tmpKey := fmt.Sprintf("logs/%s/.tmp-%d", id, now.UnixNano())
	reader := strings.NewReader(content)
	_, err := m.client.PutObject(ctx, m.logsBucket, tmpKey, reader, int64(len(content)), minio.PutObjectOptions{
		ContentType: "text/plain",
	})
	if err != nil {
		return fmt.Errorf("upload temp chunk: %w", err)
	}
	// Clean up the temp object after the compose/copy completes. Uses a fresh
	// context so cleanup succeeds even if the caller's context was cancelled.
	defer func() {
		cleanupCtx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
		defer cancel()
		if err := m.client.RemoveObject(cleanupCtx, m.logsBucket, tmpKey, minio.RemoveObjectOptions{}); err != nil {
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
		// multipart copy. We drive the flow ourselves via minio.Core so
		// that on ANY error (caller ctx cancelled, network flake, R2 tail
		// latency, simulated failure in tests) we guarantee an abort on
		// the upload ID. minio-go's high-level ComposeObject leaks the
		// upload ID on error, which is the root cause of the bot-logs
		// orphaned multipart uploads this path exists to fix.
		m.logger.Debug("Using multipart-copy for append", "existingSize", info.Size)
		if err := m.composeAppendMultipart(ctx, dailyKey, tmpKey); err != nil {
			return err
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

// composeAppendMultipartAbortTimeout is the per-abort budget used when the
// caller's context is already cancelled. We always issue the abort with a
// fresh background context so cleanup reaches R2 even after the caller's
// deadline has fired.
const composeAppendMultipartAbortTimeout = 30 * time.Second

// composeAppendMultipart performs a server-side multipart copy that
// concatenates the existing daily log and the newly-uploaded temp chunk
// back into the daily key. We own the upload ID and defer an abort that
// fires whenever the flow did not reach a successful CompleteMultipartUpload.
// The abort runs on a fresh context so a cancelled caller context does not
// prevent the cleanup from reaching R2.
//
// Safe to abort on this key because AppendBotLogs holds a per-bot mutex:
// no other goroutine can have an in-flight multipart upload for dailyKey
// when this function is running.
func (m *minIOLogger) composeAppendMultipart(ctx context.Context, dailyKey, tmpKey string) error {
	uploadID, err := m.mpClient.NewMultipartUpload(ctx, m.logsBucket, dailyKey, minio.PutObjectOptions{
		ContentType: "text/plain",
	})
	if err != nil {
		return fmt.Errorf("init multipart upload: %w", err)
	}

	done := false
	defer func() {
		if done {
			return
		}
		abortCtx, cancel := context.WithTimeout(context.Background(), composeAppendMultipartAbortTimeout)
		defer cancel()
		if abortErr := m.mpClient.AbortMultipartUpload(abortCtx, m.logsBucket, dailyKey, uploadID); abortErr != nil {
			m.logger.Warn("Failed to abort multipart upload after compose error",
				"key", dailyKey, "uploadID", uploadID, "error", abortErr)
		}
	}()

	// Part 1: the existing daily log. length = -1 copies the whole source.
	// The daily file is guaranteed >= 5 MiB here (caller checks composeMinPartSize).
	part1, err := m.mpClient.CopyObjectPart(ctx, m.logsBucket, dailyKey, m.logsBucket, dailyKey, uploadID, 1, 0, -1, nil)
	if err != nil {
		return fmt.Errorf("copy daily part: %w", err)
	}

	// Part 2: the new temp chunk. This is the LAST part so it may be < 5 MiB.
	part2, err := m.mpClient.CopyObjectPart(ctx, m.logsBucket, tmpKey, m.logsBucket, dailyKey, uploadID, 2, 0, -1, nil)
	if err != nil {
		return fmt.Errorf("copy temp part: %w", err)
	}

	if _, err := m.mpClient.CompleteMultipartUpload(ctx, m.logsBucket, dailyKey, uploadID,
		[]minio.CompletePart{part1, part2}, minio.PutObjectOptions{}); err != nil {
		return fmt.Errorf("complete multipart upload: %w", err)
	}

	done = true
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
