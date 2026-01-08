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

	// Generate file path: logs/{bot_id}/{YYYYMMDD}.log
	timestamp := time.Now().Format("20060102") // YYYYMMDD format
	objectPath := fmt.Sprintf("logs/%s/%s.log", id, timestamp)
	m.logger.Debug("Attempting to append logs to MinIO", "path", objectPath, "bucket", m.logsBucket)

	// Read existing content if file exists
	var existingContent []byte
	obj, err := m.client.GetObject(ctx, m.logsBucket, objectPath, minio.GetObjectOptions{})
	if err != nil {
		// Check if error is NoSuchKey (file doesn't exist) using string contains
		m.logger.Debug("GetObject error", "error", err.Error())
		if strings.Contains(err.Error(), "NoSuchKey") || strings.Contains(err.Error(), "The specified key does not exist") {
			m.logger.Debug("File doesn't exist, will create new one")
			// File doesn't exist, that's okay - we'll create it
		} else {
			return fmt.Errorf("failed to read existing log file: %v", err)
		}
	} else {
		m.logger.Debug("GetObject succeeded, reading content")
		existingContent, err = io.ReadAll(obj)
		obj.Close()
		if err != nil {
			// Check if this is also a NoSuchKey error (race condition)
			m.logger.Debug("ReadAll error", "error", err.Error())
			if strings.Contains(err.Error(), "NoSuchKey") || strings.Contains(err.Error(), "The specified key does not exist") {
				// File was deleted between GetObject and ReadAll, treat as if file doesn't exist
				m.logger.Debug("ReadAll NoSuchKey, treating as empty file")
				existingContent = []byte{}
			} else {
				// For any other ReadAll error, treat as empty file and continue
				m.logger.Debug("ReadAll error, treating as empty file and continuing")
				existingContent = []byte{}
			}
		} else {
			m.logger.Debug("Successfully read existing content", "bytes", len(existingContent))
		}
	}

	// Append new logs to existing content
	var buffer bytes.Buffer
	buffer.Write(existingContent)

	// Add timestamp prefix to new logs
	timestampPrefix := time.Now().Format("2006-01-02 15:04:05")
	newLogEntry := fmt.Sprintf("[%s] %s\n", timestampPrefix, strings.TrimSpace(logs))
	buffer.WriteString(newLogEntry)

	// Write the combined content back to MinIO
	reader := bytes.NewReader(buffer.Bytes())
	m.logger.Debug("Writing bytes to MinIO", "bytes", buffer.Len())
	_, err = m.client.PutObject(ctx, m.logsBucket, objectPath, reader, int64(buffer.Len()), minio.PutObjectOptions{
		ContentType: "text/plain",
	})
	if err != nil {
		m.logger.Debug("PutObject failed", "error", err.Error())
		return fmt.Errorf("failed to write logs to MinIO: %v", err)
	}

	m.logger.Debug("Successfully wrote logs to MinIO", "path", objectPath)
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
