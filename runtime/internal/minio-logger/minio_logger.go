package miniologger

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"os"
	"strings"
	"sync"
	"time"

	"github.com/minio/minio-go/v7"
	"github.com/minio/minio-go/v7/pkg/credentials"
)

type MinIOLogger interface {
	AppendBotLogs(ctx context.Context, id string, logs string) error
	StoreFinalLogs(ctx context.Context, id string, logs string) error
	Close() error
}

type minIOLogger struct {
	client        *minio.Client
	bucketName    string
	resultsBucket string
	mutexes       map[string]*sync.Mutex // Per-bot mutexes to prevent concurrent writes
	mutexesMu     sync.RWMutex           // Protects the mutexes map
}

func NewMinIOLogger(ctx context.Context) (*minIOLogger, error) {
	endpoint := os.Getenv("MINIO_ENDPOINT")
	if endpoint == "" {
		return nil, fmt.Errorf("MINIO_ENDPOINT environment variable is required")
	}

	accessKey := os.Getenv("MINIO_ACCESS_KEY")
	if accessKey == "" {
		return nil, fmt.Errorf("MINIO_ACCESS_KEY environment variable is required")
	}

	secretKey := os.Getenv("MINIO_SECRET_KEY")
	if secretKey == "" {
		return nil, fmt.Errorf("MINIO_SECRET_KEY environment variable is required")
	}

	bucketName := os.Getenv("MINIO_LOGS_BUCKET")
	if bucketName == "" {
		bucketName = "bot-logs" // Default bucket name
	}

	useSSL := os.Getenv("MINIO_USE_SSL") == "true" || os.Getenv("MINIO_SSL") == "true"

	// Initialize MinIO client
	client, err := minio.New(endpoint, &minio.Options{
		Creds:  credentials.NewStaticV4(accessKey, secretKey, ""),
		Secure: useSSL,
	})
	if err != nil {
		return nil, fmt.Errorf("failed to create MinIO client: %v", err)
	}

	// Ensure bucket exists
	exists, err := client.BucketExists(ctx, bucketName)
	if err != nil {
		return nil, fmt.Errorf("failed to check if bucket exists: %v", err)
	}

	if !exists {
		err = client.MakeBucket(ctx, bucketName, minio.MakeBucketOptions{})
		if err != nil {
			return nil, fmt.Errorf("failed to create bucket: %v", err)
		}
	}

	// Set results bucket - same as main bucket for now, but could be different
	resultsBucket := os.Getenv("MINIO_RESULTS_BUCKET")
	if resultsBucket == "" {
		resultsBucket = bucketName // Use same bucket by default
	}

	return &minIOLogger{
		client:        client,
		bucketName:    bucketName,
		resultsBucket: resultsBucket,
		mutexes:       make(map[string]*sync.Mutex),
		mutexesMu:     sync.RWMutex{},
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
	fmt.Printf("DEBUG: Attempting to append logs to MinIO path: %s, bucket: %s\n", objectPath, m.bucketName)

	// Read existing content if file exists
	var existingContent []byte
	obj, err := m.client.GetObject(ctx, m.bucketName, objectPath, minio.GetObjectOptions{})
	if err != nil {
		// Check if error is NoSuchKey (file doesn't exist) using string contains
		fmt.Printf("DEBUG: GetObject error: %v\n", err)
		if strings.Contains(err.Error(), "NoSuchKey") || strings.Contains(err.Error(), "The specified key does not exist") {
			fmt.Printf("DEBUG: File doesn't exist, will create new one\n")
			// File doesn't exist, that's okay - we'll create it
		} else {
			return fmt.Errorf("failed to read existing log file: %v", err)
		}
	} else {
		fmt.Printf("DEBUG: GetObject succeeded, reading content\n")
		existingContent, err = io.ReadAll(obj)
		obj.Close()
		if err != nil {
			// Check if this is also a NoSuchKey error (race condition)
			fmt.Printf("DEBUG: ReadAll error: %v\n", err)
			if strings.Contains(err.Error(), "NoSuchKey") || strings.Contains(err.Error(), "The specified key does not exist") {
				// File was deleted between GetObject and ReadAll, treat as if file doesn't exist
				fmt.Printf("DEBUG: ReadAll NoSuchKey, treating as empty file\n")
				existingContent = []byte{}
			} else {
				// For any other ReadAll error, treat as empty file and continue
				fmt.Printf("DEBUG: ReadAll error, treating as empty file and continuing\n")
				existingContent = []byte{}
			}
		} else {
			fmt.Printf("DEBUG: Successfully read %d bytes of existing content\n", len(existingContent))
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
	fmt.Printf("DEBUG: Writing %d bytes to MinIO\n", buffer.Len())
	_, err = m.client.PutObject(ctx, m.bucketName, objectPath, reader, int64(buffer.Len()), minio.PutObjectOptions{
		ContentType: "text/plain",
	})
	if err != nil {
		fmt.Printf("DEBUG: PutObject failed: %v\n", err)
		return fmt.Errorf("failed to write logs to MinIO: %v", err)
	}

	fmt.Printf("DEBUG: Successfully wrote logs to MinIO path: %s\n", objectPath)
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
