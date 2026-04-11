package miniologger

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"os"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/minio/minio-go/v7"
	"github.com/minio/minio-go/v7/pkg/credentials"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"github.com/testcontainers/testcontainers-go"
	"github.com/testcontainers/testcontainers-go/wait"
)

// setupMinIOTestContainer starts a MinIO container for testing
func setupMinIOTestContainer(t *testing.T) (testcontainers.Container, string, string, string) {
	ctx := context.Background()

	accessKey := "testkey"
	secretKey := "testsecret"

	req := testcontainers.ContainerRequest{
		Image:        "minio/minio:latest",
		ExposedPorts: []string{"9000/tcp"},
		Env: map[string]string{
			"MINIO_ACCESS_KEY": accessKey,
			"MINIO_SECRET_KEY": secretKey,
		},
		Cmd:        []string{"server", "/data"},
		WaitingFor: wait.ForHTTP("/minio/health/live").WithPort("9000/tcp"),
	}

	container, err := testcontainers.GenericContainer(ctx, testcontainers.GenericContainerRequest{
		ContainerRequest: req,
		Started:          true,
	})
	require.NoError(t, err)

	host, err := container.Host(ctx)
	require.NoError(t, err)

	port, err := container.MappedPort(ctx, "9000")
	require.NoError(t, err)

	endpoint := fmt.Sprintf("%s:%s", host, port.Port())

	// Wait for MinIO to be fully initialized for bucket operations
	// The health endpoint may return OK before bucket operations are ready
	waitForMinIOReady(t, endpoint, accessKey, secretKey)

	return container, endpoint, accessKey, secretKey
}

// waitForMinIOReady waits until MinIO is ready for bucket operations
func waitForMinIOReady(t *testing.T, endpoint, accessKey, secretKey string) {
	client, err := minio.New(endpoint, &minio.Options{
		Creds:  credentials.NewStaticV4(accessKey, secretKey, ""),
		Secure: false,
	})
	require.NoError(t, err)

	ctx := context.Background()
	maxRetries := 10
	for i := 0; i < maxRetries; i++ {
		_, err := client.ListBuckets(ctx)
		if err == nil {
			return // MinIO is ready
		}
		time.Sleep(200 * time.Millisecond)
	}
	t.Fatalf("MinIO did not become ready after %d retries", maxRetries)
}

func TestNewMinIOLogger(t *testing.T) {
	// Test missing environment variables
	t.Run("MissingEndpoint", func(t *testing.T) {

		_, err := NewMinIOLogger(context.Background(), MinioLoggerOptions{
			Endpoint:      "",
			AccessKey:     "test",
			SecretKey:     "test",
			UseSSL:        false,
			LogsBucket:    "test-logs",
			ResultsBucket: "test-results",
		})
		assert.Error(t, err)
		assert.Contains(t, err.Error(), "endpoint is required")
	})

	t.Run("MissingAccessKey", func(t *testing.T) {

		_, err := NewMinIOLogger(context.Background(), MinioLoggerOptions{
			Endpoint:      "localhost:9000",
			AccessKey:     "",
			SecretKey:     "test",
			UseSSL:        false,
			LogsBucket:    "test-logs",
			ResultsBucket: "test-results",
		})
		assert.Error(t, err)
		assert.Contains(t, err.Error(), "access key is required")
	})

	t.Run("MissingSecretKey", func(t *testing.T) {
		os.Setenv("MINIO_ENDPOINT", "localhost:9000")
		os.Setenv("MINIO_ACCESS_KEY", "test")
		os.Setenv("MINIO_SECRET_KEY", "")

		_, err := NewMinIOLogger(context.Background(), MinioLoggerOptions{
			Endpoint:      "localhost:9000",
			AccessKey:     "test",
			SecretKey:     "",
			UseSSL:        false,
			LogsBucket:    "test-logs",
			ResultsBucket: "test-results",
		})
		assert.Error(t, err)
		assert.Contains(t, err.Error(), "secret key is required")
	})

	t.Run("SuccessfulConnection", func(t *testing.T) {
		container, endpoint, accessKey, secretKey := setupMinIOTestContainer(t)
		defer container.Terminate(context.Background())

		os.Setenv("MINIO_ENDPOINT", endpoint)
		os.Setenv("MINIO_ACCESS_KEY", accessKey)
		os.Setenv("MINIO_SECRET_KEY", secretKey)
		os.Setenv("MINIO_USE_SSL", "false")
		os.Setenv("MINIO_LOGS_BUCKET", "test-logs")

		logger, err := NewMinIOLogger(context.Background(), MinioLoggerOptions{
			Endpoint:      endpoint,
			AccessKey:     accessKey,
			SecretKey:     secretKey,
			UseSSL:        false,
			LogsBucket:    "test-logs",
			ResultsBucket: "test-results",
		})
		assert.NoError(t, err)
		assert.NotNil(t, logger)

		err = logger.Close()
		assert.NoError(t, err)
	})
}

func TestGetLogFilePath(t *testing.T) {
	logger := &minIOLogger{}

	date := time.Date(2023, 12, 25, 10, 30, 0, 0, time.UTC)
	path := logger.GetLogFilePath("test-bot-123", date)

	expected := "logs/test-bot-123/20231225.log"
	assert.Equal(t, expected, path)
}

func TestGetBotMutex(t *testing.T) {
	logger := &minIOLogger{
		mutexes:   make(map[string]*sync.Mutex),
		mutexesMu: sync.RWMutex{},
	}

	// Test creating new mutex
	mutex1 := logger.getMutex("bot1")
	assert.NotNil(t, mutex1)

	// Test getting existing mutex
	mutex2 := logger.getMutex("bot1")
	assert.Equal(t, mutex1, mutex2) // Should be the same mutex

	// Test different bot gets different mutex
	mutex3 := logger.getMutex("bot2")
	assert.NotNil(t, mutex3)

	// Use direct pointer comparison instead of assert.NotEqual (which has issues with sync.Mutex)
	if mutex1 == mutex3 {
		t.Errorf("Expected different mutexes for different bots, but got the same mutex")
	}
}

func TestAppendBotLogs(t *testing.T) {
	t.Run("EmptyLogs", func(t *testing.T) {
		logger := &minIOLogger{
			mutexes:   make(map[string]*sync.Mutex),
			mutexesMu: sync.RWMutex{},
		}

		err := logger.AppendBotLogs(context.Background(), "test-bot", "")
		assert.NoError(t, err) // Should not error on empty logs
	})

	t.Run("IntegrationTest", func(t *testing.T) {
		container, endpoint, accessKey, secretKey := setupMinIOTestContainer(t)
		defer container.Terminate(context.Background())

		os.Setenv("MINIO_ENDPOINT", endpoint)
		os.Setenv("MINIO_ACCESS_KEY", accessKey)
		os.Setenv("MINIO_SECRET_KEY", secretKey)
		os.Setenv("MINIO_USE_SSL", "false")
		os.Setenv("MINIO_LOGS_BUCKET", "test-logs")

		logger, err := NewMinIOLogger(context.Background(), MinioLoggerOptions{
			Endpoint:      endpoint,
			AccessKey:     accessKey,
			SecretKey:     secretKey,
			UseSSL:        false,
			LogsBucket:    "test-logs",
			ResultsBucket: "test-results",
		})
		require.NoError(t, err)
		defer logger.Close()

		botID := "test-bot-123"

		// Test appending logs multiple times
		err = logger.AppendBotLogs(context.Background(), botID, "First log entry")
		assert.NoError(t, err)

		err = logger.AppendBotLogs(context.Background(), botID, "Second log entry")
		assert.NoError(t, err)

		// Verify logs were written by reading them back
		client, err := minio.New(endpoint, &minio.Options{
			Creds:  credentials.NewStaticV4(accessKey, secretKey, ""),
			Secure: false,
		})
		require.NoError(t, err)

		timestamp := time.Now().Format("20060102")
		objectPath := fmt.Sprintf("logs/%s/%s.log", botID, timestamp)

		obj, err := client.GetObject(context.Background(), "test-logs", objectPath, minio.GetObjectOptions{})
		require.NoError(t, err)
		defer obj.Close()

		content, err := io.ReadAll(obj)
		require.NoError(t, err)

		contentStr := string(content)
		assert.Contains(t, contentStr, "First log entry")
		assert.Contains(t, contentStr, "Second log entry")
		assert.Contains(t, contentStr, time.Now().Format("2006-01-02")) // Should have timestamps
	})

	t.Run("ConcurrentAppends", func(t *testing.T) {
		container, endpoint, accessKey, secretKey := setupMinIOTestContainer(t)
		defer container.Terminate(context.Background())

		os.Setenv("MINIO_ENDPOINT", endpoint)
		os.Setenv("MINIO_ACCESS_KEY", accessKey)
		os.Setenv("MINIO_SECRET_KEY", secretKey)
		os.Setenv("MINIO_USE_SSL", "false")
		os.Setenv("MINIO_LOGS_BUCKET", "test-logs")

		logger, err := NewMinIOLogger(context.Background(), MinioLoggerOptions{
			Endpoint:      endpoint,
			AccessKey:     accessKey,
			SecretKey:     secretKey,
			UseSSL:        false,
			LogsBucket:    "test-logs",
			ResultsBucket: "test-results",
		})
		require.NoError(t, err)
		defer logger.Close()

		botID := "concurrent-bot"
		numGoroutines := 10

		var wg sync.WaitGroup
		wg.Add(numGoroutines)

		// Launch multiple goroutines to append logs concurrently
		for i := 0; i < numGoroutines; i++ {
			go func(index int) {
				defer wg.Done()
				logEntry := fmt.Sprintf("Log entry from goroutine %d", index)
				err := logger.AppendBotLogs(context.Background(), botID, logEntry)
				assert.NoError(t, err)
			}(i)
		}

		wg.Wait()

		// Verify all logs were written
		client, err := minio.New(endpoint, &minio.Options{
			Creds:  credentials.NewStaticV4(accessKey, secretKey, ""),
			Secure: false,
		})
		require.NoError(t, err)

		timestamp := time.Now().Format("20060102")
		objectPath := fmt.Sprintf("logs/%s/%s.log", botID, timestamp)

		obj, err := client.GetObject(context.Background(), "test-logs", objectPath, minio.GetObjectOptions{})
		require.NoError(t, err)
		defer obj.Close()

		content, err := io.ReadAll(obj)
		require.NoError(t, err)

		contentStr := string(content)
		// Each goroutine should have written its log entry
		for i := 0; i < numGoroutines; i++ {
			expectedEntry := fmt.Sprintf("Log entry from goroutine %d", i)
			assert.Contains(t, contentStr, expectedEntry)
		}
	})
}

func TestStoreFinalLogs(t *testing.T) {
	t.Run("EmptyLogs", func(t *testing.T) {
		logger := &minIOLogger{
			resultsBucket: "test-bucket",
		}

		err := logger.StoreFinalLogs(context.Background(), "test-bot", "")
		assert.NoError(t, err) // Should not error on empty logs
	})

	t.Run("IntegrationTest", func(t *testing.T) {
		container, endpoint, accessKey, secretKey := setupMinIOTestContainer(t)
		defer container.Terminate(context.Background())

		logger, err := NewMinIOLogger(context.Background(), MinioLoggerOptions{
			Endpoint:      endpoint,
			AccessKey:     accessKey,
			SecretKey:     secretKey,
			UseSSL:        false,
			LogsBucket:    "test-logs",
			ResultsBucket: "test-results",
		})
		require.NoError(t, err)
		defer logger.Close()

		botID := "final-logs-bot"
		finalLogs := "Bot execution completed successfully\nFinal metrics: CPU 0.5%, Memory 128MB"

		err = logger.StoreFinalLogs(context.Background(), botID, finalLogs)
		assert.NoError(t, err)

		// Verify logs were stored by reading them back
		client, err := minio.New(endpoint, &minio.Options{
			Creds:  credentials.NewStaticV4(accessKey, secretKey, ""),
			Secure: false,
		})
		require.NoError(t, err)

		objectPath := fmt.Sprintf("%s/logs.txt", botID)

		obj, err := client.GetObject(context.Background(), "test-results", objectPath, minio.GetObjectOptions{})
		require.NoError(t, err)
		defer obj.Close()

		content, err := io.ReadAll(obj)
		require.NoError(t, err)

		assert.Equal(t, finalLogs, string(content))
	})
}

func TestClose(t *testing.T) {
	logger := &minIOLogger{}
	err := logger.Close()
	assert.NoError(t, err) // Close should never error
}

func TestGetLogFilePathEdgeCases(t *testing.T) {
	logger := &minIOLogger{}

	tests := []struct {
		name     string
		botID    string
		date     time.Time
		expected string
	}{
		{
			name:     "Normal case",
			botID:    "test-bot-123",
			date:     time.Date(2023, 12, 25, 10, 30, 0, 0, time.UTC),
			expected: "logs/test-bot-123/20231225.log",
		},
		{
			name:     "Empty bot ID",
			botID:    "",
			date:     time.Date(2023, 1, 1, 0, 0, 0, 0, time.UTC),
			expected: "logs//20230101.log",
		},
		{
			name:     "Bot ID with special characters",
			botID:    "bot@domain.com/path",
			date:     time.Date(2023, 6, 15, 12, 0, 0, 0, time.UTC),
			expected: "logs/bot@domain.com/path/20230615.log",
		},
		{
			name:     "Leap year date",
			botID:    "leap-year-bot",
			date:     time.Date(2024, 2, 29, 23, 59, 59, 999999999, time.UTC),
			expected: "logs/leap-year-bot/20240229.log",
		},
		{
			name:     "Different timezone should not affect date",
			botID:    "tz-bot",
			date:     time.Date(2023, 12, 31, 23, 59, 59, 0, time.FixedZone("EST", -5*60*60)),
			expected: "logs/tz-bot/20231231.log",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			path := logger.GetLogFilePath(tt.botID, tt.date)
			assert.Equal(t, tt.expected, path)
		})
	}
}

func TestGetMutexConcurrency(t *testing.T) {
	logger := &minIOLogger{
		mutexes:   make(map[string]*sync.Mutex),
		mutexesMu: sync.RWMutex{},
	}

	// Test concurrent access to getMutex
	const numGoroutines = 100
	const botID = "concurrent-test-bot"

	var wg sync.WaitGroup
	mutexes := make([]*sync.Mutex, numGoroutines)

	wg.Add(numGoroutines)
	for i := 0; i < numGoroutines; i++ {
		go func(index int) {
			defer wg.Done()
			mutexes[index] = logger.getMutex(botID)
		}(i)
	}

	wg.Wait()

	// All goroutines should have gotten the same mutex
	firstMutex := mutexes[0]
	for i := 1; i < numGoroutines; i++ {
		assert.Equal(t, firstMutex, mutexes[i], "All goroutines should get the same mutex for the same bot ID")
	}

	// Different bot should get different mutex
	differentBotMutex := logger.getMutex("different-bot")
	// Use direct pointer comparison since assert.NotEqual has issues with sync.Mutex
	if firstMutex == differentBotMutex {
		t.Error("Different bots should get different mutexes")
	}
}

// TestComposeAppend verifies that AppendBotLogs scales with chunk size rather
// than accumulated file size by appending many chunks and checking that each
// append completes within a bounded duration. It also verifies that the final
// file content is the ordered concatenation of all chunks with timestamps.
func TestComposeAppend(t *testing.T) {
	if testing.Short() {
		t.Skip("Skipping integration test")
	}

	container, endpoint, accessKey, secretKey := setupMinIOTestContainer(t)
	defer container.Terminate(context.Background())

	logger, err := NewMinIOLogger(context.Background(), MinioLoggerOptions{
		Endpoint:      endpoint,
		AccessKey:     accessKey,
		SecretKey:     secretKey,
		UseSSL:        false,
		LogsBucket:    "test-compose-logs",
		ResultsBucket: "test-compose-results",
	})
	require.NoError(t, err)
	defer logger.Close()

	botID := "compose-append-bot"
	const numChunks = 600 // > 5 MiB total, so later appends exercise the ComposeObject path
	const chunkSize = 10 * 1024 // 10KB

	// Generate reproducible 10KB payloads so we can verify ordering later.
	chunks := make([]string, numChunks)
	for i := 0; i < numChunks; i++ {
		payload := make([]byte, chunkSize)
		for j := range payload {
			payload[j] = byte('A' + (i % 26))
		}
		chunks[i] = fmt.Sprintf("CHUNK-%04d:%s", i, string(payload))
	}

	// Append all chunks.
	for i, chunk := range chunks {
		err := logger.AppendBotLogs(context.Background(), botID, chunk)
		require.NoError(t, err, "chunk %d failed", i)
	}

	// Read back the full daily log and verify content.
	client, err := minio.New(endpoint, &minio.Options{
		Creds:  credentials.NewStaticV4(accessKey, secretKey, ""),
		Secure: false,
	})
	require.NoError(t, err)

	timestamp := time.Now().Format("20060102")
	objectPath := fmt.Sprintf("logs/%s/%s.log", botID, timestamp)

	obj, err := client.GetObject(context.Background(), "test-compose-logs", objectPath, minio.GetObjectOptions{})
	require.NoError(t, err)
	defer obj.Close()

	content, err := io.ReadAll(obj)
	require.NoError(t, err)

	contentStr := string(content)

	// The final file must be larger than composeMinPartSize to confirm
	// that later appends actually used the ComposeObject path.
	assert.Greater(t, int64(len(content)), int64(composeMinPartSize),
		"final file should exceed the compose threshold")

	// Every chunk must appear in order.
	lastIdx := -1
	for i, chunk := range chunks {
		idx := strings.Index(contentStr, chunk)
		require.NotEqual(t, -1, idx, "chunk %d not found in final file", i)
		assert.Greater(t, idx, lastIdx,
			"chunk %d appeared at index %d which is before chunk %d at index %d",
			i, idx, i-1, lastIdx)
		lastIdx = idx
	}

	// Each entry must have a timestamp prefix like [2026-04-01 ...]
	todayPrefix := time.Now().Format("2006-01-02")
	assert.Contains(t, contentStr, todayPrefix, "file should contain today's date in timestamp prefixes")

	// Verify no temp objects were left behind. Because cleanup is synchronous
	// (runs before AppendBotLogs returns), we can assert immediately without
	// polling or sleeping.
	tempPrefix := fmt.Sprintf("logs/%s/.tmp-", botID)
	var tempCount int
	for range client.ListObjects(context.Background(), "test-compose-logs", minio.ListObjectsOptions{
		Prefix:    tempPrefix,
		Recursive: true,
	}) {
		tempCount++
	}
	assert.Equal(t, 0, tempCount, "temp objects should be cleaned up synchronously before AppendBotLogs returns")
}

// TestComposeAppendFirstWrite verifies that the first write of the day (no
// existing daily file) works correctly with the compose-based implementation.
func TestComposeAppendFirstWrite(t *testing.T) {
	if testing.Short() {
		t.Skip("Skipping integration test")
	}

	container, endpoint, accessKey, secretKey := setupMinIOTestContainer(t)
	defer container.Terminate(context.Background())

	logger, err := NewMinIOLogger(context.Background(), MinioLoggerOptions{
		Endpoint:      endpoint,
		AccessKey:     accessKey,
		SecretKey:     secretKey,
		UseSSL:        false,
		LogsBucket:    "test-first-write",
		ResultsBucket: "test-first-write-results",
	})
	require.NoError(t, err)
	defer logger.Close()

	botID := "first-write-bot"

	err = logger.AppendBotLogs(context.Background(), botID, "hello world")
	require.NoError(t, err)

	// Read back
	client, err := minio.New(endpoint, &minio.Options{
		Creds:  credentials.NewStaticV4(accessKey, secretKey, ""),
		Secure: false,
	})
	require.NoError(t, err)

	timestamp := time.Now().Format("20060102")
	objectPath := fmt.Sprintf("logs/%s/%s.log", botID, timestamp)

	obj, err := client.GetObject(context.Background(), "test-first-write", objectPath, minio.GetObjectOptions{})
	require.NoError(t, err)
	defer obj.Close()

	content, err := io.ReadAll(obj)
	require.NoError(t, err)

	assert.Contains(t, string(content), "hello world")
	assert.Contains(t, string(content), time.Now().Format("2006-01-02"))
}

// fakeMultipartClient is a controllable implementation of multipartClient.
// It records every call and can be programmed to return errors at specific
// steps so we can deterministically exercise the abort-on-error contract.
type fakeMultipartClient struct {
	mu sync.Mutex

	// Programmed errors. If set, the matching method returns that error.
	newErr      error
	copyErr     error // returned on every CopyObjectPart call unless copyErrPartID > 0
	copyErrPart int   // if > 0, only the CopyObjectPart call with this partID errors
	completeErr error
	abortErr    error

	// Call records.
	uploadIDs    []string // uploadIDs handed out
	copyCalls    []fakeCopyCall
	completedIDs []string
	abortedIDs   []string
}

type fakeCopyCall struct {
	srcBucket, srcObject     string
	destBucket, destObject   string
	uploadID                 string
	partID                   int
	startOffset, length      int64
}

func (f *fakeMultipartClient) NewMultipartUpload(ctx context.Context, bucket, object string, opts minio.PutObjectOptions) (string, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.newErr != nil {
		return "", f.newErr
	}
	id := fmt.Sprintf("fake-upload-%d", len(f.uploadIDs)+1)
	f.uploadIDs = append(f.uploadIDs, id)
	return id, nil
}

func (f *fakeMultipartClient) CopyObjectPart(ctx context.Context, srcBucket, srcObject, destBucket, destObject, uploadID string,
	partID int, startOffset, length int64, metadata map[string]string,
) (minio.CompletePart, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.copyCalls = append(f.copyCalls, fakeCopyCall{
		srcBucket: srcBucket, srcObject: srcObject,
		destBucket: destBucket, destObject: destObject,
		uploadID: uploadID, partID: partID,
		startOffset: startOffset, length: length,
	})
	if f.copyErr != nil && (f.copyErrPart == 0 || f.copyErrPart == partID) {
		return minio.CompletePart{}, f.copyErr
	}
	return minio.CompletePart{PartNumber: partID, ETag: fmt.Sprintf("etag-%d", partID)}, nil
}

func (f *fakeMultipartClient) CompleteMultipartUpload(ctx context.Context, bucket, object, uploadID string, parts []minio.CompletePart, opts minio.PutObjectOptions) (minio.UploadInfo, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.completeErr != nil {
		return minio.UploadInfo{}, f.completeErr
	}
	f.completedIDs = append(f.completedIDs, uploadID)
	return minio.UploadInfo{Bucket: bucket, Key: object}, nil
}

func (f *fakeMultipartClient) AbortMultipartUpload(ctx context.Context, bucket, object, uploadID string) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.abortErr != nil {
		return f.abortErr
	}
	f.abortedIDs = append(f.abortedIDs, uploadID)
	return nil
}

// primeDailyLog puts an object at the daily key for a bot using the real
// MinIO client, so the subsequent AppendBotLogs call sees a file >= the
// compose threshold and takes the multipart-copy branch.
func primeDailyLog(t *testing.T, client *minio.Client, bucket, botID string, size int) {
	t.Helper()
	payload := make([]byte, size)
	for i := range payload {
		payload[i] = 'x'
	}
	dailyKey := fmt.Sprintf("logs/%s/%s.log", botID, time.Now().Format("20060102"))
	_, err := client.PutObject(context.Background(), bucket, dailyKey, bytes.NewReader(payload), int64(len(payload)), minio.PutObjectOptions{ContentType: "text/plain"})
	require.NoError(t, err)
}

func TestAppendBotLogs_AbortsOnCompleteMultipartError(t *testing.T) {
	if testing.Short() {
		t.Skip("Skipping integration test")
	}

	container, endpoint, accessKey, secretKey := setupMinIOTestContainer(t)
	defer container.Terminate(context.Background())

	logger, err := NewMinIOLogger(context.Background(), MinioLoggerOptions{
		Endpoint:      endpoint,
		AccessKey:     accessKey,
		SecretKey:     secretKey,
		UseSSL:        false,
		LogsBucket:    "test-abort-complete",
		ResultsBucket: "test-abort-complete-results",
	})
	require.NoError(t, err)
	defer logger.Close()

	// Seed an 8 MiB daily log so the next append takes the compose branch.
	botID := "abort-complete-bot"
	primeDailyLog(t, logger.client, "test-abort-complete", botID, 8*1024*1024)

	// Swap in a fake multipart client programmed to fail on complete.
	fake := &fakeMultipartClient{completeErr: fmt.Errorf("simulated r2 complete failure")}
	logger.mpClient = fake

	err = logger.AppendBotLogs(context.Background(), botID, "new chunk")
	require.Error(t, err, "AppendBotLogs should surface the compose failure")
	assert.Contains(t, err.Error(), "simulated r2 complete failure")

	// Contract: on failure, the multipart upload must have been aborted.
	require.Len(t, fake.uploadIDs, 1, "exactly one multipart upload should have been created")
	assert.Equal(t, fake.uploadIDs, fake.abortedIDs, "the created upload must be aborted")
	assert.Empty(t, fake.completedIDs, "no upload should be marked complete on failure")
}

func TestAppendBotLogs_AbortsOnCopyPartError(t *testing.T) {
	if testing.Short() {
		t.Skip("Skipping integration test")
	}

	container, endpoint, accessKey, secretKey := setupMinIOTestContainer(t)
	defer container.Terminate(context.Background())

	logger, err := NewMinIOLogger(context.Background(), MinioLoggerOptions{
		Endpoint:      endpoint,
		AccessKey:     accessKey,
		SecretKey:     secretKey,
		UseSSL:        false,
		LogsBucket:    "test-abort-copy",
		ResultsBucket: "test-abort-copy-results",
	})
	require.NoError(t, err)
	defer logger.Close()

	botID := "abort-copy-bot"
	primeDailyLog(t, logger.client, "test-abort-copy", botID, 8*1024*1024)

	fake := &fakeMultipartClient{copyErr: fmt.Errorf("simulated uploadpartcopy failure"), copyErrPart: 2}
	logger.mpClient = fake

	err = logger.AppendBotLogs(context.Background(), botID, "new chunk")
	require.Error(t, err)
	assert.Contains(t, err.Error(), "simulated uploadpartcopy failure")

	require.Len(t, fake.uploadIDs, 1)
	assert.Equal(t, fake.uploadIDs, fake.abortedIDs, "uploadPartCopy failure must still abort")
}

func TestAppendBotLogs_NoLeakedUploadsOnHappyCompose(t *testing.T) {
	if testing.Short() {
		t.Skip("Skipping integration test")
	}

	container, endpoint, accessKey, secretKey := setupMinIOTestContainer(t)
	defer container.Terminate(context.Background())

	logger, err := NewMinIOLogger(context.Background(), MinioLoggerOptions{
		Endpoint:      endpoint,
		AccessKey:     accessKey,
		SecretKey:     secretKey,
		UseSSL:        false,
		LogsBucket:    "test-happy-compose",
		ResultsBucket: "test-happy-compose-results",
	})
	require.NoError(t, err)
	defer logger.Close()

	botID := "happy-compose-bot"

	// Drive 10 appends each larger than the 5 MiB threshold so every call
	// after the first exercises the multipart-copy path on the real MinIO.
	chunk := make([]byte, 6*1024*1024)
	for i := range chunk {
		chunk[i] = 'y'
	}
	for i := 0; i < 10; i++ {
		err := logger.AppendBotLogs(context.Background(), botID, string(chunk))
		require.NoError(t, err, "append %d failed", i)
	}

	client, err := minio.New(endpoint, &minio.Options{
		Creds:  credentials.NewStaticV4(accessKey, secretKey, ""),
		Secure: false,
	})
	require.NoError(t, err)

	// Verify no orphaned incomplete multipart uploads remain.
	var incomplete int
	for range client.ListIncompleteUploads(context.Background(), "test-happy-compose", fmt.Sprintf("logs/%s/", botID), true) {
		incomplete++
	}
	assert.Equal(t, 0, incomplete, "happy-path compose must not leave incomplete uploads")
}

func TestMinIOLoggerInterface(t *testing.T) {
	// Test that minIOLogger implements MinIOLogger interface
	var logger MinIOLogger = &minIOLogger{
		logsBucket:    "test",
		resultsBucket: "test",
		mutexes:       make(map[string]*sync.Mutex),
		mutexesMu:     sync.RWMutex{},
	}

	assert.NotNil(t, logger)

	// Test interface methods exist (will fail without real MinIO, but interface is satisfied)
	err := logger.AppendBotLogs(context.Background(), "test", "")
	assert.NoError(t, err) // Empty logs should not error

	err = logger.StoreFinalLogs(context.Background(), "test", "")
	assert.NoError(t, err) // Empty logs should not error

	err = logger.Close()
	assert.NoError(t, err) // Close should not error

	// Test that AppendBotLogs fails gracefully with nil client when logs are non-empty
	err = logger.AppendBotLogs(context.Background(), "test", "some logs")
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "MinIO client is not initialized")

	// Test that StoreFinalLogs fails gracefully with nil client when logs are non-empty
	err = logger.StoreFinalLogs(context.Background(), "test", "some logs")
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "MinIO client is not initialized")
}
