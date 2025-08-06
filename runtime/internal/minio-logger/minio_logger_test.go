package miniologger

import (
	"context"
	"fmt"
	"io"
	"os"
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

	return container, endpoint, accessKey, secretKey
}

func TestNewMinIOLogger(t *testing.T) {
	// Test missing environment variables
	t.Run("MissingEndpoint", func(t *testing.T) {
		os.Setenv("MINIO_ENDPOINT", "")
		os.Setenv("MINIO_ACCESS_KEY", "test")
		os.Setenv("MINIO_SECRET_KEY", "test")

		_, err := NewMinIOLogger(context.Background())
		assert.Error(t, err)
		assert.Contains(t, err.Error(), "MINIO_ENDPOINT")
	})

	t.Run("MissingAccessKey", func(t *testing.T) {
		os.Setenv("MINIO_ENDPOINT", "localhost:9000")
		os.Setenv("MINIO_ACCESS_KEY", "")
		os.Setenv("MINIO_SECRET_KEY", "test")

		_, err := NewMinIOLogger(context.Background())
		assert.Error(t, err)
		assert.Contains(t, err.Error(), "MINIO_ACCESS_KEY")
	})

	t.Run("MissingSecretKey", func(t *testing.T) {
		os.Setenv("MINIO_ENDPOINT", "localhost:9000")
		os.Setenv("MINIO_ACCESS_KEY", "test")
		os.Setenv("MINIO_SECRET_KEY", "")

		_, err := NewMinIOLogger(context.Background())
		assert.Error(t, err)
		assert.Contains(t, err.Error(), "MINIO_SECRET_KEY")
	})

	t.Run("SuccessfulConnection", func(t *testing.T) {
		container, endpoint, accessKey, secretKey := setupMinIOTestContainer(t)
		defer container.Terminate(context.Background())

		os.Setenv("MINIO_ENDPOINT", endpoint)
		os.Setenv("MINIO_ACCESS_KEY", accessKey)
		os.Setenv("MINIO_SECRET_KEY", secretKey)
		os.Setenv("MINIO_USE_SSL", "false")
		os.Setenv("MINIO_LOGS_BUCKET", "test-logs")

		logger, err := NewMinIOLogger(context.Background())
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

		logger, err := NewMinIOLogger(context.Background())
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

		logger, err := NewMinIOLogger(context.Background())
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

		os.Setenv("MINIO_ENDPOINT", endpoint)
		os.Setenv("MINIO_ACCESS_KEY", accessKey)
		os.Setenv("MINIO_SECRET_KEY", secretKey)
		os.Setenv("MINIO_USE_SSL", "false")
		os.Setenv("MINIO_LOGS_BUCKET", "test-logs")

		logger, err := NewMinIOLogger(context.Background())
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

		obj, err := client.GetObject(context.Background(), "test-logs", objectPath, minio.GetObjectOptions{})
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

func TestMinIOLoggerInterface(t *testing.T) {
	// Test that minIOLogger implements MinIOLogger interface
	var logger MinIOLogger = &minIOLogger{
		bucketName:    "test",
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
