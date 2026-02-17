package daemon

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"runtime/internal/util"
)

// MockMinIOLogger implements miniologger.MinIOLogger for testing.
type MockMinIOLogger struct {
	AppendCalls []struct {
		BotID   string
		Content string
	}
	StoreFinalCalls []struct {
		BotID   string
		Content string
	}
	AppendError     error
	StoreFinalError error
	CloseError      error
	CloseCalled     bool
}

func (m *MockMinIOLogger) AppendBotLogs(ctx context.Context, botID, content string) error {
	m.AppendCalls = append(m.AppendCalls, struct {
		BotID   string
		Content string
	}{BotID: botID, Content: content})
	return m.AppendError
}

func (m *MockMinIOLogger) StoreFinalLogs(ctx context.Context, botID, content string) error {
	m.StoreFinalCalls = append(m.StoreFinalCalls, struct {
		BotID   string
		Content string
	}{BotID: botID, Content: content})
	return m.StoreFinalError
}

func (m *MockMinIOLogger) Close() error {
	m.CloseCalled = true
	return m.CloseError
}

// MockLogPublisher implements LogPublisher for testing.
type MockLogPublisher struct {
	PublishCalls []struct {
		BotID   string
		Content string
	}
	PublishError error
	CloseCalled  bool
	CloseError   error
}

func (m *MockLogPublisher) Publish(botID string, content string) error {
	m.PublishCalls = append(m.PublishCalls, struct {
		BotID   string
		Content string
	}{BotID: botID, Content: content})
	return m.PublishError
}

func (m *MockLogPublisher) Close() error {
	m.CloseCalled = true
	return m.CloseError
}

func TestNewLogsSyncer_NilUploader(t *testing.T) {
	syncer := NewLogsSyncer("bot-1", "/logs", nil, nil, &util.DefaultLogger{})
	assert.Nil(t, syncer, "should return nil when uploader is nil")
}

func TestNewLogsSyncer_ValidUploader(t *testing.T) {
	mockUploader := &MockMinIOLogger{}
	syncer := NewLogsSyncer("bot-1", "/logs", mockUploader, nil, &util.DefaultLogger{})

	require.NotNil(t, syncer)
	assert.Equal(t, "bot-1", syncer.botID)
	assert.Equal(t, "/logs", syncer.logsPath)
}

func TestNewLogsSyncer_NilPublisher(t *testing.T) {
	mockUploader := &MockMinIOLogger{}
	syncer := NewLogsSyncer("bot-1", "/logs", mockUploader, nil, &util.DefaultLogger{})

	require.NotNil(t, syncer)
	assert.Nil(t, syncer.natsPublisher, "natsPublisher should be nil when not provided")
}

func TestLogsSyncer_Sync_NoLogFile(t *testing.T) {
	tmpDir := t.TempDir()
	mockUploader := &MockMinIOLogger{}

	syncer := NewLogsSyncer("bot-1", tmpDir, mockUploader, nil, &util.DefaultLogger{})
	synced := syncer.Sync(context.Background())

	assert.False(t, synced, "should return false when log file doesn't exist")
	assert.Empty(t, mockUploader.AppendCalls, "should not call uploader when no log file")
}

func TestLogsSyncer_Sync_EmptyLogFile(t *testing.T) {
	tmpDir := t.TempDir()
	logFile := filepath.Join(tmpDir, "bot.log")

	// Create empty log file
	require.NoError(t, os.WriteFile(logFile, []byte{}, 0644))

	mockUploader := &MockMinIOLogger{}
	syncer := NewLogsSyncer("bot-1", tmpDir, mockUploader, nil, &util.DefaultLogger{})
	synced := syncer.Sync(context.Background())

	assert.False(t, synced, "should return false for empty log file")
	assert.Empty(t, mockUploader.AppendCalls, "should not call uploader for empty log file")
}

func TestLogsSyncer_Sync_FirstSync(t *testing.T) {
	tmpDir := t.TempDir()
	logFile := filepath.Join(tmpDir, "bot.log")

	// Create log file with content
	content := "2024-01-01 10:00:00 Bot started\n2024-01-01 10:00:01 Processing...\n"
	require.NoError(t, os.WriteFile(logFile, []byte(content), 0644))

	mockUploader := &MockMinIOLogger{}
	syncer := NewLogsSyncer("bot-1", tmpDir, mockUploader, nil, &util.DefaultLogger{})
	synced := syncer.Sync(context.Background())

	assert.True(t, synced, "should return true when logs are synced")
	require.Len(t, mockUploader.AppendCalls, 1)
	assert.Equal(t, "bot-1", mockUploader.AppendCalls[0].BotID)
	assert.Equal(t, content, mockUploader.AppendCalls[0].Content)
}

func TestLogsSyncer_Sync_IncrementalSync(t *testing.T) {
	tmpDir := t.TempDir()
	logFile := filepath.Join(tmpDir, "bot.log")

	// Create initial log file
	initial := "Line 1\n"
	require.NoError(t, os.WriteFile(logFile, []byte(initial), 0644))

	mockUploader := &MockMinIOLogger{}
	syncer := NewLogsSyncer("bot-1", tmpDir, mockUploader, nil, &util.DefaultLogger{})

	// First sync
	synced := syncer.Sync(context.Background())
	assert.True(t, synced)
	require.Len(t, mockUploader.AppendCalls, 1)
	assert.Equal(t, initial, mockUploader.AppendCalls[0].Content)

	// Append more content
	additional := "Line 2\nLine 3\n"
	f, err := os.OpenFile(logFile, os.O_APPEND|os.O_WRONLY, 0644)
	require.NoError(t, err)
	_, err = f.WriteString(additional)
	require.NoError(t, err)
	f.Close()

	// Second sync should only get new content
	synced = syncer.Sync(context.Background())
	assert.True(t, synced)
	require.Len(t, mockUploader.AppendCalls, 2)
	assert.Equal(t, additional, mockUploader.AppendCalls[1].Content)
}

func TestLogsSyncer_Sync_NoNewContent(t *testing.T) {
	tmpDir := t.TempDir()
	logFile := filepath.Join(tmpDir, "bot.log")

	content := "Line 1\n"
	require.NoError(t, os.WriteFile(logFile, []byte(content), 0644))

	mockUploader := &MockMinIOLogger{}
	syncer := NewLogsSyncer("bot-1", tmpDir, mockUploader, nil, &util.DefaultLogger{})

	// First sync
	syncer.Sync(context.Background())
	require.Len(t, mockUploader.AppendCalls, 1)

	// Second sync with no changes
	synced := syncer.Sync(context.Background())
	assert.False(t, synced, "should return false when no new content")
	assert.Len(t, mockUploader.AppendCalls, 1, "should not call uploader when no new content")
}

func TestLogsSyncer_Sync_UploadError(t *testing.T) {
	tmpDir := t.TempDir()
	logFile := filepath.Join(tmpDir, "bot.log")

	require.NoError(t, os.WriteFile(logFile, []byte("content\n"), 0644))

	mockUploader := &MockMinIOLogger{
		AppendError: assert.AnError,
	}
	syncer := NewLogsSyncer("bot-1", tmpDir, mockUploader, nil, &util.DefaultLogger{})

	synced := syncer.Sync(context.Background())
	assert.False(t, synced, "should return false on upload error")
}

func TestLogsSyncer_Sync_LargeFileChunking(t *testing.T) {
	tmpDir := t.TempDir()
	logFile := filepath.Join(tmpDir, "bot.log")

	// Create a 2.5MB log file (should be split into 3 chunks: 1MB + 1MB + 0.5MB)
	chunkSize := 1024 * 1024 // 1MB
	content := strings.Repeat("A", chunkSize*2+chunkSize/2)
	require.NoError(t, os.WriteFile(logFile, []byte(content), 0644))

	mockUploader := &MockMinIOLogger{}
	mockPublisher := &MockLogPublisher{}
	syncer := NewLogsSyncer("bot-1", tmpDir, mockUploader, mockPublisher, &util.DefaultLogger{})

	synced := syncer.Sync(context.Background())

	assert.True(t, synced, "should return true when logs are synced")
	require.Len(t, mockUploader.AppendCalls, 3, "should split into 3 chunks")
	require.Len(t, mockPublisher.PublishCalls, 3, "publisher should also get 3 chunks")

	// Verify chunk sizes
	assert.Len(t, mockUploader.AppendCalls[0].Content, chunkSize, "first chunk should be 1MB")
	assert.Len(t, mockUploader.AppendCalls[1].Content, chunkSize, "second chunk should be 1MB")
	assert.Len(t, mockUploader.AppendCalls[2].Content, chunkSize/2, "third chunk should be 0.5MB")

	// Verify total content is preserved
	total := mockUploader.AppendCalls[0].Content + mockUploader.AppendCalls[1].Content + mockUploader.AppendCalls[2].Content
	assert.Equal(t, content, total, "all chunks combined should equal original content")
}

func TestLogsSyncer_Close(t *testing.T) {
	mockUploader := &MockMinIOLogger{}
	mockPublisher := &MockLogPublisher{}
	syncer := NewLogsSyncer("bot-1", "/logs", mockUploader, mockPublisher, &util.DefaultLogger{})

	err := syncer.Close()
	require.NoError(t, err)
	assert.True(t, mockUploader.CloseCalled)
	assert.True(t, mockPublisher.CloseCalled)
}

func TestLogsSyncer_Close_WithBothErrors(t *testing.T) {
	mockUploader := &MockMinIOLogger{
		CloseError: fmt.Errorf("uploader error"),
	}
	mockPublisher := &MockLogPublisher{
		CloseError: fmt.Errorf("publisher error"),
	}
	syncer := NewLogsSyncer("bot-1", "/logs", mockUploader, mockPublisher, &util.DefaultLogger{})

	err := syncer.Close()
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "uploader error")
	assert.Contains(t, err.Error(), "publisher error")
}

func TestLogsSyncer_Sync_WithNATSPublisher(t *testing.T) {
	tmpDir := t.TempDir()
	logFile := filepath.Join(tmpDir, "bot.log")

	content := "2024-01-01 10:00:00 Bot started\n"
	require.NoError(t, os.WriteFile(logFile, []byte(content), 0644))

	mockUploader := &MockMinIOLogger{}
	mockPublisher := &MockLogPublisher{}
	syncer := NewLogsSyncer("bot-1", tmpDir, mockUploader, mockPublisher, &util.DefaultLogger{})

	synced := syncer.Sync(context.Background())

	assert.True(t, synced, "should return true when logs are synced")
	require.Len(t, mockUploader.AppendCalls, 1)
	assert.Equal(t, content, mockUploader.AppendCalls[0].Content)

	// Verify NATS publisher was called with same content
	require.Len(t, mockPublisher.PublishCalls, 1)
	assert.Equal(t, "bot-1", mockPublisher.PublishCalls[0].BotID)
	assert.Equal(t, content, mockPublisher.PublishCalls[0].Content)
}

func TestLogsSyncer_Sync_NATSPublishError(t *testing.T) {
	tmpDir := t.TempDir()
	logFile := filepath.Join(tmpDir, "bot.log")

	content := "some log line\n"
	require.NoError(t, os.WriteFile(logFile, []byte(content), 0644))

	mockUploader := &MockMinIOLogger{}
	mockPublisher := &MockLogPublisher{
		PublishError: assert.AnError,
	}
	syncer := NewLogsSyncer("bot-1", tmpDir, mockUploader, mockPublisher, &util.DefaultLogger{})

	synced := syncer.Sync(context.Background())

	// Sync should still succeed even when NATS publish fails
	assert.True(t, synced, "should return true even when NATS publish fails")
	require.Len(t, mockUploader.AppendCalls, 1)
	require.Len(t, mockPublisher.PublishCalls, 1)
}
