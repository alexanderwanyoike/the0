package daemon

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"runtime/internal/util"
)

// MockMinIOLogger implements miniologger.MinIOLogger for testing.
type MockMinIOLogger struct {
	AppendCalls     []appendCall
	StoreFinalCalls []struct {
		BotID   string
		Content string
	}
	AppendError     error
	StoreFinalError error
	CloseError      error
	CloseCalled     bool
}

type appendCall struct {
	BotID   string
	Content string
	At      time.Time
}

func (m *MockMinIOLogger) AppendBotLogs(ctx context.Context, botID, content string, at time.Time) error {
	m.AppendCalls = append(m.AppendCalls, appendCall{BotID: botID, Content: content, At: at})
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

// slowDeadlineRecordingUploader records the ctx deadline and optionally blocks
// until the ctx is cancelled, simulating a long upload.
type slowDeadlineRecordingUploader struct {
	observedDeadline time.Time
	block            time.Duration
	returnErr        error
}

func (s *slowDeadlineRecordingUploader) AppendBotLogs(ctx context.Context, botID, content string, at time.Time) error {
	if d, ok := ctx.Deadline(); ok {
		s.observedDeadline = d
	}
	if s.block > 0 {
		select {
		case <-time.After(s.block):
		case <-ctx.Done():
			return ctx.Err()
		}
	}
	return s.returnErr
}
func (s *slowDeadlineRecordingUploader) StoreFinalLogs(ctx context.Context, id, content string) error {
	return nil
}
func (s *slowDeadlineRecordingUploader) Close() error { return nil }

func TestLogsSyncer_DefaultUploadTimeoutIs120s(t *testing.T) {
	recorder := &slowDeadlineRecordingUploader{}
	syncer := NewLogsSyncer("bot-1", "/logs", recorder, nil, &util.DefaultLogger{})
	require.NotNil(t, syncer)

	start := time.Now()
	err := syncer.uploadChunk(context.Background(), logChunk{content: "hello"})
	require.NoError(t, err)

	require.False(t, recorder.observedDeadline.IsZero(), "upload should have observed a ctx deadline")
	gotTimeout := recorder.observedDeadline.Sub(start)
	assert.InDelta(t, (120 * time.Second).Seconds(), gotTimeout.Seconds(), 1.0,
		"default upload timeout must be 120s (got %s)", gotTimeout)
}

func TestLogsSyncer_WithUploadTimeoutOverridesDefault(t *testing.T) {
	recorder := &slowDeadlineRecordingUploader{}
	syncer := NewLogsSyncer("bot-1", "/logs", recorder, nil, &util.DefaultLogger{}, WithLogsUploadTimeout(50*time.Millisecond))
	require.NotNil(t, syncer)

	start := time.Now()
	err := syncer.uploadChunk(context.Background(), logChunk{content: "hello"})
	require.NoError(t, err)

	gotTimeout := recorder.observedDeadline.Sub(start)
	assert.InDelta(t, (50 * time.Millisecond).Seconds(), gotTimeout.Seconds(), 0.1,
		"upload timeout should honor WithLogsUploadTimeout (got %s)", gotTimeout)
}

func TestLogsSyncer_UploadTimeoutEnforced(t *testing.T) {
	recorder := &slowDeadlineRecordingUploader{block: 500 * time.Millisecond}
	syncer := NewLogsSyncer("bot-1", "/logs", recorder, nil, &util.DefaultLogger{}, WithLogsUploadTimeout(50*time.Millisecond))
	require.NotNil(t, syncer)

	err := syncer.uploadChunk(context.Background(), logChunk{content: "hello"})
	require.Error(t, err, "upload must error when mock blocks past the configured timeout")
	assert.True(t, errors.Is(err, context.DeadlineExceeded),
		"error should wrap context.DeadlineExceeded, got %v", err)
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
	assert.Equal(t, []string{"2024-01-01 10:00:00 Bot started", "2024-01-01 10:00:01 Processing..."},
		entryMessages(t, mockUploader.AppendCalls[0].Content))
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
	assert.Equal(t, []string{"Line 1"}, entryMessages(t, mockUploader.AppendCalls[0].Content))

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
	assert.Equal(t, []string{"Line 2", "Line 3"}, entryMessages(t, mockUploader.AppendCalls[1].Content))
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

	// With no newline to split on, each raw chunk becomes one entry.
	var messages []string
	for i, call := range mockUploader.AppendCalls {
		assert.Equal(t, call.Content, mockPublisher.PublishCalls[i].Content, "chunk %d", i)
		messages = append(messages, entryMessages(t, call.Content)...)
	}
	require.Len(t, messages, 3)
	assert.Len(t, messages[0], chunkSize, "first chunk should be 1MB")
	assert.Len(t, messages[1], chunkSize, "second chunk should be 1MB")
	assert.Len(t, messages[2], chunkSize/2, "third chunk should be 0.5MB")
	assert.Equal(t, content, strings.Join(messages, ""), "all chunks combined should equal original content")
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
	assert.Equal(t, []string{"2024-01-01 10:00:00 Bot started"}, entryMessages(t, mockUploader.AppendCalls[0].Content))

	require.Len(t, mockPublisher.PublishCalls, 1)
	assert.Equal(t, "bot-1", mockPublisher.PublishCalls[0].BotID)
	assert.Equal(t, mockUploader.AppendCalls[0].Content, mockPublisher.PublishCalls[0].Content)
}

// orderRecordingUploader and orderRecordingPublisher share a slice to record
// the relative order of publish vs upload calls within a chunk.
type orderRecordingUploader struct {
	order *[]string
	err   error
}

func (o *orderRecordingUploader) AppendBotLogs(ctx context.Context, botID, content string, at time.Time) error {
	*o.order = append(*o.order, "upload")
	return o.err
}
func (o *orderRecordingUploader) StoreFinalLogs(ctx context.Context, botID, content string) error {
	return nil
}
func (o *orderRecordingUploader) Close() error { return nil }

type orderRecordingPublisher struct {
	order *[]string
}

func (o *orderRecordingPublisher) Publish(botID string, content string) error {
	*o.order = append(*o.order, "publish")
	return nil
}
func (o *orderRecordingPublisher) Close() error { return nil }

func TestLogsSyncer_Sync_PublishesToNATSBeforeUpload(t *testing.T) {
	tmpDir := t.TempDir()
	logFile := filepath.Join(tmpDir, "bot.log")
	require.NoError(t, os.WriteFile(logFile, []byte("line 1\n"), 0644))

	var order []string
	uploader := &orderRecordingUploader{order: &order}
	publisher := &orderRecordingPublisher{order: &order}
	syncer := NewLogsSyncer("bot-1", tmpDir, uploader, publisher, &util.DefaultLogger{})

	synced := syncer.Sync(context.Background())

	assert.True(t, synced)
	assert.Equal(t, []string{"publish", "upload"}, order,
		"NATS publish (liveness) must happen before MinIO upload (durability)")
}

func TestLogsSyncer_Sync_PublishesToNATSWhenUploadFails(t *testing.T) {
	tmpDir := t.TempDir()
	logFile := filepath.Join(tmpDir, "bot.log")

	content := "important live line\n"
	require.NoError(t, os.WriteFile(logFile, []byte(content), 0644))

	mockUploader := &MockMinIOLogger{AppendError: assert.AnError}
	mockPublisher := &MockLogPublisher{}
	syncer := NewLogsSyncer("bot-1", tmpDir, mockUploader, mockPublisher, &util.DefaultLogger{})

	synced := syncer.Sync(context.Background())

	assert.False(t, synced, "sync reports failure so durability is retried")
	require.Len(t, mockPublisher.PublishCalls, 1,
		"chunk must still reach live viewers when MinIO upload fails")
	assert.Equal(t, []string{"important live line"}, entryMessages(t, mockPublisher.PublishCalls[0].Content))
}

func TestLogsSyncer_Sync_RetriesUploadOfSameChunkAfterFailure(t *testing.T) {
	tmpDir := t.TempDir()
	logFile := filepath.Join(tmpDir, "bot.log")

	content := "line to retry\n"
	require.NoError(t, os.WriteFile(logFile, []byte(content), 0644))

	mockUploader := &MockMinIOLogger{AppendError: assert.AnError}
	mockPublisher := &MockLogPublisher{}
	syncer := NewLogsSyncer("bot-1", tmpDir, mockUploader, mockPublisher, &util.DefaultLogger{})

	// First sync: upload fails, offset must not advance
	assert.False(t, syncer.Sync(context.Background()))
	require.Len(t, mockUploader.AppendCalls, 1)

	// Upload recovers: the same chunk is uploaded again so durability never
	// drops data (the re-publish to NATS is accepted at-least-once behavior)
	mockUploader.AppendError = nil
	assert.True(t, syncer.Sync(context.Background()))
	require.Len(t, mockUploader.AppendCalls, 2)
	assert.Equal(t, mockUploader.AppendCalls[0].Content, mockUploader.AppendCalls[1].Content,
		"a retry resends the bytes it formatted the first time, keys and timestamps included")
	assert.True(t, mockUploader.AppendCalls[0].At.Equal(mockUploader.AppendCalls[1].At),
		"a retry goes to the daily file of its first attempt")
	require.Len(t, mockPublisher.PublishCalls, 2)
	assert.Equal(t, mockPublisher.PublishCalls[0].Content, mockPublisher.PublishCalls[1].Content)
	assert.Equal(t, []string{"line to retry"}, entryMessages(t, mockUploader.AppendCalls[1].Content))
}

func TestLogsSyncer_Sync_RetriesSameChunkWhenMoreLinesArrived(t *testing.T) {
	tmpDir := t.TempDir()
	logFile := filepath.Join(tmpDir, "bot.log")
	require.NoError(t, os.WriteFile(logFile, []byte("first\n"), 0644))

	mockUploader := &MockMinIOLogger{AppendError: assert.AnError}
	syncer := NewLogsSyncer("bot-1", tmpDir, mockUploader, nil, &util.DefaultLogger{})
	assert.False(t, syncer.Sync(context.Background()))

	f, err := os.OpenFile(logFile, os.O_APPEND|os.O_WRONLY, 0644)
	require.NoError(t, err)
	_, err = f.WriteString("second\n")
	require.NoError(t, err)
	require.NoError(t, f.Close())

	mockUploader.AppendError = nil
	assert.True(t, syncer.Sync(context.Background()))

	require.Len(t, mockUploader.AppendCalls, 3)
	assert.Equal(t, mockUploader.AppendCalls[0].Content, mockUploader.AppendCalls[1].Content,
		"the failed chunk is resent unchanged, not merged with the lines that arrived since")
	assert.Equal(t, []string{"second"}, entryMessages(t, mockUploader.AppendCalls[2].Content))
}

func TestLogsSyncer_Sync_KeysAreUniqueAcrossChunks(t *testing.T) {
	tmpDir := t.TempDir()
	logFile := filepath.Join(tmpDir, "bot.log")
	require.NoError(t, os.WriteFile(logFile, []byte("a\nb\n"), 0644))

	mockUploader := &MockMinIOLogger{}
	syncer := NewLogsSyncer("bot-1", tmpDir, mockUploader, nil, &util.DefaultLogger{})
	require.True(t, syncer.Sync(context.Background()))

	f, err := os.OpenFile(logFile, os.O_APPEND|os.O_WRONLY, 0644)
	require.NoError(t, err)
	_, err = f.WriteString("c\n")
	require.NoError(t, err)
	require.NoError(t, f.Close())
	require.True(t, syncer.Sync(context.Background()))

	seen := map[string]bool{}
	for _, call := range mockUploader.AppendCalls {
		for _, entry := range decodeEntries(t, call.Content) {
			key, ok := entry["_key"].(string)
			require.True(t, ok, "every entry has a string _key")
			assert.False(t, seen[key], "duplicate key %s", key)
			seen[key] = true
		}
	}
	assert.Len(t, seen, 3)
}

func TestLogsSyncer_Sync_StoresUnderTheTimeInTheKeys(t *testing.T) {
	tmpDir := t.TempDir()
	require.NoError(t, os.WriteFile(filepath.Join(tmpDir, "bot.log"), []byte("a\nb\n"), 0644))

	mockUploader := &MockMinIOLogger{}
	syncer := NewLogsSyncer("bot-1", tmpDir, mockUploader, nil, &util.DefaultLogger{})
	require.True(t, syncer.Sync(context.Background()))

	require.Len(t, mockUploader.AppendCalls, 1)
	call := mockUploader.AppendCalls[0]
	for i, entry := range decodeEntries(t, call.Content) {
		assert.Equal(t, fmt.Sprintf("%d-%d", call.At.UnixNano(), i), entry["_key"],
			"the daily file is picked by the same time the keys carry")
	}
}

func TestLogsSyncer_Sync_BlankChunkSendsNothing(t *testing.T) {
	tmpDir := t.TempDir()
	logFile := filepath.Join(tmpDir, "bot.log")
	require.NoError(t, os.WriteFile(logFile, []byte("\n  \n"), 0644))

	mockUploader := &MockMinIOLogger{}
	mockPublisher := &MockLogPublisher{}
	syncer := NewLogsSyncer("bot-1", tmpDir, mockUploader, mockPublisher, &util.DefaultLogger{})
	syncer.Sync(context.Background())

	assert.Empty(t, mockUploader.AppendCalls)
	assert.Empty(t, mockPublisher.PublishCalls, "live viewers get no empty messages")
}

// decodeEntries parses an NDJSON chunk the syncer sent, failing the test on
// any line that is not a JSON object with a timestamp and an entry key.
func decodeEntries(t *testing.T, content string) []map[string]interface{} {
	t.Helper()
	require.True(t, strings.HasSuffix(content, "\n"), "chunk should end with a newline: %q", content)
	var entries []map[string]interface{}
	for _, line := range strings.Split(strings.TrimSuffix(content, "\n"), "\n") {
		var entry map[string]interface{}
		require.NoError(t, json.Unmarshal([]byte(line), &entry), "line should be a JSON object: %s", line)
		require.Contains(t, entry, "timestamp")
		require.Contains(t, entry, "_key")
		entries = append(entries, entry)
	}
	return entries
}

func entryMessages(t *testing.T, content string) []string {
	t.Helper()
	var messages []string
	for _, entry := range decodeEntries(t, content) {
		message, _ := entry["message"].(string)
		messages = append(messages, message)
	}
	return messages
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
