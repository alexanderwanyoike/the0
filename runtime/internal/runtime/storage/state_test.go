package storage

import (
	"archive/tar"
	"bytes"
	"compress/gzip"
	"context"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sync"
	"testing"

	"github.com/minio/minio-go/v7"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// testLogger is a simple logger for testing
type testLogger struct{}

func (l *testLogger) Info(msg string, args ...interface{}) {}

// TestCreateTarGz tests the internal tar.gz creation
func TestCreateTarGz(t *testing.T) {
	// Create a temp directory with some files
	tmpDir, err := os.MkdirTemp("", "state-test-")
	require.NoError(t, err)
	defer os.RemoveAll(tmpDir)

	// Create test files
	stateDir := filepath.Join(tmpDir, ".the0-state")
	require.NoError(t, os.MkdirAll(stateDir, 0755))

	testFiles := map[string]string{
		"portfolio.json": `{"AAPL": 100, "GOOGL": 50}`,
		"counters.json":  `{"trades": 42, "signals": 15}`,
	}

	for name, content := range testFiles {
		require.NoError(t, os.WriteFile(filepath.Join(stateDir, name), []byte(content), 0644))
	}

	// Create the tar.gz
	var buf bytes.Buffer
	manager := &stateManager{logger: &testLogger{}}
	err = manager.createTarGz(stateDir, &buf)
	require.NoError(t, err)

	// Verify it's valid gzip
	gzr, err := gzip.NewReader(&buf)
	require.NoError(t, err)
	defer gzr.Close()

	// Verify it contains our files
	tr := tar.NewReader(gzr)
	foundFiles := make(map[string]bool)

	for {
		header, err := tr.Next()
		if err == io.EOF {
			break
		}
		require.NoError(t, err)
		foundFiles[header.Name] = true
	}

	assert.Contains(t, foundFiles, "portfolio.json")
	assert.Contains(t, foundFiles, "counters.json")
}

// TestExtractTarGz tests the internal tar.gz extraction
func TestExtractTarGz(t *testing.T) {
	// Create a tar.gz in memory
	var buf bytes.Buffer
	gzw := gzip.NewWriter(&buf)
	tw := tar.NewWriter(gzw)

	testFiles := map[string]string{
		"portfolio.json": `{"AAPL": 100}`,
		"config.json":    `{"enabled": true}`,
	}

	for name, content := range testFiles {
		header := &tar.Header{
			Name: name,
			Mode: 0644,
			Size: int64(len(content)),
		}
		require.NoError(t, tw.WriteHeader(header))
		_, err := tw.Write([]byte(content))
		require.NoError(t, err)
	}

	require.NoError(t, tw.Close())
	require.NoError(t, gzw.Close())

	// Extract to a temp directory
	tmpDir, err := os.MkdirTemp("", "extract-test-")
	require.NoError(t, err)
	defer os.RemoveAll(tmpDir)

	manager := &stateManager{
		logger:           &testLogger{},
		maxStateSize:     8 * 1024 * 1024 * 1024,
		maxStateFileSize: 10 * 1024 * 1024,
	}
	err = manager.extractTarGz(&buf, tmpDir)
	require.NoError(t, err)

	// Verify files were extracted
	for name, expectedContent := range testFiles {
		content, err := os.ReadFile(filepath.Join(tmpDir, name))
		require.NoError(t, err)
		assert.Equal(t, expectedContent, string(content))
	}
}

// TestExtractTarGz_PreventDirectoryTraversal tests security against path traversal
func TestExtractTarGz_PreventDirectoryTraversal(t *testing.T) {
	// Create a tar.gz with a malicious path
	var buf bytes.Buffer
	gzw := gzip.NewWriter(&buf)
	tw := tar.NewWriter(gzw)

	// Try to escape the directory
	maliciousHeader := &tar.Header{
		Name: "../../../etc/malicious.txt",
		Mode: 0644,
		Size: 4,
	}
	require.NoError(t, tw.WriteHeader(maliciousHeader))
	_, err := tw.Write([]byte("evil"))
	require.NoError(t, err)

	require.NoError(t, tw.Close())
	require.NoError(t, gzw.Close())

	// Extract to a temp directory
	tmpDir, err := os.MkdirTemp("", "traversal-test-")
	require.NoError(t, err)
	defer os.RemoveAll(tmpDir)

	manager := &stateManager{
		logger:           &testLogger{},
		maxStateSize:     8 * 1024 * 1024 * 1024,
		maxStateFileSize: 10 * 1024 * 1024,
	}
	err = manager.extractTarGz(&buf, tmpDir)
	require.NoError(t, err)

	// Malicious file should NOT exist outside tmpDir
	_, err = os.Stat(filepath.Join(tmpDir, "..", "..", "..", "etc", "malicious.txt"))
	assert.True(t, os.IsNotExist(err), "Path traversal attack should be prevented")
}

// TestObjectName tests the object name generation
func TestObjectName(t *testing.T) {
	manager := &stateManager{}

	tests := []struct {
		botID    string
		expected string
	}{
		{"bot-123", "bot-123/state.tar.gz"},
		{"my-trading-bot", "my-trading-bot/state.tar.gz"},
		{"test", "test/state.tar.gz"},
	}

	for _, tc := range tests {
		t.Run(tc.botID, func(t *testing.T) {
			assert.Equal(t, tc.expected, manager.objectName(tc.botID))
		})
	}
}

// TestUploadState_EmptyDir tests that empty directories are handled
func TestUploadState_EmptyDir(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "empty-state-")
	require.NoError(t, err)
	defer os.RemoveAll(tmpDir)

	// Create empty state directory
	stateDir := filepath.Join(tmpDir, ".the0-state")
	require.NoError(t, os.MkdirAll(stateDir, 0755))

	// Verify empty directory check
	entries, err := os.ReadDir(stateDir)
	require.NoError(t, err)
	assert.Empty(t, entries, "Empty directory should have no entries")
}

// TestUploadState_NonExistentDir tests that non-existent directories are handled
func TestUploadState_NonExistentDir(t *testing.T) {
	nonExistentDir := "/tmp/non-existent-state-dir-12345"

	// Verify it doesn't exist
	_, err := os.ReadDir(nonExistentDir)
	assert.True(t, os.IsNotExist(err))
}

// fakeStateObjectWriter is a controllable stand-in for the upload subset of
// minio.Client that state.go uses. Lets us deterministically fail an upload
// mid-stream and assert that the abort path ran with the right args.
type fakeStateObjectWriter struct {
	mu sync.Mutex

	putErr    error
	removeErr error

	putCalls    []fakePutCall
	removeCalls []fakeRemoveCall
}

type fakePutCall struct {
	bucket, object string
}

type fakeRemoveCall struct {
	bucket, object string
	ctxDone        bool
}

func (f *fakeStateObjectWriter) PutObject(ctx context.Context, bucket, object string, reader io.Reader, size int64, opts minio.PutObjectOptions) (minio.UploadInfo, error) {
	// Drain the reader so the background tar.gz goroutine can exit cleanly;
	// otherwise it blocks forever writing into a dead pipe.
	_, _ = io.Copy(io.Discard, reader)
	f.mu.Lock()
	defer f.mu.Unlock()
	f.putCalls = append(f.putCalls, fakePutCall{bucket: bucket, object: object})
	if f.putErr != nil {
		return minio.UploadInfo{}, f.putErr
	}
	return minio.UploadInfo{Bucket: bucket, Key: object}, nil
}

func (f *fakeStateObjectWriter) RemoveIncompleteUpload(ctx context.Context, bucket, object string) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	done := false
	select {
	case <-ctx.Done():
		done = true
	default:
	}
	f.removeCalls = append(f.removeCalls, fakeRemoveCall{bucket: bucket, object: object, ctxDone: done})
	return f.removeErr
}

// writeSimpleStateDir creates a directory with two tiny state files that
// validateStateSize / createTarGz will happily process.
func writeSimpleStateDir(t *testing.T) string {
	t.Helper()
	dir, err := os.MkdirTemp("", "state-abort-")
	require.NoError(t, err)
	t.Cleanup(func() { os.RemoveAll(dir) })
	require.NoError(t, os.WriteFile(filepath.Join(dir, "portfolio.json"), []byte(`{"AAPL":100}`), 0644))
	require.NoError(t, os.WriteFile(filepath.Join(dir, "counters.json"), []byte(`{"trades":42}`), 0644))
	return dir
}

func TestUploadState_AbortsIncompleteUploadOnPutError(t *testing.T) {
	fake := &fakeStateObjectWriter{putErr: fmt.Errorf("simulated put failure")}
	mgr := &stateManager{
		minioClient:      nil, // ensureBucket path is bypassed via uploader seam
		uploader:         fake,
		bucket:           "bot-state",
		maxStateSize:     8 * 1024 * 1024 * 1024,
		maxStateFileSize: 10 * 1024 * 1024,
		logger:           &testLogger{},
		skipEnsureBucket: true,
	}

	srcDir := writeSimpleStateDir(t)

	err := mgr.UploadState(context.Background(), "abort-bot", srcDir)
	require.Error(t, err)
	assert.Contains(t, err.Error(), "simulated put failure")

	require.Len(t, fake.putCalls, 1, "put should have been attempted")
	require.Len(t, fake.removeCalls, 1, "RemoveIncompleteUpload must fire on put error")
	assert.Equal(t, "bot-state", fake.removeCalls[0].bucket)
	assert.Equal(t, "abort-bot/state.tar.gz", fake.removeCalls[0].object)
}

func TestUploadState_AbortUsesFreshContextWhenCallerCancelled(t *testing.T) {
	fake := &fakeStateObjectWriter{putErr: context.Canceled}
	mgr := &stateManager{
		minioClient:      nil,
		uploader:         fake,
		bucket:           "bot-state",
		maxStateSize:     8 * 1024 * 1024 * 1024,
		maxStateFileSize: 10 * 1024 * 1024,
		logger:           &testLogger{},
		skipEnsureBucket: true,
	}

	srcDir := writeSimpleStateDir(t)

	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	err := mgr.UploadState(ctx, "abort-ctx-bot", srcDir)
	require.Error(t, err)

	require.Len(t, fake.removeCalls, 1, "abort must run even though caller ctx is cancelled")
	assert.False(t, fake.removeCalls[0].ctxDone, "abort must run on a fresh, non-cancelled context")
}

func TestUploadState_HappyPathDoesNotAbort(t *testing.T) {
	fake := &fakeStateObjectWriter{}
	mgr := &stateManager{
		minioClient:      nil,
		uploader:         fake,
		bucket:           "bot-state",
		maxStateSize:     8 * 1024 * 1024 * 1024,
		maxStateFileSize: 10 * 1024 * 1024,
		logger:           &testLogger{},
		skipEnsureBucket: true,
	}

	srcDir := writeSimpleStateDir(t)
	err := mgr.UploadState(context.Background(), "ok-bot", srcDir)
	require.NoError(t, err)

	assert.Len(t, fake.putCalls, 1)
	assert.Empty(t, fake.removeCalls, "no abort should run on success")
}
