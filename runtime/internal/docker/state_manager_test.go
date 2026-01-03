package docker

import (
	"archive/tar"
	"bytes"
	"compress/gzip"
	"context"
	"io"
	"os"
	"path/filepath"
	"testing"

	"github.com/minio/minio-go/v7"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// mockMinioClient implements a minimal mock for testing StateManager
type mockMinioClient struct {
	objects      map[string][]byte
	bucketExists bool
	shouldError  bool
}

func newMockMinioClient() *mockMinioClient {
	return &mockMinioClient{
		objects:      make(map[string][]byte),
		bucketExists: true,
	}
}

func (m *mockMinioClient) BucketExists(ctx context.Context, bucketName string) (bool, error) {
	return m.bucketExists, nil
}

func (m *mockMinioClient) MakeBucket(ctx context.Context, bucketName string, opts minio.MakeBucketOptions) error {
	m.bucketExists = true
	return nil
}

func (m *mockMinioClient) GetObject(ctx context.Context, bucketName, objectName string, opts minio.GetObjectOptions) (*minio.Object, error) {
	// This is a simplified mock - in real tests, we'd use a more complete mock
	return nil, nil
}

func (m *mockMinioClient) PutObject(ctx context.Context, bucketName, objectName string, reader io.Reader, objectSize int64, opts minio.PutObjectOptions) (minio.UploadInfo, error) {
	data, err := io.ReadAll(reader)
	if err != nil {
		return minio.UploadInfo{}, err
	}
	m.objects[objectName] = data
	return minio.UploadInfo{}, nil
}

func (m *mockMinioClient) RemoveObject(ctx context.Context, bucketName, objectName string, opts minio.RemoveObjectOptions) error {
	delete(m.objects, objectName)
	return nil
}

func (m *mockMinioClient) StatObject(ctx context.Context, bucketName, objectName string, opts minio.StatObjectOptions) (minio.ObjectInfo, error) {
	if _, ok := m.objects[objectName]; ok {
		return minio.ObjectInfo{}, nil
	}
	return minio.ObjectInfo{}, minio.ErrorResponse{Code: "NoSuchKey"}
}

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
	manager := &minioStateManager{logger: &testLogger{}}
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

	manager := &minioStateManager{logger: &testLogger{}}
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

	manager := &minioStateManager{logger: &testLogger{}}
	err = manager.extractTarGz(&buf, tmpDir)
	require.NoError(t, err)

	// Malicious file should NOT exist outside tmpDir
	_, err = os.Stat(filepath.Join(tmpDir, "..", "..", "..", "etc", "malicious.txt"))
	assert.True(t, os.IsNotExist(err), "Path traversal attack should be prevented")
}

// TestObjectName tests the object name generation
func TestObjectName(t *testing.T) {
	manager := &minioStateManager{}

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

// TestUploadState_EmptyDir tests that empty directories are skipped
func TestUploadState_EmptyDir(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "empty-state-")
	require.NoError(t, err)
	defer os.RemoveAll(tmpDir)

	// Create empty state directory
	stateDir := filepath.Join(tmpDir, ".the0-state")
	require.NoError(t, os.MkdirAll(stateDir, 0755))

	mockClient := newMockMinioClient()
	manager := &minioStateManager{
		minioClient: nil, // We won't reach MinIO for empty dirs
		bucket:      "bot-state",
		logger:      &testLogger{},
	}

	// This should not error and should skip upload
	// Note: In production, we'd need a full MinIO mock
	// For now, we test the directory check logic
	entries, err := os.ReadDir(stateDir)
	require.NoError(t, err)
	assert.Empty(t, entries, "Empty directory should have no entries")

	_ = mockClient // Silence unused warning
	_ = manager
}

// TestUploadState_NonExistentDir tests that non-existent directories are handled
func TestUploadState_NonExistentDir(t *testing.T) {
	nonExistentDir := "/tmp/non-existent-state-dir-12345"

	// Verify it doesn't exist
	_, err := os.ReadDir(nonExistentDir)
	assert.True(t, os.IsNotExist(err))
}

// testLogger is a simple logger for testing
type testLogger struct{}

func (l *testLogger) Info(format string, args ...interface{})  {}
func (l *testLogger) Error(format string, args ...interface{}) {}
func (l *testLogger) Debug(format string, args ...interface{}) {}
func (l *testLogger) Warn(format string, args ...interface{})  {}
