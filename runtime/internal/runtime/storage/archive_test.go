package storage

import (
	"archive/zip"
	"bytes"
	"os"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// createTestZip creates a zip archive in memory with the given files.
func createTestZip(t *testing.T, files map[string]string) []byte {
	var buf bytes.Buffer
	w := zip.NewWriter(&buf)

	for name, content := range files {
		f, err := w.Create(name)
		require.NoError(t, err)
		_, err = f.Write([]byte(content))
		require.NoError(t, err)
	}

	require.NoError(t, w.Close())
	return buf.Bytes()
}

func TestExtractZipBytes_BasicFiles(t *testing.T) {
	testFiles := map[string]string{
		"main.py":       "print('hello')",
		"config.json":   `{"key": "value"}`,
		"lib/helper.py": "def helper(): pass",
	}

	zipData := createTestZip(t, testFiles)

	// Extract to temp directory
	tmpDir, err := os.MkdirTemp("", "extract-test-")
	require.NoError(t, err)
	defer os.RemoveAll(tmpDir)

	err = ExtractZipBytes(zipData, tmpDir)
	require.NoError(t, err)

	// Verify files were extracted
	for name, expectedContent := range testFiles {
		content, err := os.ReadFile(filepath.Join(tmpDir, name))
		require.NoError(t, err, "Should extract file: %s", name)
		assert.Equal(t, expectedContent, string(content))
	}
}

func TestExtractZipBytes_PreventDirectoryTraversal(t *testing.T) {
	// Create a zip with a malicious path
	var buf bytes.Buffer
	w := zip.NewWriter(&buf)

	// Try to escape the directory
	f, err := w.Create("../../../etc/malicious.txt")
	require.NoError(t, err)
	_, err = f.Write([]byte("evil"))
	require.NoError(t, err)
	require.NoError(t, w.Close())

	// Extract to temp directory
	tmpDir, err := os.MkdirTemp("", "traversal-test-")
	require.NoError(t, err)
	defer os.RemoveAll(tmpDir)

	err = ExtractZipBytes(buf.Bytes(), tmpDir)
	require.NoError(t, err) // Should not error, just skip the malicious file

	// Malicious file should NOT exist outside tmpDir
	_, err = os.Stat(filepath.Join(tmpDir, "..", "..", "..", "etc", "malicious.txt"))
	assert.True(t, os.IsNotExist(err), "Path traversal attack should be prevented")

	// Also shouldn't exist in tmpDir (it should be skipped entirely)
	_, err = os.Stat(filepath.Join(tmpDir, "malicious.txt"))
	assert.True(t, os.IsNotExist(err), "Malicious file should not be extracted")
}

func TestExtractZipBytes_EmptyZip(t *testing.T) {
	var buf bytes.Buffer
	w := zip.NewWriter(&buf)
	require.NoError(t, w.Close())

	tmpDir, err := os.MkdirTemp("", "empty-zip-")
	require.NoError(t, err)
	defer os.RemoveAll(tmpDir)

	err = ExtractZipBytes(buf.Bytes(), tmpDir)
	require.NoError(t, err)

	// Directory should be empty
	entries, err := os.ReadDir(tmpDir)
	require.NoError(t, err)
	assert.Empty(t, entries)
}

func TestExtractZipBytes_WithDirectories(t *testing.T) {
	var buf bytes.Buffer
	w := zip.NewWriter(&buf)

	// Add a directory entry with proper permissions
	header := &zip.FileHeader{
		Name:   "subdir/",
		Method: zip.Deflate,
	}
	header.SetMode(0755 | os.ModeDir)
	_, err := w.CreateHeader(header)
	require.NoError(t, err)

	// Add a file in the directory
	f, err := w.Create("subdir/file.txt")
	require.NoError(t, err)
	_, err = f.Write([]byte("content"))
	require.NoError(t, err)

	require.NoError(t, w.Close())

	tmpDir, err := os.MkdirTemp("", "dir-test-")
	require.NoError(t, err)
	defer os.RemoveAll(tmpDir)

	err = ExtractZipBytes(buf.Bytes(), tmpDir)
	require.NoError(t, err)

	// Check directory exists
	info, err := os.Stat(filepath.Join(tmpDir, "subdir"))
	require.NoError(t, err)
	assert.True(t, info.IsDir())

	// Check file exists
	content, err := os.ReadFile(filepath.Join(tmpDir, "subdir", "file.txt"))
	require.NoError(t, err)
	assert.Equal(t, "content", string(content))
}

func TestExtractZip_FromFile(t *testing.T) {
	testFiles := map[string]string{
		"app.js": "console.log('hello')",
	}

	zipData := createTestZip(t, testFiles)

	// Write zip to temp file
	tmpZip, err := os.CreateTemp("", "test-*.zip")
	require.NoError(t, err)
	defer os.Remove(tmpZip.Name())

	_, err = tmpZip.Write(zipData)
	require.NoError(t, err)
	require.NoError(t, tmpZip.Close())

	// Extract to temp directory
	tmpDir, err := os.MkdirTemp("", "extract-file-test-")
	require.NoError(t, err)
	defer os.RemoveAll(tmpDir)

	err = ExtractZip(tmpZip.Name(), tmpDir)
	require.NoError(t, err)

	// Verify file was extracted
	content, err := os.ReadFile(filepath.Join(tmpDir, "app.js"))
	require.NoError(t, err)
	assert.Equal(t, "console.log('hello')", string(content))
}

func TestExtractZip_NonExistentFile(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "extract-missing-")
	require.NoError(t, err)
	defer os.RemoveAll(tmpDir)

	err = ExtractZip("/nonexistent/file.zip", tmpDir)
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "failed to read zip file")
}

func TestExtractZipBytes_InvalidZip(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "invalid-zip-")
	require.NoError(t, err)
	defer os.RemoveAll(tmpDir)

	err = ExtractZipBytes([]byte("not a zip file"), tmpDir)
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "failed to open zip")
}
