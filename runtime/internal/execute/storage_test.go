package execute

import (
	"context"
	"os"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"runtime/internal/util"
)

func TestNewStorageOperations(t *testing.T) {
	t.Run("with logger", func(t *testing.T) {
		logger := &util.DefaultLogger{}
		ops := NewStorageOperations(logger)
		assert.NotNil(t, ops)
		assert.Equal(t, logger, ops.logger)
	})

	t.Run("nil logger uses default", func(t *testing.T) {
		ops := NewStorageOperations(nil)
		assert.NotNil(t, ops)
		assert.NotNil(t, ops.logger)
	})
}

func TestStorageOperations_DownloadCode_EmptyCodeFile(t *testing.T) {
	ops := NewStorageOperations(&util.DefaultLogger{})

	err := ops.DownloadCode(context.Background(), "", "/tmp/dest")
	assert.NoError(t, err, "should skip download when codeFile is empty")
}

func TestStorageOperations_UploadQueryResult_FileNotFound(t *testing.T) {
	ops := NewStorageOperations(&util.DefaultLogger{})

	err := ops.UploadQueryResult(context.Background(), "result-key", "/nonexistent/result.json")
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "failed to read result file")
}

func TestStorageOperations_UploadQueryResult_ReadFile(t *testing.T) {
	// Create a temporary result file
	tmpDir := t.TempDir()
	resultPath := filepath.Join(tmpDir, "result.json")
	require.NoError(t, os.WriteFile(resultPath, []byte(`{"status": "ok"}`), 0644))

	ops := NewStorageOperations(&util.DefaultLogger{})

	// This will fail because MinIO config is not set, but we're testing the file read path
	err := ops.UploadQueryResult(context.Background(), "result-key", resultPath)
	assert.Error(t, err)
	// Should fail at storage config, not file reading
	assert.Contains(t, err.Error(), "failed to load storage config")
}
