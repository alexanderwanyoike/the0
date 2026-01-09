package storage

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestQueryResultManager_Integration(t *testing.T) {
	if testing.Short() {
		t.Skip("Skipping integration test")
	}

	server := setupMinIOTestContainer(t)
	defer server.cleanup(t)

	ctx := context.Background()
	logger := &integrationTestLogger{t: t}

	// Create QueryResultManager
	cfg := &Config{
		QueryResultBucket: "test-query-results",
	}
	manager := NewQueryResultManager(server.client, cfg, logger)

	t.Run("upload and download", func(t *testing.T) {
		key := "test-bot/query1.json"
		data := []byte(`{"result": "success", "value": 42}`)

		// Upload
		err := manager.Upload(ctx, key, data)
		require.NoError(t, err)

		// Download
		downloaded, err := manager.Download(ctx, key)
		require.NoError(t, err)
		assert.Equal(t, data, downloaded)

		// Cleanup
		err = manager.Delete(ctx, key)
		require.NoError(t, err)
	})

	t.Run("upload creates bucket if not exists", func(t *testing.T) {
		// Use a new bucket name that doesn't exist
		cfg := &Config{
			QueryResultBucket: "auto-created-bucket",
		}
		manager := NewQueryResultManager(server.client, cfg, logger)

		key := "test/result.json"
		data := []byte(`{"status": "ok"}`)

		// Should create bucket automatically
		err := manager.Upload(ctx, key, data)
		require.NoError(t, err)

		// Verify bucket exists
		exists, err := server.client.BucketExists(ctx, "auto-created-bucket")
		require.NoError(t, err)
		assert.True(t, exists)

		// Cleanup
		err = manager.Delete(ctx, key)
		require.NoError(t, err)
	})

	t.Run("download non-existent key", func(t *testing.T) {
		key := "non-existent/result.json"

		_, err := manager.Download(ctx, key)
		assert.Error(t, err)
		assert.Contains(t, err.Error(), "failed to")
	})

	t.Run("delete non-existent key", func(t *testing.T) {
		key := "non-existent/result.json"

		// Should not error when deleting non-existent key
		err := manager.Delete(ctx, key)
		assert.NoError(t, err)
	})

	t.Run("upload large result", func(t *testing.T) {
		key := "test-bot/large-result.json"

		// Create a large JSON result (1MB)
		largeData := make([]byte, 1024*1024)
		for i := range largeData {
			largeData[i] = byte('a' + (i % 26))
		}

		err := manager.Upload(ctx, key, largeData)
		require.NoError(t, err)

		downloaded, err := manager.Download(ctx, key)
		require.NoError(t, err)
		assert.Equal(t, len(largeData), len(downloaded))
		assert.Equal(t, largeData, downloaded)

		// Cleanup
		err = manager.Delete(ctx, key)
		require.NoError(t, err)
	})

	t.Run("upload empty result", func(t *testing.T) {
		key := "test-bot/empty-result.json"
		data := []byte(`{}`)

		err := manager.Upload(ctx, key, data)
		require.NoError(t, err)

		downloaded, err := manager.Download(ctx, key)
		require.NoError(t, err)
		assert.Equal(t, data, downloaded)

		// Cleanup
		err = manager.Delete(ctx, key)
		require.NoError(t, err)
	})

	t.Run("overwrite existing result", func(t *testing.T) {
		key := "test-bot/overwrite.json"

		// Upload first version
		data1 := []byte(`{"version": 1}`)
		err := manager.Upload(ctx, key, data1)
		require.NoError(t, err)

		// Upload second version (overwrite)
		data2 := []byte(`{"version": 2}`)
		err = manager.Upload(ctx, key, data2)
		require.NoError(t, err)

		// Download should get second version
		downloaded, err := manager.Download(ctx, key)
		require.NoError(t, err)
		assert.Equal(t, data2, downloaded)

		// Cleanup
		err = manager.Delete(ctx, key)
		require.NoError(t, err)
	})

	t.Run("upload with nested path", func(t *testing.T) {
		key := "bot-123/2024/01/07/query-result.json"
		data := []byte(`{"nested": true}`)

		err := manager.Upload(ctx, key, data)
		require.NoError(t, err)

		downloaded, err := manager.Download(ctx, key)
		require.NoError(t, err)
		assert.Equal(t, data, downloaded)

		// Cleanup
		err = manager.Delete(ctx, key)
		require.NoError(t, err)
	})

	t.Run("multiple concurrent operations", func(t *testing.T) {
		keys := []string{
			"concurrent/result1.json",
			"concurrent/result2.json",
			"concurrent/result3.json",
		}

		// Upload concurrently
		done := make(chan error, len(keys))
		for i, key := range keys {
			go func(k string, idx int) {
				data := []byte(`{"index": ` + string(rune('0'+idx)) + `}`)
				done <- manager.Upload(ctx, k, data)
			}(key, i)
		}

		// Wait for all uploads
		for range keys {
			err := <-done
			require.NoError(t, err)
		}

		// Verify all uploaded
		for _, key := range keys {
			_, err := manager.Download(ctx, key)
			require.NoError(t, err)
		}

		// Cleanup
		for _, key := range keys {
			err := manager.Delete(ctx, key)
			require.NoError(t, err)
		}
	})
}

func TestNewQueryResultManager_DefaultBucket(t *testing.T) {
	logger := &testLogger{}

	// With nil config
	manager := NewQueryResultManager(nil, nil, logger)
	qrm := manager.(*queryResultManager)
	assert.Equal(t, "query-results", qrm.bucket)

	// With config but empty bucket
	cfg := &Config{
		QueryResultBucket: "",
	}
	manager = NewQueryResultManager(nil, cfg, logger)
	qrm = manager.(*queryResultManager)
	assert.Equal(t, "query-results", qrm.bucket)

	// With custom bucket
	cfg = &Config{
		QueryResultBucket: "custom-bucket",
	}
	manager = NewQueryResultManager(nil, cfg, logger)
	qrm = manager.(*queryResultManager)
	assert.Equal(t, "custom-bucket", qrm.bucket)
}

func TestQueryResultManager_KeyFormats(t *testing.T) {
	if testing.Short() {
		t.Skip("Skipping integration test")
	}

	server := setupMinIOTestContainer(t)
	defer server.cleanup(t)

	ctx := context.Background()
	logger := &integrationTestLogger{t: t}
	manager := NewQueryResultManager(server.client, nil, logger)

	tests := []struct {
		name string
		key  string
	}{
		{"simple", "result.json"},
		{"with bot id", "bot-123/result.json"},
		{"with timestamp", "bot-123/1704672000/result.json"},
		{"with guid", "bot-123/550e8400-e29b-41d4-a716-446655440000.json"},
		{"deep nesting", "a/b/c/d/e/f/result.json"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			data := []byte(`{"key": "` + tt.key + `"}`)

			err := manager.Upload(ctx, tt.key, data)
			require.NoError(t, err)

			downloaded, err := manager.Download(ctx, tt.key)
			require.NoError(t, err)
			assert.Equal(t, data, downloaded)

			err = manager.Delete(ctx, tt.key)
			require.NoError(t, err)
		})
	}
}

// Benchmark tests
func BenchmarkQueryResultManager_Upload(b *testing.B) {
	if testing.Short() {
		b.Skip("Skipping integration benchmark")
	}

	server := setupMinIOTestContainer(&testing.T{})
	defer server.cleanup(&testing.T{})

	ctx := context.Background()
	logger := &testLogger{}
	manager := NewQueryResultManager(server.client, nil, logger)

	data := []byte(`{"benchmark": true, "size": 1024}`)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		key := "benchmark/result-" + string(rune(i)) + ".json"
		_ = manager.Upload(ctx, key, data)
	}
}

func BenchmarkQueryResultManager_Download(b *testing.B) {
	if testing.Short() {
		b.Skip("Skipping integration benchmark")
	}

	server := setupMinIOTestContainer(&testing.T{})
	defer server.cleanup(&testing.T{})

	ctx := context.Background()
	logger := &testLogger{}
	manager := NewQueryResultManager(server.client, nil, logger)

	// Upload once
	key := "benchmark/result.json"
	data := []byte(`{"benchmark": true, "size": 1024}`)
	_ = manager.Upload(ctx, key, data)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, _ = manager.Download(ctx, key)
	}
}
