package storage

import (
	"bytes"
	"context"
	"fmt"
	"io"

	"github.com/minio/minio-go/v7"
)

// QueryResultManager handles ephemeral query result storage in MinIO.
// Results are stored temporarily and deleted after retrieval.
type QueryResultManager interface {
	// Upload stores a query result in MinIO.
	Upload(ctx context.Context, key string, data []byte) error

	// Download retrieves a query result from MinIO.
	Download(ctx context.Context, key string) ([]byte, error)

	// Delete removes a query result from MinIO.
	Delete(ctx context.Context, key string) error
}

type queryResultManager struct {
	minioClient *minio.Client
	bucket      string
	logger      Logger
}

// NewQueryResultManager creates a new QueryResultManager.
func NewQueryResultManager(minioClient *minio.Client, cfg *Config, logger Logger) QueryResultManager {
	bucket := cfg.QueryResultBucket
	if bucket == "" {
		bucket = "query-results"
	}
	return &queryResultManager{
		minioClient: minioClient,
		bucket:      bucket,
		logger:      logger,
	}
}

func (m *queryResultManager) ensureBucket(ctx context.Context) error {
	exists, err := m.minioClient.BucketExists(ctx, m.bucket)
	if err != nil {
		return fmt.Errorf("failed to check bucket existence: %w", err)
	}
	if !exists {
		if err := m.minioClient.MakeBucket(ctx, m.bucket, minio.MakeBucketOptions{}); err != nil {
			return fmt.Errorf("failed to create bucket: %w", err)
		}
		m.logger.Info("Created query results bucket", "bucket", m.bucket)
	}
	return nil
}

func (m *queryResultManager) Upload(ctx context.Context, key string, data []byte) error {
	if err := m.ensureBucket(ctx); err != nil {
		return err
	}

	_, err := m.minioClient.PutObject(ctx, m.bucket, key, bytes.NewReader(data), int64(len(data)), minio.PutObjectOptions{
		ContentType: "application/json",
	})
	if err != nil {
		return fmt.Errorf("failed to upload query result: %w", err)
	}

	m.logger.Info("Uploaded query result", "key", key)
	return nil
}

func (m *queryResultManager) Download(ctx context.Context, key string) ([]byte, error) {
	object, err := m.minioClient.GetObject(ctx, m.bucket, key, minio.GetObjectOptions{})
	if err != nil {
		return nil, fmt.Errorf("failed to get query result: %w", err)
	}
	defer object.Close()

	data, err := io.ReadAll(object)
	if err != nil {
		return nil, fmt.Errorf("failed to read query result: %w", err)
	}

	m.logger.Info("Downloaded query result", "key", key)
	return data, nil
}

func (m *queryResultManager) Delete(ctx context.Context, key string) error {
	err := m.minioClient.RemoveObject(ctx, m.bucket, key, minio.RemoveObjectOptions{})
	if err != nil {
		errResponse := minio.ToErrorResponse(err)
		if errResponse.Code == "NoSuchKey" {
			return nil
		}
		return fmt.Errorf("failed to delete query result: %w", err)
	}
	m.logger.Info("Deleted query result", "key", key)
	return nil
}
