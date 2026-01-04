package storage

import (
	"context"
	"fmt"
	"io"
	"os"
	"path/filepath"

	"github.com/minio/minio-go/v7"
)

// CodeManager handles downloading and extracting bot code from MinIO.
type CodeManager interface {
	// DownloadAndExtract downloads code from MinIO and extracts it to destDir.
	DownloadAndExtract(ctx context.Context, objectPath, destDir string) error
}

// codeManager implements CodeManager.
type codeManager struct {
	minioClient *minio.Client
	bucket      string
	logger      Logger
}

// NewCodeManager creates a new CodeManager.
func NewCodeManager(minioClient *minio.Client, cfg *Config, logger Logger) CodeManager {
	bucket := cfg.CodeBucket
	if bucket == "" {
		bucket = "custom-bots"
	}
	return &codeManager{
		minioClient: minioClient,
		bucket:      bucket,
		logger:      logger,
	}
}

// DownloadAndExtract downloads code from MinIO and extracts it.
func (m *codeManager) DownloadAndExtract(ctx context.Context, objectPath, destDir string) error {
	if err := os.MkdirAll(destDir, 0755); err != nil {
		return fmt.Errorf("failed to create destination directory: %w", err)
	}

	object, err := m.minioClient.GetObject(ctx, m.bucket, objectPath, minio.GetObjectOptions{})
	if err != nil {
		return fmt.Errorf("failed to get object from MinIO: %w", err)
	}
	defer object.Close()

	data, err := io.ReadAll(object)
	if err != nil {
		return fmt.Errorf("failed to read from MinIO: %w", err)
	}

	// Save to temp file then extract
	zipPath := filepath.Join(destDir, "code.zip")
	if err := os.WriteFile(zipPath, data, 0644); err != nil {
		return fmt.Errorf("failed to write zip file: %w", err)
	}
	defer os.Remove(zipPath)

	if err := ExtractZip(zipPath, destDir); err != nil {
		return fmt.Errorf("failed to extract zip: %w", err)
	}

	m.logger.Info("Downloaded and extracted code", "bucket", m.bucket, "object", objectPath)
	return nil
}
