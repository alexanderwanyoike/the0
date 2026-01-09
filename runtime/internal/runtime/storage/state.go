package storage

import (
	"archive/tar"
	"compress/gzip"
	"context"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"

	"github.com/minio/minio-go/v7"
)

// Logger interface for state manager logging.
type Logger interface {
	Info(msg string, args ...interface{})
}

// StateManager handles persisting and restoring bot state to/from MinIO.
type StateManager interface {
	// DownloadState downloads and extracts bot state from MinIO to a local directory.
	// Returns nil if no state exists (first run). Creates destDir if needed.
	DownloadState(ctx context.Context, botID string, destDir string) error

	// UploadState compresses and uploads bot state from a local directory to MinIO.
	// Skips upload if directory is empty or doesn't exist.
	UploadState(ctx context.Context, botID string, srcDir string) error

	// DeleteState removes the state archive from MinIO for a bot.
	DeleteState(ctx context.Context, botID string) error

	// StateExists checks if state exists for a bot in MinIO.
	StateExists(ctx context.Context, botID string) (bool, error)
}

// stateManager implements StateManager using MinIO as the storage backend.
type stateManager struct {
	minioClient      *minio.Client
	bucket           string
	maxStateSize     int64
	maxStateFileSize int64
	logger           Logger
}

// NewStateManager creates a new StateManager that stores state in MinIO.
func NewStateManager(minioClient *minio.Client, cfg *Config, logger Logger) StateManager {
	bucket := cfg.StateBucket
	if bucket == "" {
		bucket = "bot-state"
	}
	maxStateSize := cfg.MaxStateSizeBytes
	if maxStateSize == 0 {
		maxStateSize = 8 * 1024 * 1024 * 1024 // Default 8GB
	}
	maxStateFileSize := cfg.MaxStateFileSizeBytes
	if maxStateFileSize == 0 {
		maxStateFileSize = 10 * 1024 * 1024 // Default 10MB
	}
	return &stateManager{
		minioClient:      minioClient,
		bucket:           bucket,
		maxStateSize:     maxStateSize,
		maxStateFileSize: maxStateFileSize,
		logger:           logger,
	}
}

// ensureBucket creates the bucket if it doesn't exist.
func (m *stateManager) ensureBucket(ctx context.Context) error {
	exists, err := m.minioClient.BucketExists(ctx, m.bucket)
	if err != nil {
		return fmt.Errorf("failed to check bucket existence: %w", err)
	}
	if !exists {
		if err := m.minioClient.MakeBucket(ctx, m.bucket, minio.MakeBucketOptions{}); err != nil {
			return fmt.Errorf("failed to create bucket: %w", err)
		}
		m.logger.Info("Created state bucket", "bucket", m.bucket)
	}
	return nil
}

// objectName returns the MinIO object path for a bot's state.
func (m *stateManager) objectName(botID string) string {
	return fmt.Sprintf("%s/state.tar.gz", botID)
}

// DownloadState downloads and extracts bot state from MinIO.
func (m *stateManager) DownloadState(ctx context.Context, botID string, destDir string) error {
	if err := os.MkdirAll(destDir, 0755); err != nil {
		return fmt.Errorf("failed to create state directory: %w", err)
	}

	object, err := m.minioClient.GetObject(ctx, m.bucket, m.objectName(botID), minio.GetObjectOptions{})
	if err != nil {
		return fmt.Errorf("failed to get state from MinIO: %w", err)
	}
	defer object.Close()

	_, err = object.Stat()
	if err != nil {
		errResponse := minio.ToErrorResponse(err)
		// NoSuchKey = state object doesn't exist, NoSuchBucket = bucket doesn't exist yet
		// Both are valid first-run scenarios
		if errResponse.Code == "NoSuchKey" || errResponse.Code == "NoSuchBucket" {
			m.logger.Info("No existing state (first run)", "bot_id", botID)
			return nil
		}
		return fmt.Errorf("failed to stat state object: %w", err)
	}

	if err := m.extractTarGz(object, destDir); err != nil {
		return fmt.Errorf("failed to extract state: %w", err)
	}

	m.logger.Info("Downloaded state", "bot_id", botID)
	return nil
}

// UploadState compresses and uploads bot state to MinIO.
func (m *stateManager) UploadState(ctx context.Context, botID string, srcDir string) error {
	entries, err := os.ReadDir(srcDir)
	if err != nil {
		if os.IsNotExist(err) {
			m.logger.Info("No state directory, skipping upload", "bot_id", botID)
			return nil
		}
		return fmt.Errorf("failed to read state directory: %w", err)
	}

	if len(entries) == 0 {
		m.logger.Info("Empty state directory, skipping upload", "bot_id", botID)
		return nil
	}

	totalSize, err := m.validateStateSize(srcDir)
	if err != nil {
		return err
	}
	m.logger.Info("State size", "bot_id", botID, "bytes", totalSize)

	if err := m.ensureBucket(ctx); err != nil {
		return err
	}

	pr, pw := io.Pipe()

	errChan := make(chan error, 1)
	go func() {
		defer pw.Close()
		errChan <- m.createTarGz(srcDir, pw)
	}()

	_, err = m.minioClient.PutObject(ctx, m.bucket, m.objectName(botID), pr, -1, minio.PutObjectOptions{
		ContentType: "application/gzip",
	})
	if err != nil {
		return fmt.Errorf("failed to upload state to MinIO: %w", err)
	}

	if compressErr := <-errChan; compressErr != nil {
		return fmt.Errorf("failed to create tar.gz: %w", compressErr)
	}

	m.logger.Info("Uploaded state", "bot_id", botID, "files", len(entries))
	return nil
}

// DeleteState removes the state archive from MinIO.
func (m *stateManager) DeleteState(ctx context.Context, botID string) error {
	err := m.minioClient.RemoveObject(ctx, m.bucket, m.objectName(botID), minio.RemoveObjectOptions{})
	if err != nil {
		errResponse := minio.ToErrorResponse(err)
		if errResponse.Code == "NoSuchKey" {
			return nil
		}
		return fmt.Errorf("failed to delete state: %w", err)
	}
	m.logger.Info("Deleted state", "bot_id", botID)
	return nil
}

// StateExists checks if state exists for a bot.
func (m *stateManager) StateExists(ctx context.Context, botID string) (bool, error) {
	_, err := m.minioClient.StatObject(ctx, m.bucket, m.objectName(botID), minio.StatObjectOptions{})
	if err != nil {
		errResponse := minio.ToErrorResponse(err)
		if errResponse.Code == "NoSuchKey" {
			return false, nil
		}
		return false, fmt.Errorf("failed to check state existence: %w", err)
	}
	return true, nil
}

// createTarGz creates a tar.gz archive of a directory.
func (m *stateManager) createTarGz(srcDir string, w io.Writer) error {
	gzw := gzip.NewWriter(w)
	defer gzw.Close()

	tw := tar.NewWriter(gzw)
	defer tw.Close()

	return filepath.Walk(srcDir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		relPath, err := filepath.Rel(srcDir, path)
		if err != nil {
			return err
		}

		if relPath == "." {
			return nil
		}

		header, err := tar.FileInfoHeader(info, "")
		if err != nil {
			return err
		}
		header.Name = relPath

		if err := tw.WriteHeader(header); err != nil {
			return err
		}

		if !info.IsDir() {
			if err := copyFileToTar(path, tw); err != nil {
				return err
			}
		}

		return nil
	})
}

// copyFileToTar copies a file to a tar writer and closes immediately.
func copyFileToTar(path string, tw *tar.Writer) error {
	file, err := os.Open(path)
	if err != nil {
		return err
	}
	_, err = io.Copy(tw, file)
	file.Close()
	return err
}

// validateStateSize checks that the state directory doesn't exceed size limits.
func (m *stateManager) validateStateSize(srcDir string) (int64, error) {
	var totalSize int64

	err := filepath.Walk(srcDir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		if info.IsDir() {
			return nil
		}

		if info.Size() > m.maxStateFileSize {
			return fmt.Errorf("state file %s exceeds maximum size limit (%d bytes > %d bytes)",
				info.Name(), info.Size(), m.maxStateFileSize)
		}

		totalSize += info.Size()

		if totalSize > m.maxStateSize {
			return fmt.Errorf("total state size exceeds maximum limit (%d bytes > %d bytes)",
				totalSize, m.maxStateSize)
		}

		return nil
	})

	if err != nil {
		return 0, err
	}

	return totalSize, nil
}

// extractTarGz extracts a tar.gz archive to a directory.
func (m *stateManager) extractTarGz(r io.Reader, destDir string) error {
	gzr, err := gzip.NewReader(r)
	if err != nil {
		return err
	}
	defer gzr.Close()

	tr := tar.NewReader(gzr)

	var totalExtracted int64

	for {
		header, err := tr.Next()
		if err == io.EOF {
			break
		}
		if err != nil {
			return err
		}

		target := filepath.Join(destDir, header.Name)

		// Security: prevent directory traversal
		if !strings.HasPrefix(filepath.Clean(target), filepath.Clean(destDir)+string(os.PathSeparator)) {
			m.logger.Info("Skipping path traversal attempt", "file", header.Name)
			continue
		}

		switch header.Typeflag {
		case tar.TypeDir:
			if err := os.MkdirAll(target, os.FileMode(header.Mode)); err != nil {
				return err
			}
		case tar.TypeReg:
			if header.Size > m.maxStateFileSize {
				return fmt.Errorf("file %s exceeds maximum size limit (%d bytes > %d bytes)",
					header.Name, header.Size, m.maxStateFileSize)
			}

			totalExtracted += header.Size
			if totalExtracted > m.maxStateSize {
				return fmt.Errorf("total extracted size exceeds maximum limit (%d bytes > %d bytes)",
					totalExtracted, m.maxStateSize)
			}

			if err := os.MkdirAll(filepath.Dir(target), 0755); err != nil {
				return err
			}

			file, err := os.OpenFile(target, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, os.FileMode(header.Mode))
			if err != nil {
				return err
			}

			limited := io.LimitReader(tr, m.maxStateFileSize+1)
			written, err := io.Copy(file, limited)
			file.Close()

			if err != nil {
				return err
			}

			if written > m.maxStateFileSize {
				os.Remove(target)
				return fmt.Errorf("file %s exceeded maximum size during extraction", header.Name)
			}
		default:
			m.logger.Info("Skipping unsupported tar entry", "type", header.Typeflag, "file", header.Name)
		}
	}

	return nil
}
