// Package docker provides the StateManager component.
//
// StateManager handles persisting and restoring bot state to/from MinIO.
// State is stored as a tar.gz archive containing JSON files for each state key.
// This enables bots to maintain persistent data across executions.
package docker

import (
	"archive/tar"
	"compress/gzip"
	"context"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"runtime/internal/util"
	"strings"

	"github.com/minio/minio-go/v7"
)

// StateManager handles persisting and restoring bot state.
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

// minioStateManager implements StateManager using MinIO as the storage backend.
type minioStateManager struct {
	minioClient *minio.Client
	bucket      string
	logger      util.Logger
}

// NewMinioStateManager creates a new StateManager that stores state in MinIO.
func NewMinioStateManager(minioClient *minio.Client, config *DockerRunnerConfig, logger util.Logger) StateManager {
	bucket := config.MinioStateBucket
	if bucket == "" {
		bucket = "bot-state"
	}
	return &minioStateManager{
		minioClient: minioClient,
		bucket:      bucket,
		logger:      logger,
	}
}

// ensureBucket creates the bucket if it doesn't exist.
func (m *minioStateManager) ensureBucket(ctx context.Context) error {
	exists, err := m.minioClient.BucketExists(ctx, m.bucket)
	if err != nil {
		return fmt.Errorf("failed to check bucket existence: %w", err)
	}
	if !exists {
		if err := m.minioClient.MakeBucket(ctx, m.bucket, minio.MakeBucketOptions{}); err != nil {
			return fmt.Errorf("failed to create bucket: %w", err)
		}
		m.logger.Info("Created state bucket: %s", m.bucket)
	}
	return nil
}

// objectName returns the MinIO object path for a bot's state.
func (m *minioStateManager) objectName(botID string) string {
	return fmt.Sprintf("%s/state.tar.gz", botID)
}

// DownloadState downloads and extracts bot state from MinIO.
func (m *minioStateManager) DownloadState(ctx context.Context, botID string, destDir string) error {
	// Ensure destination directory exists
	if err := os.MkdirAll(destDir, 0755); err != nil {
		return fmt.Errorf("failed to create state directory: %w", err)
	}

	// Get object from MinIO
	object, err := m.minioClient.GetObject(ctx, m.bucket, m.objectName(botID), minio.GetObjectOptions{})
	if err != nil {
		return fmt.Errorf("failed to get state from MinIO: %w", err)
	}
	defer object.Close()

	// Check if object exists by trying to stat it
	_, err = object.Stat()
	if err != nil {
		// Object doesn't exist - this is normal for first run
		errResponse := minio.ToErrorResponse(err)
		if errResponse.Code == "NoSuchKey" {
			m.logger.Info("No existing state for bot %s (first run)", botID)
			return nil
		}
		return fmt.Errorf("failed to stat state object: %w", err)
	}

	// Extract tar.gz to destination
	if err := m.extractTarGz(object, destDir); err != nil {
		return fmt.Errorf("failed to extract state: %w", err)
	}

	m.logger.Info("Downloaded state for bot %s", botID)
	return nil
}

// UploadState compresses and uploads bot state to MinIO.
func (m *minioStateManager) UploadState(ctx context.Context, botID string, srcDir string) error {
	// Check if directory exists and has files
	entries, err := os.ReadDir(srcDir)
	if err != nil {
		if os.IsNotExist(err) {
			m.logger.Info("No state directory for bot %s, skipping upload", botID)
			return nil
		}
		return fmt.Errorf("failed to read state directory: %w", err)
	}

	if len(entries) == 0 {
		m.logger.Info("Empty state directory for bot %s, skipping upload", botID)
		return nil
	}

	// Ensure bucket exists
	if err := m.ensureBucket(ctx); err != nil {
		return err
	}

	// Create tar.gz in memory
	pr, pw := io.Pipe()

	// Run compression in goroutine
	errChan := make(chan error, 1)
	go func() {
		defer pw.Close()
		errChan <- m.createTarGz(srcDir, pw)
	}()

	// Upload to MinIO (this reads from the pipe)
	_, err = m.minioClient.PutObject(ctx, m.bucket, m.objectName(botID), pr, -1, minio.PutObjectOptions{
		ContentType: "application/gzip",
	})
	if err != nil {
		return fmt.Errorf("failed to upload state to MinIO: %w", err)
	}

	// Check for compression errors
	if compressErr := <-errChan; compressErr != nil {
		return fmt.Errorf("failed to create tar.gz: %w", compressErr)
	}

	m.logger.Info("Uploaded state for bot %s (%d files)", botID, len(entries))
	return nil
}

// DeleteState removes the state archive from MinIO.
func (m *minioStateManager) DeleteState(ctx context.Context, botID string) error {
	err := m.minioClient.RemoveObject(ctx, m.bucket, m.objectName(botID), minio.RemoveObjectOptions{})
	if err != nil {
		errResponse := minio.ToErrorResponse(err)
		if errResponse.Code == "NoSuchKey" {
			return nil // Already doesn't exist
		}
		return fmt.Errorf("failed to delete state: %w", err)
	}
	m.logger.Info("Deleted state for bot %s", botID)
	return nil
}

// StateExists checks if state exists for a bot.
func (m *minioStateManager) StateExists(ctx context.Context, botID string) (bool, error) {
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
func (m *minioStateManager) createTarGz(srcDir string, w io.Writer) error {
	gzw := gzip.NewWriter(w)
	defer gzw.Close()

	tw := tar.NewWriter(gzw)
	defer tw.Close()

	return filepath.Walk(srcDir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		// Get relative path
		relPath, err := filepath.Rel(srcDir, path)
		if err != nil {
			return err
		}

		// Skip the root directory itself
		if relPath == "." {
			return nil
		}

		// Create tar header
		header, err := tar.FileInfoHeader(info, "")
		if err != nil {
			return err
		}
		header.Name = relPath

		if err := tw.WriteHeader(header); err != nil {
			return err
		}

		// Write file content if it's a regular file
		if !info.IsDir() {
			file, err := os.Open(path)
			if err != nil {
				return err
			}
			defer file.Close()

			if _, err := io.Copy(tw, file); err != nil {
				return err
			}
		}

		return nil
	})
}

// extractTarGz extracts a tar.gz archive to a directory.
func (m *minioStateManager) extractTarGz(r io.Reader, destDir string) error {
	gzr, err := gzip.NewReader(r)
	if err != nil {
		return err
	}
	defer gzr.Close()

	tr := tar.NewReader(gzr)

	for {
		header, err := tr.Next()
		if err == io.EOF {
			break
		}
		if err != nil {
			return err
		}

		target := filepath.Join(destDir, header.Name)

		// Security check: prevent directory traversal
		if !strings.HasPrefix(filepath.Clean(target), filepath.Clean(destDir)+string(os.PathSeparator)) {
			continue
		}

		switch header.Typeflag {
		case tar.TypeDir:
			if err := os.MkdirAll(target, os.FileMode(header.Mode)); err != nil {
				return err
			}
		case tar.TypeReg:
			// Ensure parent directory exists
			if err := os.MkdirAll(filepath.Dir(target), 0755); err != nil {
				return err
			}

			file, err := os.OpenFile(target, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, os.FileMode(header.Mode))
			if err != nil {
				return err
			}

			if _, err := io.Copy(file, tr); err != nil {
				file.Close()
				return err
			}
			file.Close()
		}
	}

	return nil
}
