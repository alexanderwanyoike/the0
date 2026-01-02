// Package docker provides the CodeManager component.
//
// CodeManager handles downloading bot code archives from MinIO and extracting them
// to temporary directories. It implements security checks like directory traversal
// prevention and supports ZIP archive format with automatic format detection.
package docker

import (
	"archive/zip"
	"bytes"
	"context"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"runtime/internal/model"
	"runtime/internal/util"
	"strings"

	"github.com/minio/minio-go/v7"
)

// CodeManager handles fetching and extracting bot code from storage.
type CodeManager interface {
	// FetchAndExtract downloads code from MinIO and extracts it to a temporary directory.
	// Returns the path to the extracted code directory.
	FetchAndExtract(ctx context.Context, executable model.Executable) (string, error)
}

// minioCodeManager implements CodeManager using MinIO as the storage backend.
type minioCodeManager struct {
	minioClient *minio.Client
	config      *DockerRunnerConfig
	logger      util.Logger
}

// NewMinioCodeManager creates a new CodeManager that fetches code from MinIO.
func NewMinioCodeManager(minioClient *minio.Client, config *DockerRunnerConfig, logger util.Logger) CodeManager {
	return &minioCodeManager{
		minioClient: minioClient,
		config:      config,
		logger:      logger,
	}
}

// FetchAndExtract downloads the bot code archive from MinIO and extracts it.
// It creates a bot-specific temporary directory and performs security checks
// to prevent directory traversal attacks.
func (r *minioCodeManager) FetchAndExtract(
	ctx context.Context,
	executable model.Executable,
) (string, error) {
	// Use configured bucket for bot code instead of parsing from path
	bucketName := os.Getenv("MINIO_CODE_BUCKET")
	if bucketName == "" {
		bucketName = "custom-bots" // Default bucket for bot code
	}

	// The FilePath is the object path within the bucket
	objectName := executable.FilePath

	// Create bot-specific temp directory
	botDir := filepath.Join(r.config.TempDir, executable.ID)
	if err := os.MkdirAll(botDir, 0755); err != nil {
		return "", fmt.Errorf("failed to create bot directory: %v", err)
	}

	// Download file from MinIO
	object, err := r.minioClient.GetObject(ctx, bucketName, objectName, minio.GetObjectOptions{})
	if err != nil {
		return "", fmt.Errorf("failed to get object from MinIO: %v", err)
	}
	defer object.Close()

	// Read all data
	data, err := io.ReadAll(object)
	if err != nil {
		return "", fmt.Errorf("failed to read from MinIO: %v", err)
	}

	// Determine file type and extract
	// Try ZIP format first (most common), then fall back to file extension detection
	if strings.HasSuffix(objectName, ".zip") || len(data) >= 4 && string(data[0:4]) == "PK\x03\x04" {
		if err := r.extractZip(data, botDir); err != nil {
			return "", fmt.Errorf("failed to extract zip: %v", err)
		}
	} else {
		// Default to ZIP format for files without extension (like our MinIO files)
		if err := r.extractZip(data, botDir); err != nil {
			return "", fmt.Errorf("failed to extract as zip (unsupported archive format): %s, error: %v", objectName, err)
		}
	}

	r.logger.Info("Downloaded and extracted bot code - bot_id: %s, bucket: %s, object: %s", executable.ID, bucketName, objectName)
	return botDir, nil
}

// extractZip extracts a ZIP archive to the destination directory with security checks.
func (r *minioCodeManager) extractZip(data []byte, destDir string) error {
	reader := bytes.NewReader(data)
	zipReader, err := zip.NewReader(reader, int64(len(data)))
	if err != nil {
		return err
	}

	for _, file := range zipReader.File {
		if err := extractZipFile(file, destDir); err != nil {
			return err
		}
	}

	return nil
}

// extractZipFile extracts a single file from a zip archive.
// Using a separate function ensures defers execute after each file,
// preventing file descriptor exhaustion with large archives.
func extractZipFile(file *zip.File, destDir string) error {
	path := filepath.Join(destDir, file.Name)

	// Security check: prevent directory traversal
	if !strings.HasPrefix(path, filepath.Clean(destDir)+string(os.PathSeparator)) {
		return nil
	}

	if file.FileInfo().IsDir() {
		return os.MkdirAll(path, file.FileInfo().Mode())
	}

	// Create directory if needed
	if err := os.MkdirAll(filepath.Dir(path), 0755); err != nil {
		return err
	}

	fileReader, err := file.Open()
	if err != nil {
		return err
	}
	defer fileReader.Close()

	targetFile, err := os.OpenFile(path, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, file.FileInfo().Mode())
	if err != nil {
		return err
	}
	defer targetFile.Close()

	_, err = io.Copy(targetFile, fileReader)
	return err
}
