/*
Handles artifact management for Docker-based runtime environments.
Key responsibilities include:
- Downloading bot code archive
- Extraction of the archive
- Setting up the execution environment
- Cleanup of temporary files and directories
- Code structure validation
*/
package dockerrunner

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

type CodeManager interface {
	FetchAndExtract(ctx context.Context, executable model.Executable) (string, error)
}

type minioCodeManager struct {
	minioClient *minio.Client
	config      *DockerRunnerConfig
	logger      util.Logger
}

func NewMinioCodeManager(minioClient *minio.Client, config *DockerRunnerConfig, logger util.Logger) CodeManager {
	return &minioCodeManager{
		minioClient: minioClient,
		config:      config,
		logger:      logger,
	}
}

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

func (r *minioCodeManager) extractZip(data []byte, destDir string) error {
	reader := bytes.NewReader(data)
	zipReader, err := zip.NewReader(reader, int64(len(data)))
	if err != nil {
		return err
	}

	for _, file := range zipReader.File {
		path := filepath.Join(destDir, file.Name)

		// Security check: prevent directory traversal
		if !strings.HasPrefix(path, filepath.Clean(destDir)+string(os.PathSeparator)) {
			continue
		}

		if file.FileInfo().IsDir() {
			os.MkdirAll(path, file.FileInfo().Mode())
			continue
		}

		// Create directory if needed
		if err := os.MkdirAll(filepath.Dir(path), 0755); err != nil {
			return err
		}

		fileReader, err := file.Open()
		if err != nil {
			return err
		}

		targetFile, err := os.OpenFile(path, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, file.FileInfo().Mode())
		if err != nil {
			fileReader.Close()
			return err
		}

		_, err = io.Copy(targetFile, fileReader)
		fileReader.Close()
		targetFile.Close()
		if err != nil {
			return err
		}
	}

	return nil
}
