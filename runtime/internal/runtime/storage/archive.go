package storage

import (
	"archive/zip"
	"bytes"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
)

// ExtractZip extracts a ZIP file from disk to the destination directory.
func ExtractZip(zipPath, destDir string) error {
	data, err := os.ReadFile(zipPath)
	if err != nil {
		return fmt.Errorf("failed to read zip file: %w", err)
	}
	return ExtractZipBytes(data, destDir)
}

// ExtractZipBytes extracts ZIP data to the destination directory.
func ExtractZipBytes(data []byte, destDir string) error {
	reader, err := zip.NewReader(bytes.NewReader(data), int64(len(data)))
	if err != nil {
		return fmt.Errorf("failed to open zip: %w", err)
	}

	for _, file := range reader.File {
		if err := extractZipFile(file, destDir); err != nil {
			return err
		}
	}

	return nil
}

// extractZipFile extracts a single file from a zip archive.
func extractZipFile(file *zip.File, destDir string) error {
	path := filepath.Join(destDir, file.Name)

	// Security: prevent directory traversal
	if !strings.HasPrefix(filepath.Clean(path), filepath.Clean(destDir)+string(os.PathSeparator)) {
		return nil // Skip files with path traversal
	}

	if file.FileInfo().IsDir() {
		return os.MkdirAll(path, file.FileInfo().Mode())
	}

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
