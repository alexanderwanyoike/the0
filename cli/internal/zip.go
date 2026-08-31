package internal

import (
	"archive/zip"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"time"
)

// CreateBotZip creates a ZIP file containing all bot files from current directory
func CreateBotZip() (string, error) {
	return CreateBotZipFromDir(".")
}

// CreateBotZip creates a ZIP file containing all bot files
func CreateBotZipFromDir(sourceDir string) (string, error) {
	zipPath := fmt.Sprintf("bot_%d.zip", time.Now().Unix())
	zipFile, err := os.Create(zipPath)
	if err != nil {
		return "", err
	}
	defer zipFile.Close()

	zipWriter := zip.NewWriter(zipFile)
	defer zipWriter.Close()

	// Create ignore parser for the source directory
	ignoreParser, err := CreateIgnoreParserForDir(sourceDir)
	if err != nil {
		return "", fmt.Errorf("failed to create ignore parser: %v", err)
	}

	err = filepath.Walk(sourceDir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		// Skip the zip file itself
		if filepath.Base(path) == zipPath {
			return nil
		}

		// Get relative path from source directory
		relPath, err := filepath.Rel(sourceDir, path)
		if err != nil {
			return err
		}

		// Skip if it's the source directory itself
		if relPath == "." {
			return nil
		}

		// Skip hidden files and directories
		if strings.HasPrefix(filepath.Base(path), ".") {
			if info.IsDir() {
				return filepath.SkipDir
			}
			return nil
		}

		// Check ignore patterns
		if ignoreParser.IsIgnored(relPath, info.IsDir()) {
			if info.IsDir() {
				return filepath.SkipDir
			}
			return nil
		}

		if info.IsDir() {
			return nil
		}

		file, err := os.Open(path)
		if err != nil {
			return err
		}
		defer file.Close()

		// Create header with file info to preserve permissions
		header, err := zip.FileInfoHeader(info)
		if err != nil {
			return err
		}
		// The ZIP spec (APPNOTE 4.4.17.1) requires forward slashes as the path
		// separator. filepath.Rel returns OS-native separators, so on Windows
		// this produced entries like `vendor\the0\__init__.py`, which Linux
		// extractors treat as one flat filename -- the bot's vendor/ directory
		// never materialises and every dependency import fails at runtime.
		header.Name = filepath.ToSlash(relPath)
		header.Method = zip.Deflate

		writer, err := zipWriter.CreateHeader(header)
		if err != nil {
			return err
		}

		_, err = io.Copy(writer, file)
		return err
	})

	return zipPath, err
}
