// Package execute provides bot execution logic for Docker and K8s containers.
// This package handles code/state download, query result upload, and process execution.
package execute

import (
	"context"
	"fmt"
	"os"

	"runtime/internal/runtime/storage"
	"runtime/internal/util"
)

// StorageOperations provides MinIO storage operations for bot execution.
type StorageOperations struct {
	logger util.Logger
}

// NewStorageOperations creates a new StorageOperations instance.
func NewStorageOperations(logger util.Logger) *StorageOperations {
	if logger == nil {
		logger = &util.DefaultLogger{}
	}
	return &StorageOperations{logger: logger}
}

// DownloadCode downloads and extracts bot code from MinIO.
// If codeFile is empty, skips the download (code may be mounted or pre-deployed).
func (s *StorageOperations) DownloadCode(ctx context.Context, codeFile, destPath string) error {
	if codeFile == "" {
		s.logger.Info("CODE_FILE not set, skipping code download")
		return nil
	}

	s.logger.Info("Downloading code: %s", codeFile)

	storageCfg, err := storage.LoadConfigFromEnv()
	if err != nil {
		return fmt.Errorf("failed to load storage config: %w", err)
	}

	minioClient, err := storage.NewMinioClient(storageCfg)
	if err != nil {
		return fmt.Errorf("failed to create MinIO client: %w", err)
	}

	codeManager := storage.NewCodeManager(minioClient, storageCfg, s.logger)
	if err := codeManager.DownloadAndExtract(ctx, codeFile, destPath); err != nil {
		return fmt.Errorf("failed to download code: %w", err)
	}

	s.logger.Info("Code downloaded to %s", destPath)
	return nil
}

// DownloadState downloads bot state from MinIO.
// Returns nil error if state doesn't exist (first run).
func (s *StorageOperations) DownloadState(ctx context.Context, botID, destPath string) error {
	s.logger.Info("Downloading state for bot: %s", botID)

	storageCfg, err := storage.LoadConfigFromEnv()
	if err != nil {
		return fmt.Errorf("failed to load storage config: %w", err)
	}

	minioClient, err := storage.NewMinioClient(storageCfg)
	if err != nil {
		return fmt.Errorf("failed to create MinIO client: %w", err)
	}

	stateManager := storage.NewStateManager(minioClient, storageCfg, s.logger)
	if err := stateManager.DownloadState(ctx, botID, destPath); err != nil {
		return fmt.Errorf("failed to download state: %w", err)
	}

	s.logger.Info("State downloaded to %s", destPath)
	return nil
}

// UploadQueryResult uploads a query result file to MinIO.
// This is used in K8s mode where ephemeral query pods write results to MinIO.
func (s *StorageOperations) UploadQueryResult(ctx context.Context, resultKey, resultPath string) error {
	data, err := os.ReadFile(resultPath)
	if err != nil {
		return fmt.Errorf("failed to read result file: %w", err)
	}

	storageCfg, err := storage.LoadConfigFromEnv()
	if err != nil {
		return fmt.Errorf("failed to load storage config: %w", err)
	}

	minioClient, err := storage.NewMinioClient(storageCfg)
	if err != nil {
		return fmt.Errorf("failed to create MinIO client: %w", err)
	}

	resultManager := storage.NewQueryResultManager(minioClient, storageCfg, s.logger)
	if err := resultManager.Upload(ctx, resultKey, data); err != nil {
		return fmt.Errorf("failed to upload result: %w", err)
	}

	s.logger.Info("Uploaded query result to MinIO: %s", resultKey)
	return nil
}
