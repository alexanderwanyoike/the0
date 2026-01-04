// Package docker provides configuration management for the Docker-based runtime environment.
// This file handles environment-based configuration loading for MinIO, Docker, and resource limits.
package docker

import (
	"fmt"
	"os"
	"strconv"
)

// DockerRunnerConfig holds all configuration needed to run containers
// including MinIO credentials, resource limits, and directory paths.
type DockerRunnerConfig struct {
	MinIOEndpoint         string // MinIO server endpoint (e.g., "localhost:9000")
	MinIOAccessKeyID      string // MinIO access key
	MinIOSecretAccessKey  string // MinIO secret key
	MinIOUseSSL           bool   // Whether to use SSL/TLS for MinIO connections
	MinIOCodeBucket       string // Bucket containing bot code archives
	MinioResultsBucket    string // Bucket for storing backtest results
	MinioLogsBucket       string // Bucket for storing logs
	MinioStateBucket      string // Bucket for storing persistent bot state
	MaxStateSizeBytes     int64  // Maximum total size of bot state folder (default: 8GB)
	MaxStateFileSizeBytes int64  // Maximum size of individual state file (default: 10MB)
	TempDir               string // Temporary directory for extracted bot code
	MemoryLimitMB         int64  // Memory limit in bytes for containers
	CPUShares             int64  // CPU shares allocated to containers
	DevRuntimePath        string // Optional: host path to runtime binary for dev mode
}

// LoadConfigFromEnv loads configuration from environment variables.
// Required env vars: MINIO_ENDPOINT, MINIO_ACCESS_KEY, MINIO_SECRET_KEY
// Optional env vars: MINIO_SSL, MINIO_BACKTESTS_BUCKET, TEMP_DIR, MINIO_CODE_BUCKET,
// MINIO_STATE_BUCKET, MINIO_LOGS_BUCKET, BOT_MEMORY_LIMIT_MB, BOT_CPU_SHARES,
// MAX_STATE_SIZE_MB (default: 8192 = 8GB), MAX_STATE_FILE_SIZE_MB (default: 10),
// DEV_RUNTIME_PATH (optional: host path to runtime binary for dev mode)
func LoadConfigFromEnv() (*DockerRunnerConfig, error) {
	endpoint := os.Getenv("MINIO_ENDPOINT")
	if endpoint == "" {
		return nil, fmt.Errorf("MINIO_ENDPOINT environment variable is required")
	}

	accessKey := os.Getenv("MINIO_ACCESS_KEY")
	if accessKey == "" {
		return nil, fmt.Errorf("MINIO_ACCESS_KEY environment variable is required")
	}

	secretKey := os.Getenv("MINIO_SECRET_KEY")
	if secretKey == "" {
		return nil, fmt.Errorf("MINIO_SECRET_KEY environment variable is required")
	}

	useSSL := os.Getenv("MINIO_SSL") == "true"

	resultsBucket := os.Getenv("MINIO_BACKTESTS_BUCKET")
	if resultsBucket == "" {
		resultsBucket = "backtests"
	}

	logsBucket := os.Getenv("MINIO_LOGS_BUCKET")
	if logsBucket == "" {
		logsBucket = "bot-logs"
	}

	codeBucket := os.Getenv("MINIO_CODE_BUCKET")
	if codeBucket == "" {
		codeBucket = "custom-bots"
	}

	stateBucket := os.Getenv("MINIO_STATE_BUCKET")
	if stateBucket == "" {
		stateBucket = "bot-state"
	}

	tempDir := os.Getenv("TEMP_DIR")
	if tempDir == "" {
		tempDir = "/tmp/runtime"
	}

	// Optional: Dev mode runtime path for mounting binary from host
	devRuntimePath := os.Getenv("DEV_RUNTIME_PATH")

	return &DockerRunnerConfig{
		MinIOEndpoint:         endpoint,
		MinIOAccessKeyID:      accessKey,
		MinIOSecretAccessKey:  secretKey,
		MinIOUseSSL:           useSSL,
		MinIOCodeBucket:       codeBucket,
		MinioResultsBucket:    resultsBucket,
		MinioLogsBucket:       logsBucket,
		MinioStateBucket:      stateBucket,
		MaxStateSizeBytes:     getMaxStateSize(),
		MaxStateFileSizeBytes: getMaxStateFileSize(),
		TempDir:               tempDir,
		MemoryLimitMB:         getMemoryLimit(),
		CPUShares:             getCPUShares(),
		DevRuntimePath:        devRuntimePath,
	}, nil
}

// getCPUShares returns the CPU shares from environment variable BOT_CPU_SHARES.
// Defaults to 512 (half CPU) if not set or invalid.
func getCPUShares() int64 {
	cpuShares := os.Getenv("BOT_CPU_SHARES")
	if cpuShares == "" {
		return 512 // Default half CPU
	}

	shares, err := strconv.Atoi(cpuShares)
	if err != nil {
		return 512 // Default on error
	}

	return int64(shares)
}

// getMemoryLimit returns the memory limit in bytes from environment variable BOT_MEMORY_LIMIT_MB.
// Defaults to 512MB if not set or invalid.
func getMemoryLimit() int64 {
	memoryLimitMB := os.Getenv("BOT_MEMORY_LIMIT_MB")
	if memoryLimitMB == "" {
		return 512 * 1024 * 1024 // Default 512MB
	}

	limitMB, err := strconv.Atoi(memoryLimitMB)
	if err != nil {
		return 512 * 1024 * 1024 // Default on error
	}

	return int64(limitMB) * 1024 * 1024
}

// getMaxStateSize returns the maximum total state folder size in bytes from MAX_STATE_SIZE_MB.
// Defaults to 8GB if not set or invalid.
func getMaxStateSize() int64 {
	maxStateMB := os.Getenv("MAX_STATE_SIZE_MB")
	if maxStateMB == "" {
		return 8 * 1024 * 1024 * 1024 // Default 8GB
	}

	sizeMB, err := strconv.Atoi(maxStateMB)
	if err != nil {
		return 8 * 1024 * 1024 * 1024 // Default on error
	}

	return int64(sizeMB) * 1024 * 1024
}

// getMaxStateFileSize returns the maximum individual state file size in bytes from MAX_STATE_FILE_SIZE_MB.
// Defaults to 10MB if not set or invalid.
func getMaxStateFileSize() int64 {
	maxFileMB := os.Getenv("MAX_STATE_FILE_SIZE_MB")
	if maxFileMB == "" {
		return 10 * 1024 * 1024 // Default 10MB
	}

	sizeMB, err := strconv.Atoi(maxFileMB)
	if err != nil {
		return 10 * 1024 * 1024 // Default on error
	}

	return int64(sizeMB) * 1024 * 1024
}
