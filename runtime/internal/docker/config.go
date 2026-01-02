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
	MinIOEndpoint        string // MinIO server endpoint (e.g., "localhost:9000")
	MinIOAccessKeyID     string // MinIO access key
	MinIOSecretAccessKey string // MinIO secret key
	MinIOUseSSL          bool   // Whether to use SSL/TLS for MinIO connections
	MinIOCodeBucket      string // Bucket containing bot code archives
	MinioResultsBucket   string // Bucket for storing backtest results
	MinioLogsBucket      string // Bucket for storing logs
	TempDir              string // Temporary directory for extracted bot code
	MemoryLimitMB        int64  // Memory limit in bytes for containers
	CPUShares            int64  // CPU shares allocated to containers
}

// LoadConfigFromEnv loads configuration from environment variables.
// Required env vars: MINIO_ENDPOINT, MINIO_ACCESS_KEY, MINIO_SECRET_KEY
// Optional env vars: MINIO_SSL, MINIO_BACKTESTS_BUCKET, TEMP_DIR, MINIO_CODE_BUCKET,
// BOT_MEMORY_LIMIT_MB, BOT_CPU_SHARES
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

	tempDir := os.Getenv("TEMP_DIR")
	if tempDir == "" {
		tempDir = "/tmp/runtime"
	}

	return &DockerRunnerConfig{
		MinIOEndpoint:        endpoint,
		MinIOAccessKeyID:     accessKey,
		MinIOSecretAccessKey: secretKey,
		MinIOUseSSL:          useSSL,
		MinIOCodeBucket:      codeBucket,
		MinioResultsBucket:   resultsBucket,
		TempDir:              tempDir,
		MemoryLimitMB:        getMemoryLimit(),
		CPUShares:            getCPUShares(),
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
