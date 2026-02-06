// Package storage provides shared MinIO storage operations for both Docker and K8s modes.
// This includes state management (tar.gz upload/download) and code archive extraction.
package storage

import (
	"fmt"
	"os"
	"strconv"
)

// Config holds MinIO and storage configuration.
type Config struct {
	// MinIO connection
	Endpoint  string
	AccessKey string
	SecretKey string
	UseSSL    bool

	// Buckets
	CodeBucket        string // Bot code archives
	StateBucket       string // Bot persistent state
	LogsBucket        string // Bot logs
	QueryResultBucket string // Ephemeral query results

	// State limits
	MaxStateSizeBytes     int64 // Maximum total state folder size (default: 8GB)
	MaxStateFileSizeBytes int64 // Maximum individual state file size (default: 10MB)
}

// DefaultConfig returns a Config with sensible defaults.
func DefaultConfig() *Config {
	return &Config{
		CodeBucket:            "custom-bots",
		StateBucket:           "bot-state",
		LogsBucket:            "bot-logs",
		QueryResultBucket:     "query-results",
		MaxStateSizeBytes:     8 * 1024 * 1024 * 1024, // 8GB
		MaxStateFileSizeBytes: 10 * 1024 * 1024,       // 10MB
	}
}

// LoadConfigFromEnv loads storage configuration from environment variables.
func LoadConfigFromEnv() (*Config, error) {
	cfg := DefaultConfig()

	// Required
	cfg.Endpoint = os.Getenv("MINIO_ENDPOINT")
	if cfg.Endpoint == "" {
		return nil, fmt.Errorf("MINIO_ENDPOINT is required")
	}

	cfg.AccessKey = os.Getenv("MINIO_ACCESS_KEY")
	if cfg.AccessKey == "" {
		return nil, fmt.Errorf("MINIO_ACCESS_KEY is required")
	}

	cfg.SecretKey = os.Getenv("MINIO_SECRET_KEY")
	if cfg.SecretKey == "" {
		return nil, fmt.Errorf("MINIO_SECRET_KEY is required")
	}

	// Optional
	cfg.UseSSL = os.Getenv("MINIO_USE_SSL") == "true"

	if bucket := os.Getenv("MINIO_CODE_BUCKET"); bucket != "" {
		cfg.CodeBucket = bucket
	}
	if bucket := os.Getenv("MINIO_STATE_BUCKET"); bucket != "" {
		cfg.StateBucket = bucket
	}
	if bucket := os.Getenv("MINIO_LOGS_BUCKET"); bucket != "" {
		cfg.LogsBucket = bucket
	}
	if bucket := os.Getenv("MINIO_QUERY_RESULTS_BUCKET"); bucket != "" {
		cfg.QueryResultBucket = bucket
	}

	if sizeStr := os.Getenv("MAX_STATE_SIZE_MB"); sizeStr != "" {
		if size, err := strconv.ParseInt(sizeStr, 10, 64); err == nil {
			cfg.MaxStateSizeBytes = size * 1024 * 1024
		}
	}
	if sizeStr := os.Getenv("MAX_STATE_FILE_SIZE_MB"); sizeStr != "" {
		if size, err := strconv.ParseInt(sizeStr, 10, 64); err == nil {
			cfg.MaxStateFileSizeBytes = size * 1024 * 1024
		}
	}

	return cfg, nil
}
