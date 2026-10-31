// Package storage provides shared MinIO storage operations for both Docker and K8s modes.
// This includes state management (tar.gz upload/download) and code archive extraction.
package storage

import (
	"fmt"
	"os"

	"runtime/internal/launch"
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
		CodeBucket:            launch.DefaultCodeBucket,
		StateBucket:           launch.DefaultStateBucket,
		LogsBucket:            launch.DefaultLogsBucket,
		QueryResultBucket:     launch.DefaultQueryResultsBucket,
		MaxStateSizeBytes:     8 * 1024 * 1024 * 1024, // 8GB
		MaxStateFileSizeBytes: 10 * 1024 * 1024,       // 10MB
	}
}

// LoadConfigFromEnv loads storage configuration from environment variables.
func LoadConfigFromEnv() (*Config, error) {
	cfg := DefaultConfig()
	launched := launch.ReadStorage(os.LookupEnv)

	// Required
	cfg.Endpoint = launched.Endpoint
	if cfg.Endpoint == "" {
		return nil, fmt.Errorf("MINIO_ENDPOINT is required")
	}

	cfg.AccessKey = launched.AccessKey
	if cfg.AccessKey == "" {
		return nil, fmt.Errorf("MINIO_ACCESS_KEY is required")
	}

	cfg.SecretKey = launched.SecretKey
	if cfg.SecretKey == "" {
		return nil, fmt.Errorf("MINIO_SECRET_KEY is required")
	}

	// Optional
	cfg.UseSSL = launched.UseSSL
	cfg.CodeBucket = launched.CodeBucket
	cfg.StateBucket = launched.StateBucket
	cfg.LogsBucket = launched.LogsBucket
	cfg.QueryResultBucket = launched.QueryResultsBucket

	if launched.MaxStateSizeMB != nil {
		cfg.MaxStateSizeBytes = *launched.MaxStateSizeMB * 1024 * 1024
	}
	if launched.MaxStateFileSizeMB != nil {
		cfg.MaxStateFileSizeBytes = *launched.MaxStateFileSizeMB * 1024 * 1024
	}

	return cfg, nil
}
