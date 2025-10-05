package dockerrunner

import (
	"fmt"
	"os"
	"strconv"
)

type DockerRunnerConfig struct {
	MinIOEndpoint        string
	MinIOAccessKeyID     string
	MinIOSecretAccessKey string
	MinIOUseSSL          bool
	MinIOCodeBucket      string
	MinioResultsBucket   string
	TempDir              string
	MemoryLimitMB        int64
	CPUShares            int64
}

func LoaConfigFromEnv() (*DockerRunnerConfig, error) {
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
		resultsBucket = "test-results"
	}

	tempDir := os.Getenv("TEMP_DIR")
	if tempDir == "" {
		tempDir = "/tmp/runtime"
	}

	codeBucket := os.Getenv("MINIO_CODE_BUCKET")
	if codeBucket == "" {
		codeBucket = "custom-bots"
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
