package docker

import (
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestLoadConfigFromEnv_Success(t *testing.T) {
	// Set required environment variables
	os.Setenv("MINIO_ENDPOINT", "localhost:9000")
	os.Setenv("MINIO_ACCESS_KEY", "test-access-key")
	os.Setenv("MINIO_SECRET_KEY", "test-secret-key")
	defer func() {
		os.Unsetenv("MINIO_ENDPOINT")
		os.Unsetenv("MINIO_ACCESS_KEY")
		os.Unsetenv("MINIO_SECRET_KEY")
	}()

	cfg, err := LoadConfigFromEnv()

	require.NoError(t, err)
	assert.Equal(t, "localhost:9000", cfg.MinIOEndpoint)
	assert.Equal(t, "test-access-key", cfg.MinIOAccessKeyID)
	assert.Equal(t, "test-secret-key", cfg.MinIOSecretAccessKey)
	assert.False(t, cfg.MinIOUseSSL) // Default
}

func TestLoadConfigFromEnv_MissingEndpoint(t *testing.T) {
	os.Unsetenv("MINIO_ENDPOINT")
	os.Setenv("MINIO_ACCESS_KEY", "test-access-key")
	os.Setenv("MINIO_SECRET_KEY", "test-secret-key")
	defer func() {
		os.Unsetenv("MINIO_ACCESS_KEY")
		os.Unsetenv("MINIO_SECRET_KEY")
	}()

	cfg, err := LoadConfigFromEnv()

	assert.Error(t, err)
	assert.Nil(t, cfg)
	assert.Contains(t, err.Error(), "MINIO_ENDPOINT")
}

func TestLoadConfigFromEnv_MissingAccessKey(t *testing.T) {
	os.Setenv("MINIO_ENDPOINT", "localhost:9000")
	os.Unsetenv("MINIO_ACCESS_KEY")
	os.Setenv("MINIO_SECRET_KEY", "test-secret-key")
	defer func() {
		os.Unsetenv("MINIO_ENDPOINT")
		os.Unsetenv("MINIO_SECRET_KEY")
	}()

	cfg, err := LoadConfigFromEnv()

	assert.Error(t, err)
	assert.Nil(t, cfg)
	assert.Contains(t, err.Error(), "MINIO_ACCESS_KEY")
}

func TestLoadConfigFromEnv_MissingSecretKey(t *testing.T) {
	os.Setenv("MINIO_ENDPOINT", "localhost:9000")
	os.Setenv("MINIO_ACCESS_KEY", "test-access-key")
	os.Unsetenv("MINIO_SECRET_KEY")
	defer func() {
		os.Unsetenv("MINIO_ENDPOINT")
		os.Unsetenv("MINIO_ACCESS_KEY")
	}()

	cfg, err := LoadConfigFromEnv()

	assert.Error(t, err)
	assert.Nil(t, cfg)
	assert.Contains(t, err.Error(), "MINIO_SECRET_KEY")
}

func TestLoadConfigFromEnv_WithOptionalFields(t *testing.T) {
	// Set all environment variables including optional ones
	os.Setenv("MINIO_ENDPOINT", "minio.example.com:9000")
	os.Setenv("MINIO_ACCESS_KEY", "access-key")
	os.Setenv("MINIO_SECRET_KEY", "secret-key")
	os.Setenv("MINIO_SSL", "true")
	os.Setenv("MINIO_CODE_BUCKET", "custom-code-bucket")
	os.Setenv("MINIO_STATE_BUCKET", "custom-state-bucket")
	os.Setenv("MINIO_LOGS_BUCKET", "custom-logs-bucket")
	os.Setenv("TEMP_DIR", "/custom/temp")
	os.Setenv("BOT_MEMORY_LIMIT_MB", "2048")
	os.Setenv("BOT_CPU_SHARES", "512")
	os.Setenv("MAX_STATE_SIZE_MB", "4096")
	os.Setenv("MAX_STATE_FILE_SIZE_MB", "20")
	os.Setenv("DEV_RUNTIME_PATH", "/dev/runtime")
	os.Setenv("DOCKER_NETWORK", "custom-network")
	defer func() {
		os.Unsetenv("MINIO_ENDPOINT")
		os.Unsetenv("MINIO_ACCESS_KEY")
		os.Unsetenv("MINIO_SECRET_KEY")
		os.Unsetenv("MINIO_SSL")
		os.Unsetenv("MINIO_CODE_BUCKET")
		os.Unsetenv("MINIO_STATE_BUCKET")
		os.Unsetenv("MINIO_LOGS_BUCKET")
		os.Unsetenv("TEMP_DIR")
		os.Unsetenv("BOT_MEMORY_LIMIT_MB")
		os.Unsetenv("BOT_CPU_SHARES")
		os.Unsetenv("MAX_STATE_SIZE_MB")
		os.Unsetenv("MAX_STATE_FILE_SIZE_MB")
		os.Unsetenv("DEV_RUNTIME_PATH")
		os.Unsetenv("DOCKER_NETWORK")
	}()

	cfg, err := LoadConfigFromEnv()

	require.NoError(t, err)
	assert.Equal(t, "minio.example.com:9000", cfg.MinIOEndpoint)
	assert.Equal(t, "access-key", cfg.MinIOAccessKeyID)
	assert.Equal(t, "secret-key", cfg.MinIOSecretAccessKey)
	assert.True(t, cfg.MinIOUseSSL)
	assert.Equal(t, "custom-code-bucket", cfg.MinIOCodeBucket)
	assert.Equal(t, "custom-state-bucket", cfg.MinioStateBucket)
	assert.Equal(t, "custom-logs-bucket", cfg.MinioLogsBucket)
	assert.Equal(t, "/custom/temp", cfg.TempDir)
	assert.Equal(t, int64(2048*1024*1024), cfg.MemoryLimitMB)
	assert.Equal(t, int64(512), cfg.CPUShares)
	assert.Equal(t, int64(4096*1024*1024), cfg.MaxStateSizeBytes)
	assert.Equal(t, int64(20*1024*1024), cfg.MaxStateFileSizeBytes)
	assert.Equal(t, "/dev/runtime", cfg.DevRuntimePath)
	assert.Equal(t, "custom-network", cfg.DockerNetwork)
}

func TestLoadConfigFromEnv_DefaultValues(t *testing.T) {
	// Only set required fields
	os.Setenv("MINIO_ENDPOINT", "localhost:9000")
	os.Setenv("MINIO_ACCESS_KEY", "test-key")
	os.Setenv("MINIO_SECRET_KEY", "test-secret")
	// Clear optional fields
	os.Unsetenv("MINIO_SSL")
	os.Unsetenv("BOT_MEMORY_LIMIT_MB")
	os.Unsetenv("BOT_CPU_SHARES")
	os.Unsetenv("MAX_STATE_SIZE_MB")
	os.Unsetenv("MAX_STATE_FILE_SIZE_MB")
	defer func() {
		os.Unsetenv("MINIO_ENDPOINT")
		os.Unsetenv("MINIO_ACCESS_KEY")
		os.Unsetenv("MINIO_SECRET_KEY")
	}()

	cfg, err := LoadConfigFromEnv()

	require.NoError(t, err)
	// Check defaults
	assert.False(t, cfg.MinIOUseSSL)
	assert.Equal(t, int64(512*1024*1024), cfg.MemoryLimitMB) // Default 512MB
	assert.Equal(t, int64(512), cfg.CPUShares)                // Default 512
	assert.Equal(t, int64(8192*1024*1024), cfg.MaxStateSizeBytes) // Default 8GB
	assert.Equal(t, int64(10*1024*1024), cfg.MaxStateFileSizeBytes) // Default 10MB
}

func TestLoadConfigFromEnv_InvalidMemoryLimit(t *testing.T) {
	os.Setenv("MINIO_ENDPOINT", "localhost:9000")
	os.Setenv("MINIO_ACCESS_KEY", "test-key")
	os.Setenv("MINIO_SECRET_KEY", "test-secret")
	os.Setenv("BOT_MEMORY_LIMIT_MB", "not-a-number")
	defer func() {
		os.Unsetenv("MINIO_ENDPOINT")
		os.Unsetenv("MINIO_ACCESS_KEY")
		os.Unsetenv("MINIO_SECRET_KEY")
		os.Unsetenv("BOT_MEMORY_LIMIT_MB")
	}()

	// Should still succeed with default value
	cfg, err := LoadConfigFromEnv()

	require.NoError(t, err)
	// Should use default when parsing fails
	assert.Equal(t, int64(512*1024*1024), cfg.MemoryLimitMB)
}

func TestLoadConfigFromEnv_InvalidCPUShares(t *testing.T) {
	os.Setenv("MINIO_ENDPOINT", "localhost:9000")
	os.Setenv("MINIO_ACCESS_KEY", "test-key")
	os.Setenv("MINIO_SECRET_KEY", "test-secret")
	os.Setenv("BOT_CPU_SHARES", "invalid")
	defer func() {
		os.Unsetenv("MINIO_ENDPOINT")
		os.Unsetenv("MINIO_ACCESS_KEY")
		os.Unsetenv("MINIO_SECRET_KEY")
		os.Unsetenv("BOT_CPU_SHARES")
	}()

	cfg, err := LoadConfigFromEnv()

	require.NoError(t, err)
	// Should use default when parsing fails
	assert.Equal(t, int64(512), cfg.CPUShares)
}

func TestLoadConfigFromEnv_InvalidMaxStateSize(t *testing.T) {
	os.Setenv("MINIO_ENDPOINT", "localhost:9000")
	os.Setenv("MINIO_ACCESS_KEY", "test-key")
	os.Setenv("MINIO_SECRET_KEY", "test-secret")
	os.Setenv("MAX_STATE_SIZE_MB", "not-valid")
	defer func() {
		os.Unsetenv("MINIO_ENDPOINT")
		os.Unsetenv("MINIO_ACCESS_KEY")
		os.Unsetenv("MINIO_SECRET_KEY")
		os.Unsetenv("MAX_STATE_SIZE_MB")
	}()

	cfg, err := LoadConfigFromEnv()

	require.NoError(t, err)
	// Should use default when parsing fails (8GB)
	assert.Equal(t, int64(8192*1024*1024), cfg.MaxStateSizeBytes)
}

func TestLoadConfigFromEnv_InvalidMaxStateFileSize(t *testing.T) {
	os.Setenv("MINIO_ENDPOINT", "localhost:9000")
	os.Setenv("MINIO_ACCESS_KEY", "test-key")
	os.Setenv("MINIO_SECRET_KEY", "test-secret")
	os.Setenv("MAX_STATE_FILE_SIZE_MB", "xyz")
	defer func() {
		os.Unsetenv("MINIO_ENDPOINT")
		os.Unsetenv("MINIO_ACCESS_KEY")
		os.Unsetenv("MINIO_SECRET_KEY")
		os.Unsetenv("MAX_STATE_FILE_SIZE_MB")
	}()

	cfg, err := LoadConfigFromEnv()

	require.NoError(t, err)
	// Should use default when parsing fails (10MB)
	assert.Equal(t, int64(10*1024*1024), cfg.MaxStateFileSizeBytes)
}

func TestGetContainerEndpoint_WithContainerEndpoint(t *testing.T) {
	cfg := &DockerRunnerConfig{
		MinIOEndpoint:          "localhost:9000",
		MinIOContainerEndpoint: "host.docker.internal:9000",
	}

	result := cfg.GetContainerEndpoint()

	assert.Equal(t, "host.docker.internal:9000", result)
}

func TestGetContainerEndpoint_FallbackToMinIOEndpoint(t *testing.T) {
	cfg := &DockerRunnerConfig{
		MinIOEndpoint:          "localhost:9000",
		MinIOContainerEndpoint: "", // Empty
	}

	result := cfg.GetContainerEndpoint()

	assert.Equal(t, "localhost:9000", result)
}

func TestLoadConfigFromEnv_ContainerEndpoint(t *testing.T) {
	os.Setenv("MINIO_ENDPOINT", "localhost:9000")
	os.Setenv("MINIO_ACCESS_KEY", "test-key")
	os.Setenv("MINIO_SECRET_KEY", "test-secret")
	os.Setenv("MINIO_CONTAINER_ENDPOINT", "host.docker.internal:9000")
	defer func() {
		os.Unsetenv("MINIO_ENDPOINT")
		os.Unsetenv("MINIO_ACCESS_KEY")
		os.Unsetenv("MINIO_SECRET_KEY")
		os.Unsetenv("MINIO_CONTAINER_ENDPOINT")
	}()

	cfg, err := LoadConfigFromEnv()

	require.NoError(t, err)
	assert.Equal(t, "localhost:9000", cfg.MinIOEndpoint)
	assert.Equal(t, "host.docker.internal:9000", cfg.MinIOContainerEndpoint)
	assert.Equal(t, "host.docker.internal:9000", cfg.GetContainerEndpoint())
}
