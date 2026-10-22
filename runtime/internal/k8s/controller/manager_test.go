package controller

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"runtime/internal/k8s/podgen"
)

func TestManagerConfig_EveryPodGeneratorReadsStorageCredentialsFromTheSecret(t *testing.T) {
	config := ManagerConfig{
		Namespace:          "the0",
		MinIOEndpoint:      "minio:9000",
		MinIOAccessKey:     "access-key",
		MinIOSecretKey:     "secret-key",
		StorageCredentials: testStorageCredentialsSecret("the0-the0-secrets"),
		RuntimeImage:       "the0/runtime:latest",
	}

	assert.Equal(t, config.StorageCredentials, config.botControllerConfig("bots").StorageCredentials)
	assert.Equal(t, config.StorageCredentials, config.scheduleControllerConfig("schedules").StorageCredentials)

	bot := createTestBot("bot-1", "price-alerts", "1.0.0", "python3.11")
	bot.CustomBotVersion.Config.Entrypoints["query"] = "query.py"
	queryPod, err := podgen.NewPodGenerator(config.queryPodGeneratorConfig()).
		GenerateQueryPod(bot, "/status", nil, "bot-1/1/result.json")
	require.NoError(t, err)
	env := queryPod.Spec.Containers[0].Env
	assert.Equal(t, "the0-the0-secrets", storageSecretName(t, env, "MINIO_ACCESS_KEY"))
	assert.Equal(t, "the0-the0-secrets", storageSecretName(t, env, "MINIO_SECRET_KEY"))
}
