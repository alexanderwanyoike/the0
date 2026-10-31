package controller

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	corev1 "k8s.io/api/core/v1"

	"runtime/internal/k8s/podgen"
	"runtime/internal/launch"
	"runtime/internal/model"
)

func TestManagerConfig_EveryPodGeneratorReadsStorageCredentialsFromTheSecret(t *testing.T) {
	config := ManagerConfig{
		Namespace: "the0",
		Storage: launch.Storage{
			Endpoint:  "minio:9000",
			AccessKey: "access-key",
			SecretKey: "secret-key",
		},
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

func TestManagerConfig_EveryPodAndTheQueryHandlerUseTheControllersBuckets(t *testing.T) {
	config := ManagerConfig{
		Namespace: "the0",
		Storage: launch.Storage{
			Endpoint:           "minio:9000",
			AccessKey:          "access-key",
			SecretKey:          "secret-key",
			CodeBucket:         "team-code",
			LogsBucket:         "team-logs",
			QueryResultsBucket: "team-results",
		},
		RuntimeImage: "the0/runtime:latest",
	}
	ctx := context.Background()

	pods := &MockK8sClient{}
	bot := createTestBot("bot-1", "price-alerts", "1.0.0", "python3.11")
	require.NoError(t, NewBotController(config.botControllerConfig("bots"), &MockDesiredBots{Running: []model.Bot{bot}}, pods).Reconcile(ctx))
	require.Len(t, pods.CreatedPods, 1)
	realtime := pods.CreatedPods[0].Spec

	cronJobs := NewMockK8sCronJobClient()
	schedule := createTestSchedule("schedule-1", "daily-report", "1.0.0", "python3.11", "0 9 * * *")
	require.NoError(t, NewBotScheduleController(config.scheduleControllerConfig("schedules"), &MockDesiredSchedules{Running: []model.BotSchedule{schedule}}, cronJobs).Reconcile(ctx))
	cronJob := cronJobs.CronJobs["the0/schedule-schedule-1"]
	require.NotNil(t, cronJob)

	queryBot := createTestBot("bot-2", "price-alerts", "1.0.0", "python3.11")
	queryBot.CustomBotVersion.Config.Entrypoints["query"] = "query.py"
	queryPod, err := podgen.NewPodGenerator(config.queryPodGeneratorConfig()).
		GenerateQueryPod(queryBot, "/status", nil, "bot-2/1/result.json")
	require.NoError(t, err)

	containers := map[string]corev1.Container{
		"realtime bot":  realtime.Containers[0],
		"sync sidecar":  realtime.InitContainers[0],
		"scheduled bot": cronJob.Spec.JobTemplate.Spec.Template.Spec.Containers[0],
		"query pod":     queryPod.Spec.Containers[0],
	}
	for name, c := range containers {
		env := envToMap(c.Env)
		assert.Equal(t, "team-code", env["MINIO_CODE_BUCKET"], name)
		assert.Equal(t, "team-logs", env["MINIO_LOGS_BUCKET"], name)
		assert.Equal(t, "team-results", env["MINIO_QUERY_RESULTS_BUCKET"], name)
	}

	assert.Equal(t, "team-results", config.queryStorageConfig().QueryResultBucket,
		"the query handler must read results from the bucket query pods write them to")
}
