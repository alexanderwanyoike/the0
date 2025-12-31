package imagebuilder

import (
	"context"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	batchv1 "k8s.io/api/batch/v1"

	"runtime/internal/bot-runner/model"
)

func createTestBot(id, name, version, runtime, entrypoint string) model.Bot {
	enabled := true
	return model.Bot{
		ID: id,
		Config: map[string]interface{}{
			"test_key": "test_value",
		},
		CustomBotVersion: model.CustomBotVersion{
			Version:  version,
			FilePath: "custom-bots/" + name + "/" + version + ".tar.gz",
			Config: model.APIBotConfig{
				Name:    name,
				Runtime: runtime,
				Entrypoints: map[string]string{
					"bot": entrypoint,
				},
			},
		},
		Enabled: &enabled,
	}
}

func TestKanikoImageBuilder_EnsureImage_ImageExists(t *testing.T) {
	mockRegistry := NewMockRegistryClient()
	mockRegistry.AddExistingImage("localhost:5000/the0/bots/price-alerts:1.0.0")

	mockJobClient := NewMockK8sJobClient()

	builder := NewKanikoImageBuilder(
		KanikoBuilderConfig{
			Namespace: "the0",
			Registry:  "localhost:5000",
		},
		mockRegistry,
		mockJobClient,
	)

	bot := createTestBot("bot-1", "price-alerts", "1.0.0", "python3.11", "main.py")

	imageRef, err := builder.EnsureImage(context.Background(), bot)

	require.NoError(t, err)
	assert.Equal(t, "localhost:5000/the0/bots/price-alerts:1.0.0", imageRef)
	assert.Equal(t, 0, mockJobClient.CreateCalled, "should not create build job when image exists")
}

func TestKanikoImageBuilder_EnsureImage_BuildsImage(t *testing.T) {
	mockRegistry := NewMockRegistryClient() // No existing images
	mockJobClient := NewMockK8sJobClient()
	mockJobClient.SimulateSuccess = true

	builder := NewKanikoImageBuilder(
		KanikoBuilderConfig{
			Namespace:    "the0",
			Registry:     "localhost:5000",
			BuildTimeout: 1 * time.Minute,
			MinIO: MinIOConfig{
				Endpoint:        "minio:9000",
				AccessKeyID:     "access-key",
				SecretAccessKey: "secret-key",
				Bucket:          "the0-custom-bots",
			},
		},
		mockRegistry,
		mockJobClient,
	)

	bot := createTestBot("bot-1", "sma-crossover", "2.0.0", "nodejs20", "index.js")

	imageRef, err := builder.EnsureImage(context.Background(), bot)

	require.NoError(t, err)
	assert.Equal(t, "localhost:5000/the0/bots/sma-crossover:2.0.0", imageRef)
	assert.Equal(t, 1, mockJobClient.CreateCalled, "should create build job")
	assert.Equal(t, 1, mockJobClient.WaitCalled, "should wait for job completion")

	// Verify job was created with correct properties
	job := mockJobClient.Jobs["the0/build-bot-sma-crossover-2-0-0"]
	require.NotNil(t, job)
	assert.Equal(t, "sma-crossover", job.Labels[LabelCustomBotID])
	assert.Equal(t, "2.0.0", job.Labels[LabelCustomBotVersion])
}

func TestKanikoImageBuilder_EnsureImage_BuildFails(t *testing.T) {
	mockRegistry := NewMockRegistryClient()
	mockJobClient := NewMockK8sJobClient()
	mockJobClient.SimulateSuccess = false

	builder := NewKanikoImageBuilder(
		KanikoBuilderConfig{
			Namespace: "the0",
			Registry:  "localhost:5000",
		},
		mockRegistry,
		mockJobClient,
	)

	bot := createTestBot("bot-1", "failing-bot", "1.0.0", "python3.11", "main.py")

	_, err := builder.EnsureImage(context.Background(), bot)

	require.Error(t, err)
	assert.Contains(t, err.Error(), "failed")
}

func TestKanikoImageBuilder_EnsureImage_NoBotName(t *testing.T) {
	mockRegistry := NewMockRegistryClient()
	mockJobClient := NewMockK8sJobClient()

	builder := NewKanikoImageBuilder(
		KanikoBuilderConfig{
			Namespace: "the0",
			Registry:  "localhost:5000",
		},
		mockRegistry,
		mockJobClient,
	)

	bot := model.Bot{
		ID: "bot-1",
		CustomBotVersion: model.CustomBotVersion{
			Version: "1.0.0",
			Config: model.APIBotConfig{
				Name: "", // Empty name
			},
		},
	}

	_, err := builder.EnsureImage(context.Background(), bot)

	require.Error(t, err)
	assert.Contains(t, err.Error(), "no custom bot name")
}

func TestKanikoImageBuilder_EnsureImage_NoVersion(t *testing.T) {
	mockRegistry := NewMockRegistryClient()
	mockJobClient := NewMockK8sJobClient()

	builder := NewKanikoImageBuilder(
		KanikoBuilderConfig{
			Namespace: "the0",
			Registry:  "localhost:5000",
		},
		mockRegistry,
		mockJobClient,
	)

	bot := model.Bot{
		ID: "bot-1",
		CustomBotVersion: model.CustomBotVersion{
			Version: "", // Empty version
			Config: model.APIBotConfig{
				Name: "test-bot",
			},
		},
	}

	_, err := builder.EnsureImage(context.Background(), bot)

	require.Error(t, err)
	assert.Contains(t, err.Error(), "no version")
}

func TestKanikoImageBuilder_GenerateJobName(t *testing.T) {
	builder := NewKanikoImageBuilder(
		KanikoBuilderConfig{},
		NewMockRegistryClient(),
		NewMockK8sJobClient(),
	)

	tests := []struct {
		customBotID string
		version     string
		expected    string
	}{
		{"price-alerts", "1.0.0", "build-bot-price-alerts-1-0-0"},
		{"sma-crossover", "2.1.0", "build-bot-sma-crossover-2-1-0"},
		{"My Bot", "1.0", "build-bot-my-bot-1-0"},
		{"very-long-name-that-exceeds-kubernetes-name-limit-of-63-chars", "1.0.0", "build-bot-very-long-name-that-exceeds-kubernetes-name-limit-of"},
	}

	for _, tt := range tests {
		t.Run(tt.customBotID, func(t *testing.T) {
			result := builder.generateJobName(tt.customBotID, tt.version)
			assert.Equal(t, tt.expected, result)
			assert.LessOrEqual(t, len(result), 63, "job name should not exceed 63 chars")
			assert.NotEqual(t, '-', result[len(result)-1], "job name should not end with dash")
		})
	}
}

func TestKanikoImageBuilder_GetEntrypoint(t *testing.T) {
	builder := NewKanikoImageBuilder(
		KanikoBuilderConfig{},
		NewMockRegistryClient(),
		NewMockK8sJobClient(),
	)

	t.Run("from entrypoints config", func(t *testing.T) {
		bot := createTestBot("bot-1", "test", "1.0.0", "python3.11", "custom_main.py")
		result := builder.getEntrypoint(bot)
		assert.Equal(t, "custom_main.py", result)
	})

	t.Run("default for python", func(t *testing.T) {
		bot := model.Bot{
			CustomBotVersion: model.CustomBotVersion{
				Config: model.APIBotConfig{
					Runtime: "python3.11",
				},
			},
		}
		result := builder.getEntrypoint(bot)
		assert.Equal(t, "main.py", result)
	})

	t.Run("default for nodejs", func(t *testing.T) {
		bot := model.Bot{
			CustomBotVersion: model.CustomBotVersion{
				Config: model.APIBotConfig{
					Runtime: "nodejs20",
				},
			},
		}
		result := builder.getEntrypoint(bot)
		assert.Equal(t, "index.js", result)
	})

	t.Run("default for unknown runtime", func(t *testing.T) {
		bot := model.Bot{
			CustomBotVersion: model.CustomBotVersion{
				Config: model.APIBotConfig{
					Runtime: "unknown",
				},
			},
		}
		result := builder.getEntrypoint(bot)
		assert.Equal(t, "main", result)
	})
}

func TestKanikoImageBuilder_CreateKanikoJob(t *testing.T) {
	builder := NewKanikoImageBuilder(
		KanikoBuilderConfig{
			Namespace:        "the0",
			Registry:         "localhost:5000",
			KanikoImage:      "gcr.io/kaniko-project/executor:v1.9.0",
			InsecureRegistry: true, // For testing with local registry
			MinIO: MinIOConfig{
				Endpoint:        "minio:9000",
				AccessKeyID:     "access-key",
				SecretAccessKey: "secret-key",
				Bucket:          "the0-custom-bots",
			},
		},
		NewMockRegistryClient(),
		NewMockK8sJobClient(),
	)

	dockerfile := "FROM python:3.11-slim\nWORKDIR /bot\nCOPY . /bot/"
	entrypoint := "#!/bin/bash\npython3 main.py"

	job := builder.createKanikoJob(
		"build-bot-test-1-0-0",
		"test-bot",
		"1.0.0",
		"localhost:5000/the0/bots/test-bot:1.0.0",
		"custom-bots/test-bot/1.0.0.tar.gz",
		dockerfile,
		entrypoint,
	)

	// Check metadata
	assert.Equal(t, "build-bot-test-1-0-0", job.Name)
	assert.Equal(t, "the0", job.Namespace)
	assert.Equal(t, "true", job.Labels[LabelBuildJob])
	assert.Equal(t, "test-bot", job.Labels[LabelCustomBotID])
	assert.Equal(t, "1.0.0", job.Labels[LabelCustomBotVersion])

	// Check spec
	assert.Equal(t, int32(2), *job.Spec.BackoffLimit)
	assert.NotNil(t, job.Spec.TTLSecondsAfterFinished)

	// Check init container
	require.Len(t, job.Spec.Template.Spec.InitContainers, 1)
	initContainer := job.Spec.Template.Spec.InitContainers[0]
	assert.Equal(t, "setup", initContainer.Name)
	assert.Equal(t, "busybox:latest", initContainer.Image)

	// Check kaniko container
	require.Len(t, job.Spec.Template.Spec.Containers, 1)
	kanikoContainer := job.Spec.Template.Spec.Containers[0]
	assert.Equal(t, "kaniko", kanikoContainer.Name)
	assert.Equal(t, "gcr.io/kaniko-project/executor:v1.9.0", kanikoContainer.Image)

	// Check kaniko args
	assert.Contains(t, kanikoContainer.Args, "--dockerfile=/workspace/Dockerfile")
	assert.Contains(t, kanikoContainer.Args, "--destination=localhost:5000/the0/bots/test-bot:1.0.0")
	assert.Contains(t, kanikoContainer.Args, "--insecure")

	// Check environment variables
	envMap := make(map[string]string)
	for _, env := range kanikoContainer.Env {
		envMap[env.Name] = env.Value
	}
	assert.Equal(t, "access-key", envMap["AWS_ACCESS_KEY_ID"])
	assert.Equal(t, "secret-key", envMap["AWS_SECRET_ACCESS_KEY"])
	assert.Equal(t, "minio:9000", envMap["S3_ENDPOINT"])

	// Check volumes
	require.Len(t, job.Spec.Template.Spec.Volumes, 1)
	assert.Equal(t, "workspace", job.Spec.Template.Spec.Volumes[0].Name)
}

func TestEscapeForShell(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{"hello world", "hello world"},
		{"don't", "don'\\''t"},
		{"it's a test", "it'\\''s a test"},
		{"no quotes", "no quotes"},
		{"multiple'single'quotes", "multiple'\\''single'\\''quotes"},
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			result := escapeForShell(tt.input)
			assert.Equal(t, tt.expected, result)
		})
	}
}

func TestMockK8sJobClient(t *testing.T) {
	mock := NewMockK8sJobClient()

	ctx := context.Background()

	t.Run("CreateJob", func(t *testing.T) {
		job := &batchv1.Job{}
		job.Name = "test-job"
		job.Namespace = "test-ns"

		err := mock.CreateJob(ctx, job)
		require.NoError(t, err)
		assert.Equal(t, 1, mock.CreateCalled)
	})

	t.Run("GetJob", func(t *testing.T) {
		job, err := mock.GetJob(ctx, "test-ns", "test-job")
		require.NoError(t, err)
		assert.NotNil(t, job)
		assert.Equal(t, 1, mock.GetCalled)
	})

	t.Run("GetJob not found", func(t *testing.T) {
		job, err := mock.GetJob(ctx, "test-ns", "missing-job")
		require.NoError(t, err)
		assert.Nil(t, job)
	})

	t.Run("DeleteJob", func(t *testing.T) {
		err := mock.DeleteJob(ctx, "test-ns", "test-job")
		require.NoError(t, err)
		assert.Equal(t, 1, mock.DeleteCalled)
	})

	t.Run("WaitForJobCompletion success", func(t *testing.T) {
		mock.SimulateSuccess = true
		err := mock.WaitForJobCompletion(ctx, "test-ns", "test-job", 1*time.Minute)
		require.NoError(t, err)
	})

	t.Run("WaitForJobCompletion failure", func(t *testing.T) {
		mock.SimulateSuccess = false
		err := mock.WaitForJobCompletion(ctx, "test-ns", "test-job", 1*time.Minute)
		require.Error(t, err)
	})
}

func TestNewKanikoImageBuilder_Defaults(t *testing.T) {
	builder := NewKanikoImageBuilder(
		KanikoBuilderConfig{},
		NewMockRegistryClient(),
		NewMockK8sJobClient(),
	)

	assert.Equal(t, "the0", builder.config.Namespace)
	assert.Equal(t, DefaultBuildTimeout, builder.config.BuildTimeout)
	assert.Equal(t, KanikoImage, builder.config.KanikoImage)
}
