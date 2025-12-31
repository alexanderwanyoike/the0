package controller

import (
	"context"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	batchv1 "k8s.io/api/batch/v1"
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"

	botModel "runtime/internal/bot-runner/model"
	scheduleModel "runtime/internal/bot-scheduler/model"
	"runtime/internal/k8s/podgen"
)

// MockBotScheduleRepository is a mock implementation of BotScheduleRepository.
type MockBotScheduleRepository struct {
	Schedules []scheduleModel.BotSchedule
	Error     error
	Called    int
}

func (m *MockBotScheduleRepository) FindAllEnabled(ctx context.Context) ([]scheduleModel.BotSchedule, error) {
	m.Called++
	return m.Schedules, m.Error
}

// MockScheduleImageBuilder mocks the ImageBuilder for schedule tests.
type MockScheduleImageBuilder struct {
	ImageRef string
	Error    error
	Called   int
	BotIDs   []string
}

func (m *MockScheduleImageBuilder) EnsureImage(ctx context.Context, bot botModel.Bot) (string, error) {
	m.Called++
	m.BotIDs = append(m.BotIDs, bot.ID)
	if m.Error != nil {
		return "", m.Error
	}
	return m.ImageRef, nil
}

func createTestSchedule(id, name, version, runtime, schedule string) scheduleModel.BotSchedule {
	enabled := true
	return scheduleModel.BotSchedule{
		ID: id,
		Config: map[string]interface{}{
			"schedule": schedule,
			"symbol":   "BTC/USD",
		},
		CustomBotVersion: scheduleModel.CustomBotVersion{
			Version:  version,
			FilePath: "custom-bots/" + name + "/" + version + ".tar.gz",
			Config: scheduleModel.APIBotConfig{
				Name:    name,
				Runtime: runtime,
				Entrypoints: map[string]string{
					"bot": "main.py",
				},
			},
		},
		Enabled: &enabled,
	}
}

func TestNewBotScheduleController(t *testing.T) {
	mockRepo := &MockBotScheduleRepository{}
	mockCronClient := NewMockK8sCronJobClient()
	mockImageBuilder := &MockScheduleImageBuilder{ImageRef: "registry/test:1.0.0"}

	controller := NewBotScheduleController(
		BotScheduleControllerConfig{},
		mockRepo,
		mockCronClient,
		mockImageBuilder,
	)

	assert.NotNil(t, controller)
	assert.Equal(t, "the0", controller.config.Namespace)
	assert.Equal(t, 30*time.Second, controller.config.ReconcileInterval)
	assert.Equal(t, "the0-schedule-controller", controller.config.ControllerName)
}

func TestBotScheduleController_Reconcile_CreatesCronJob(t *testing.T) {
	schedule := createTestSchedule("schedule-1", "daily-report", "1.0.0", "python3.11", "0 9 * * *")

	mockRepo := &MockBotScheduleRepository{
		Schedules: []scheduleModel.BotSchedule{schedule},
	}
	mockCronClient := NewMockK8sCronJobClient()
	mockImageBuilder := &MockScheduleImageBuilder{
		ImageRef: "localhost:5000/the0/bots/daily-report:1.0.0",
	}

	controller := NewBotScheduleController(
		BotScheduleControllerConfig{
			Namespace:      "the0",
			ControllerName: "test-controller",
		},
		mockRepo,
		mockCronClient,
		mockImageBuilder,
	)

	err := controller.Reconcile(context.Background())

	require.NoError(t, err)
	assert.Equal(t, 1, mockCronClient.CreateCalled)
	assert.Equal(t, 1, mockImageBuilder.Called)

	// Verify CronJob was created
	cronJob := mockCronClient.CronJobs["the0/schedule-schedule-1"]
	require.NotNil(t, cronJob)
	assert.Equal(t, "schedule-schedule-1", cronJob.Name)
	assert.Equal(t, "0 9 * * *", cronJob.Spec.Schedule)
	assert.Equal(t, "schedule-1", cronJob.Labels[LabelScheduleID])
	assert.Equal(t, "daily-report", cronJob.Labels[podgen.LabelCustomBotID])
}

func TestBotScheduleController_Reconcile_DeletesOrphanedCronJob(t *testing.T) {
	mockRepo := &MockBotScheduleRepository{
		Schedules: []scheduleModel.BotSchedule{}, // No schedules
	}
	mockCronClient := NewMockK8sCronJobClient()
	mockCronClient.CronJobs["the0/schedule-orphan"] = &batchv1.CronJob{
		ObjectMeta: metav1.ObjectMeta{
			Name:      "schedule-orphan",
			Namespace: "the0",
			Labels: map[string]string{
				LabelScheduleID: "orphan",
			},
		},
	}
	mockImageBuilder := &MockScheduleImageBuilder{}

	controller := NewBotScheduleController(
		BotScheduleControllerConfig{Namespace: "the0"},
		mockRepo,
		mockCronClient,
		mockImageBuilder,
	)

	err := controller.Reconcile(context.Background())

	require.NoError(t, err)
	assert.Equal(t, 1, mockCronClient.DeleteCalled)
	assert.Nil(t, mockCronClient.CronJobs["the0/schedule-orphan"])
}

func TestBotScheduleController_Reconcile_UpdatesCronJobOnChange(t *testing.T) {
	schedule := createTestSchedule("schedule-1", "daily-report", "2.0.0", "python3.11", "0 10 * * *")

	mockRepo := &MockBotScheduleRepository{
		Schedules: []scheduleModel.BotSchedule{schedule},
	}
	mockCronClient := NewMockK8sCronJobClient()
	mockCronClient.CronJobs["the0/schedule-schedule-1"] = &batchv1.CronJob{
		ObjectMeta: metav1.ObjectMeta{
			Name:      "schedule-schedule-1",
			Namespace: "the0",
			Labels: map[string]string{
				LabelScheduleID: "schedule-1",
			},
			Annotations: map[string]string{
				AnnotationScheduleHash: "old-hash-value",
			},
		},
		Spec: batchv1.CronJobSpec{
			Schedule: "0 9 * * *", // Old schedule
		},
	}
	mockImageBuilder := &MockScheduleImageBuilder{
		ImageRef: "localhost:5000/the0/bots/daily-report:2.0.0",
	}

	controller := NewBotScheduleController(
		BotScheduleControllerConfig{Namespace: "the0"},
		mockRepo,
		mockCronClient,
		mockImageBuilder,
	)

	err := controller.Reconcile(context.Background())

	require.NoError(t, err)
	assert.Equal(t, 1, mockCronClient.UpdateCalled)
	assert.Equal(t, 0, mockCronClient.CreateCalled)

	// Verify CronJob was updated
	cronJob := mockCronClient.CronJobs["the0/schedule-schedule-1"]
	require.NotNil(t, cronJob)
	assert.Equal(t, "0 10 * * *", cronJob.Spec.Schedule)
}

func TestBotScheduleController_Reconcile_NoCronJobForScheduleWithoutExpression(t *testing.T) {
	schedule := scheduleModel.BotSchedule{
		ID:     "schedule-1",
		Config: map[string]interface{}{}, // No schedule expression
		CustomBotVersion: scheduleModel.CustomBotVersion{
			Version: "1.0.0",
			Config: scheduleModel.APIBotConfig{
				Name:    "test-bot",
				Runtime: "python3.11",
			},
		},
	}

	mockRepo := &MockBotScheduleRepository{
		Schedules: []scheduleModel.BotSchedule{schedule},
	}
	mockCronClient := NewMockK8sCronJobClient()
	mockImageBuilder := &MockScheduleImageBuilder{}

	controller := NewBotScheduleController(
		BotScheduleControllerConfig{Namespace: "the0"},
		mockRepo,
		mockCronClient,
		mockImageBuilder,
	)

	err := controller.Reconcile(context.Background())

	require.NoError(t, err)
	assert.Equal(t, 0, mockCronClient.CreateCalled, "should not create CronJob without schedule expression")
}

func TestBotScheduleController_Reconcile_NoChangesNeeded(t *testing.T) {
	schedule := createTestSchedule("schedule-1", "daily-report", "1.0.0", "python3.11", "0 9 * * *")
	configHash := computeScheduleHash(schedule)

	mockRepo := &MockBotScheduleRepository{
		Schedules: []scheduleModel.BotSchedule{schedule},
	}
	mockCronClient := NewMockK8sCronJobClient()
	mockCronClient.CronJobs["the0/schedule-schedule-1"] = &batchv1.CronJob{
		ObjectMeta: metav1.ObjectMeta{
			Name:      "schedule-schedule-1",
			Namespace: "the0",
			Labels: map[string]string{
				LabelScheduleID: "schedule-1",
			},
			Annotations: map[string]string{
				AnnotationScheduleHash: configHash,
			},
		},
		Spec: batchv1.CronJobSpec{
			Schedule: "0 9 * * *",
		},
	}
	mockImageBuilder := &MockScheduleImageBuilder{}

	controller := NewBotScheduleController(
		BotScheduleControllerConfig{Namespace: "the0"},
		mockRepo,
		mockCronClient,
		mockImageBuilder,
	)

	err := controller.Reconcile(context.Background())

	require.NoError(t, err)
	assert.Equal(t, 0, mockCronClient.CreateCalled)
	assert.Equal(t, 0, mockCronClient.UpdateCalled)
	assert.Equal(t, 0, mockCronClient.DeleteCalled)
}

func TestConvertToK8sCronFormat(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected string
	}{
		{"5 fields unchanged", "0 9 * * *", "0 9 * * *"},
		{"6 fields strips seconds", "0 0 9 * * *", "0 9 * * *"},
		{"5 fields with ranges", "0-30 */2 * * 1-5", "0-30 */2 * * 1-5"},
		{"6 fields strips seconds preserves rest", "30 0 9 1 * 0", "0 9 1 * 0"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := convertToK8sCronFormat(tt.input)
			assert.Equal(t, tt.expected, result)
		})
	}
}

func TestGenerateCronJobName(t *testing.T) {
	tests := []struct {
		scheduleID string
		expected   string
	}{
		{"schedule-1", "schedule-schedule-1"},
		{"my-schedule", "schedule-my-schedule"},
		{"very-long-schedule-id-that-needs-to-be-truncated-for-k8s-name-limit", "schedule-very-long-schedule-id-that-needs-to-be-trun"},
	}

	for _, tt := range tests {
		t.Run(tt.scheduleID, func(t *testing.T) {
			result := generateCronJobName(tt.scheduleID)
			assert.Equal(t, tt.expected, result)
			assert.LessOrEqual(t, len(result), 52, "name should not exceed 52 chars")
		})
	}
}

func TestComputeScheduleHash(t *testing.T) {
	schedule1 := createTestSchedule("schedule-1", "daily-report", "1.0.0", "python3.11", "0 9 * * *")
	schedule2 := createTestSchedule("schedule-1", "daily-report", "1.0.0", "python3.11", "0 10 * * *")
	schedule3 := createTestSchedule("schedule-1", "daily-report", "2.0.0", "python3.11", "0 9 * * *")

	hash1 := computeScheduleHash(schedule1)
	hash2 := computeScheduleHash(schedule2)
	hash3 := computeScheduleHash(schedule3)

	// Hashes should be 16 characters
	assert.Len(t, hash1, 16)

	// Different configs should produce different hashes
	assert.NotEqual(t, hash1, hash2, "different schedule expressions should produce different hashes")
	assert.NotEqual(t, hash1, hash3, "different versions should produce different hashes")

	// Same config should produce same hash
	schedule1Copy := createTestSchedule("schedule-1", "daily-report", "1.0.0", "python3.11", "0 9 * * *")
	assert.Equal(t, hash1, computeScheduleHash(schedule1Copy))
}

func TestScheduleToBot(t *testing.T) {
	mockRepo := &MockBotScheduleRepository{}
	mockCronClient := NewMockK8sCronJobClient()
	mockImageBuilder := &MockScheduleImageBuilder{}

	controller := NewBotScheduleController(
		BotScheduleControllerConfig{},
		mockRepo,
		mockCronClient,
		mockImageBuilder,
	)

	schedule := createTestSchedule("schedule-1", "daily-report", "1.0.0", "python3.11", "0 9 * * *")
	bot := controller.scheduleToBot(schedule)

	assert.Equal(t, "schedule-1", bot.ID)
	assert.Equal(t, "daily-report", bot.CustomBotVersion.Config.Name)
	assert.Equal(t, "1.0.0", bot.CustomBotVersion.Version)
	assert.Equal(t, "python3.11", bot.CustomBotVersion.Config.Runtime)
	assert.NotNil(t, bot.Enabled)
	assert.True(t, *bot.Enabled)
}

func TestBotScheduleController_StartStop(t *testing.T) {
	mockRepo := &MockBotScheduleRepository{}
	mockCronClient := NewMockK8sCronJobClient()
	mockImageBuilder := &MockScheduleImageBuilder{}

	controller := NewBotScheduleController(
		BotScheduleControllerConfig{
			ReconcileInterval: 100 * time.Millisecond,
		},
		mockRepo,
		mockCronClient,
		mockImageBuilder,
	)

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Start controller in goroutine
	done := make(chan error, 1)
	go func() {
		done <- controller.Start(ctx)
	}()

	// Wait for at least one reconciliation
	time.Sleep(50 * time.Millisecond)

	// Stop controller
	controller.Stop()

	// Wait for controller to stop
	select {
	case err := <-done:
		assert.NoError(t, err)
	case <-time.After(1 * time.Second):
		t.Fatal("controller did not stop in time")
	}

	assert.GreaterOrEqual(t, mockRepo.Called, 1)
}

func TestBotScheduleController_DoubleStart(t *testing.T) {
	mockRepo := &MockBotScheduleRepository{}
	mockCronClient := NewMockK8sCronJobClient()
	mockImageBuilder := &MockScheduleImageBuilder{}

	controller := NewBotScheduleController(
		BotScheduleControllerConfig{
			ReconcileInterval: 1 * time.Second,
		},
		mockRepo,
		mockCronClient,
		mockImageBuilder,
	)

	ctx := context.Background()

	// Start controller in goroutine
	go controller.Start(ctx)
	time.Sleep(50 * time.Millisecond)

	// Try to start again
	err := controller.Start(ctx)
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "already running")

	controller.Stop()
}

func TestMockK8sCronJobClient(t *testing.T) {
	mock := NewMockK8sCronJobClient()
	ctx := context.Background()

	t.Run("CreateCronJob", func(t *testing.T) {
		cronJob := &batchv1.CronJob{
			ObjectMeta: metav1.ObjectMeta{
				Name:      "test-cronjob",
				Namespace: "test-ns",
			},
		}

		err := mock.CreateCronJob(ctx, cronJob)
		require.NoError(t, err)
		assert.Equal(t, 1, mock.CreateCalled)
	})

	t.Run("GetCronJob", func(t *testing.T) {
		cronJob, err := mock.GetCronJob(ctx, "test-ns", "test-cronjob")
		require.NoError(t, err)
		assert.NotNil(t, cronJob)
	})

	t.Run("GetCronJob not found", func(t *testing.T) {
		cronJob, err := mock.GetCronJob(ctx, "test-ns", "missing")
		require.NoError(t, err)
		assert.Nil(t, cronJob)
	})

	t.Run("ListCronJobs", func(t *testing.T) {
		jobs, err := mock.ListCronJobs(ctx, "test-ns")
		require.NoError(t, err)
		assert.Len(t, jobs, 1)
		assert.Equal(t, 1, mock.ListCalled)
	})

	t.Run("UpdateCronJob", func(t *testing.T) {
		cronJob := &batchv1.CronJob{
			ObjectMeta: metav1.ObjectMeta{
				Name:      "test-cronjob",
				Namespace: "test-ns",
			},
			Spec: batchv1.CronJobSpec{
				Schedule: "*/5 * * * *",
			},
		}

		err := mock.UpdateCronJob(ctx, cronJob)
		require.NoError(t, err)
		assert.Equal(t, 1, mock.UpdateCalled)
	})

	t.Run("DeleteCronJob", func(t *testing.T) {
		err := mock.DeleteCronJob(ctx, "test-ns", "test-cronjob")
		require.NoError(t, err)
		assert.Equal(t, 1, mock.DeleteCalled)
	})
}

func TestMustParseQuantityWithDefault(t *testing.T) {
	tests := []struct {
		name         string
		input        string
		defaultValue string
		isValid      bool
		expectValue  string
	}{
		{"valid memory", "512Mi", "128Mi", true, "512Mi"},
		{"valid cpu", "500m", "100m", true, "500m"},
		{"valid large memory", "1Gi", "512Mi", true, "1Gi"},
		{"invalid with memory default", "invalid", "128Mi", false, "128Mi"},
		{"invalid with cpu default", "invalid", "100m", false, "100m"},
		{"empty with memory default", "", "512Mi", false, "512Mi"},
		{"empty with cpu default", "", "500m", false, "500m"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := mustParseQuantityWithDefault(tt.input, tt.defaultValue)
			assert.False(t, result.IsZero(), "quantity should never be zero")
			assert.Equal(t, tt.expectValue, result.String())
		})
	}
}

func TestCronJobSpec(t *testing.T) {
	schedule := createTestSchedule("schedule-1", "daily-report", "1.0.0", "python3.11", "0 9 * * *")

	mockRepo := &MockBotScheduleRepository{}
	mockCronClient := NewMockK8sCronJobClient()
	mockImageBuilder := &MockScheduleImageBuilder{}

	controller := NewBotScheduleController(
		BotScheduleControllerConfig{
			Namespace:      "the0",
			ControllerName: "test-controller",
		},
		mockRepo,
		mockCronClient,
		mockImageBuilder,
	)

	cronJob := controller.createCronJobSpec(schedule, "0 9 * * *", "registry/image:1.0.0")

	// Check metadata
	assert.Equal(t, "schedule-schedule-1", cronJob.Name)
	assert.Equal(t, "the0", cronJob.Namespace)
	assert.Equal(t, "schedule-1", cronJob.Labels[LabelScheduleID])
	assert.Equal(t, "daily-report", cronJob.Labels[podgen.LabelCustomBotID])
	assert.Equal(t, "test-controller", cronJob.Labels[LabelScheduleManagedBy])

	// Check spec
	assert.Equal(t, "0 9 * * *", cronJob.Spec.Schedule)
	assert.Equal(t, batchv1.ForbidConcurrent, cronJob.Spec.ConcurrencyPolicy)

	// Check job template
	container := cronJob.Spec.JobTemplate.Spec.Template.Spec.Containers[0]
	assert.Equal(t, "bot", container.Name)
	assert.Equal(t, "registry/image:1.0.0", container.Image)
	assert.Equal(t, []string{"/bin/bash", "/bot/entrypoint.sh"}, container.Command)

	// Check env vars
	envMap := make(map[string]string)
	for _, env := range container.Env {
		envMap[env.Name] = env.Value
	}
	assert.Equal(t, "schedule-1", envMap["BOT_ID"])
	assert.Contains(t, envMap["BOT_CONFIG"], "schedule")
}
