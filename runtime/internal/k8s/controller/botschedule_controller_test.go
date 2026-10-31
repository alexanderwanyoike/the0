package controller

import (
	"context"
	"errors"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	batchv1 "k8s.io/api/batch/v1"
	corev1 "k8s.io/api/core/v1"
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"

	"runtime/internal/k8s/podgen"
	"runtime/internal/launch"
	"runtime/internal/model"
)

// MockDesiredSchedules is a mock implementation of DesiredSchedules.
type MockDesiredSchedules struct {
	Running []model.BotSchedule
	Error   error
	Called  int
}

func (m *MockDesiredSchedules) Schedules(ctx context.Context) ([]model.BotSchedule, error) {
	m.Called++
	return m.Running, m.Error
}

func createTestSchedule(id, name, version, runtime, schedule string) model.BotSchedule {
	return model.BotSchedule{
		ID: id,
		Config: map[string]interface{}{
			"schedule": schedule,
			"symbol":   "BTC/USD",
		},
		CustomBotVersion: model.CustomBotVersion{
			Version:  version,
			FilePath: "custom-bots/" + name + "/" + version + ".zip",
			Config: model.APIBotConfig{
				Name:    name,
				Runtime: runtime,
				Entrypoints: map[string]string{
					"bot": "main.py",
				},
			},
		},
	}
}

func TestNewBotScheduleController(t *testing.T) {
	mockRepo := &MockDesiredSchedules{}
	mockCronClient := NewMockK8sCronJobClient()

	controller := NewBotScheduleController(
		BotScheduleControllerConfig{},
		mockRepo,
		mockCronClient,
	)

	assert.NotNil(t, controller)
	assert.Equal(t, "the0", controller.config.Namespace)
	assert.Equal(t, 30*time.Second, controller.config.ReconcileInterval)
	assert.Equal(t, "the0-schedule-controller", controller.config.ControllerName)
}

func TestBotScheduleController_Reconcile_CreatesCronJob(t *testing.T) {
	schedule := createTestSchedule("schedule-1", "daily-report", "1.0.0", "python3.11", "0 9 * * *")

	mockRepo := &MockDesiredSchedules{
		Running: []model.BotSchedule{schedule},
	}
	mockCronClient := NewMockK8sCronJobClient()

	controller := NewBotScheduleController(
		BotScheduleControllerConfig{
			Namespace:      "the0",
			ControllerName: "test-controller",
		},
		mockRepo,
		mockCronClient,
	)

	err := controller.Reconcile(context.Background())

	require.NoError(t, err)
	assert.Equal(t, 1, mockCronClient.CreateCalled)

	// Verify CronJob was created
	cronJob := mockCronClient.CronJobs["the0/schedule-schedule-1"]
	require.NotNil(t, cronJob)
	assert.Equal(t, "schedule-schedule-1", cronJob.Name)
	assert.Equal(t, "0 9 * * *", cronJob.Spec.Schedule)
	assert.Equal(t, "schedule-1", cronJob.Labels[LabelScheduleID])
	assert.Equal(t, "daily-report", cronJob.Labels[podgen.LabelCustomBotID])
}

func TestBotScheduleController_Reconcile_DeletesOrphanedCronJob(t *testing.T) {
	mockRepo := &MockDesiredSchedules{
		Running: []model.BotSchedule{}, // No schedules
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

	controller := NewBotScheduleController(
		BotScheduleControllerConfig{Namespace: "the0"},
		mockRepo,
		mockCronClient,
	)

	err := controller.Reconcile(context.Background())

	require.NoError(t, err)
	assert.Equal(t, 1, mockCronClient.DeleteCalled)
	assert.Nil(t, mockCronClient.CronJobs["the0/schedule-orphan"])
}

func TestBotScheduleController_Reconcile_UpdatesCronJobOnChange(t *testing.T) {
	schedule := createTestSchedule("schedule-1", "daily-report", "2.0.0", "python3.11", "0 10 * * *")

	mockRepo := &MockDesiredSchedules{
		Running: []model.BotSchedule{schedule},
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

	controller := NewBotScheduleController(
		BotScheduleControllerConfig{Namespace: "the0"},
		mockRepo,
		mockCronClient,
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
	schedule := model.BotSchedule{
		ID:     "schedule-1",
		Config: map[string]interface{}{}, // No schedule expression
		CustomBotVersion: model.CustomBotVersion{
			Version: "1.0.0",
			Config: model.APIBotConfig{
				Name:    "test-bot",
				Runtime: "python3.11",
			},
		},
	}

	mockRepo := &MockDesiredSchedules{
		Running: []model.BotSchedule{schedule},
	}
	mockCronClient := NewMockK8sCronJobClient()

	controller := NewBotScheduleController(
		BotScheduleControllerConfig{Namespace: "the0"},
		mockRepo,
		mockCronClient,
	)

	err := controller.Reconcile(context.Background())

	require.NoError(t, err)
	assert.Equal(t, 0, mockCronClient.CreateCalled, "should not create CronJob without schedule expression")
}

func TestBotScheduleController_Reconcile_NoCronJobForASixFieldCron(t *testing.T) {
	schedule := createTestSchedule("schedule-1", "daily-report", "1.0.0", "python3.11", "30 0 9 * * *")

	mockRepo := &MockDesiredSchedules{
		Running: []model.BotSchedule{schedule},
	}
	mockCronClient := NewMockK8sCronJobClient()

	controller := NewBotScheduleController(
		BotScheduleControllerConfig{Namespace: "the0"},
		mockRepo,
		mockCronClient,
	)

	err := controller.Reconcile(context.Background())

	require.NoError(t, err)
	assert.Equal(t, 0, mockCronClient.CreateCalled,
		"a seconds field must be refused, not dropped into a CronJob that runs at another time")
}

func TestBotScheduleController_Reconcile_KeepsTheCronJobWhenTheCronGainsASecondsField(t *testing.T) {
	schedule := createTestSchedule("schedule-1", "daily-report", "1.0.0", "python3.11", "30 0 10 * * *")

	mockRepo := &MockDesiredSchedules{
		Running: []model.BotSchedule{schedule},
	}
	mockCronClient := NewMockK8sCronJobClient()
	mockCronClient.CronJobs["the0/schedule-schedule-1"] = &batchv1.CronJob{
		ObjectMeta: metav1.ObjectMeta{
			Name:        "schedule-schedule-1",
			Namespace:   "the0",
			Labels:      map[string]string{LabelScheduleID: "schedule-1"},
			Annotations: map[string]string{AnnotationScheduleHash: "old-hash-value"},
		},
		Spec: batchv1.CronJobSpec{Schedule: "0 9 * * *"},
	}

	controller := NewBotScheduleController(
		BotScheduleControllerConfig{Namespace: "the0"},
		mockRepo,
		mockCronClient,
	)

	err := controller.Reconcile(context.Background())

	require.NoError(t, err)
	assert.Equal(t, 0, mockCronClient.UpdateCalled,
		"a seconds field must be refused, not dropped into a CronJob that runs at another time")
	assert.Equal(t, "0 9 * * *", mockCronClient.CronJobs["the0/schedule-schedule-1"].Spec.Schedule)
}

func TestBotScheduleController_Reconcile_NoChangesNeeded(t *testing.T) {
	schedule := createTestSchedule("schedule-1", "daily-report", "1.0.0", "python3.11", "0 9 * * *")
	mockRepo := &MockDesiredSchedules{
		Running: []model.BotSchedule{schedule},
	}
	mockCronClient := NewMockK8sCronJobClient()
	controller := NewBotScheduleController(
		BotScheduleControllerConfig{Namespace: "the0"},
		mockRepo,
		mockCronClient,
	)
	generated, err := controller.createCronJobSpec(schedule, "0 9 * * *")
	require.NoError(t, err)
	configHash := generated.Annotations[AnnotationScheduleHash]
	noRetries := int32(0)

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
			JobTemplate: batchv1.JobTemplateSpec{
				Spec: batchv1.JobSpec{BackoffLimit: &noRetries},
			},
		},
	}

	err = controller.Reconcile(context.Background())

	require.NoError(t, err)
	assert.Equal(t, 0, mockCronClient.CreateCalled)
	assert.Equal(t, 0, mockCronClient.UpdateCalled)
	assert.Equal(t, 0, mockCronClient.DeleteCalled)
}

func TestBotScheduleController_Reconcile_ScheduledRunsAreNotRetried(t *testing.T) {
	schedule := createTestSchedule("schedule-1", "daily-report", "1.0.0", "python3.11", "0 9 * * *")
	mockCronClient := NewMockK8sCronJobClient()

	controller := NewBotScheduleController(
		BotScheduleControllerConfig{Namespace: "the0", ControllerName: "test-controller"},
		&MockDesiredSchedules{Running: []model.BotSchedule{schedule}},
		mockCronClient,
	)

	require.NoError(t, controller.Reconcile(context.Background()))

	cronJob := mockCronClient.CronJobs["the0/schedule-schedule-1"]
	require.NotNil(t, cronJob)
	jobSpec := cronJob.Spec.JobTemplate.Spec
	require.NotNil(t, jobSpec.BackoffLimit, "an unset backoffLimit lets Kubernetes retry a failed run 6 times")
	assert.Equal(t, int32(0), *jobSpec.BackoffLimit, "a failed scheduled run must not be retried")
	assert.Equal(t, corev1.RestartPolicyNever, jobSpec.Template.Spec.RestartPolicy,
		"a failed bot container must not be restarted in place either")
}

func TestBotScheduleController_Reconcile_StopsRetriesOnExistingCronJobs(t *testing.T) {
	previousLimit := int32(2)
	cases := []struct {
		name         string
		backoffLimit *int32
	}{
		{name: "limit of 2 set by earlier releases", backoffLimit: &previousLimit},
		{name: "unset limit, which Kubernetes defaults to 6", backoffLimit: nil},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			schedule := createTestSchedule("schedule-1", "daily-report", "1.0.0", "python3.11", "0 9 * * *")
			mockCronClient := NewMockK8sCronJobClient()
			controller := NewBotScheduleController(
				BotScheduleControllerConfig{Namespace: "the0", ControllerName: "test-controller"},
				&MockDesiredSchedules{Running: []model.BotSchedule{schedule}},
				mockCronClient,
			)

			// Seed the CronJob this controller generates, so the retry limit is the only difference
			// and the schedule hash still matches.
			require.NoError(t, controller.Reconcile(context.Background()))
			require.Equal(t, 1, mockCronClient.CreateCalled)
			mockCronClient.CronJobs["the0/schedule-schedule-1"].Spec.JobTemplate.Spec.BackoffLimit = tc.backoffLimit

			require.NoError(t, controller.Reconcile(context.Background()))

			assert.Equal(t, 1, mockCronClient.UpdateCalled,
				"a CronJob that still retries failed runs must be updated without any other change")
			cronJob := mockCronClient.CronJobs["the0/schedule-schedule-1"]
			require.NotNil(t, cronJob)
			require.NotNil(t, cronJob.Spec.JobTemplate.Spec.BackoffLimit)
			assert.Equal(t, int32(0), *cronJob.Spec.JobTemplate.Spec.BackoffLimit)
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

	controller := NewBotScheduleController(BotScheduleControllerConfig{}, &MockDesiredSchedules{}, NewMockK8sCronJobClient())
	hashOf := func(schedule model.BotSchedule) string {
		cronJob, err := controller.createCronJobSpec(schedule, schedule.Config["schedule"].(string))
		require.NoError(t, err)
		return computeScheduleHash(schedule, cronJob)
	}

	hash1 := hashOf(schedule1)
	hash2 := hashOf(schedule2)
	hash3 := hashOf(schedule3)

	// Hashes should be 16 characters
	assert.Len(t, hash1, 16)

	// Different configs should produce different hashes
	assert.NotEqual(t, hash1, hash2, "different schedule expressions should produce different hashes")
	assert.NotEqual(t, hash1, hash3, "different versions should produce different hashes")

	// Same config should produce same hash
	schedule1Copy := createTestSchedule("schedule-1", "daily-report", "1.0.0", "python3.11", "0 9 * * *")
	assert.Equal(t, hash1, hashOf(schedule1Copy))
}

func TestBotScheduleController_StartStop(t *testing.T) {
	mockRepo := &MockDesiredSchedules{}
	mockCronClient := NewMockK8sCronJobClient()

	controller := NewBotScheduleController(
		BotScheduleControllerConfig{
			ReconcileInterval: 100 * time.Millisecond,
		},
		mockRepo,
		mockCronClient,
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
	mockRepo := &MockDesiredSchedules{}
	mockCronClient := NewMockK8sCronJobClient()

	controller := NewBotScheduleController(
		BotScheduleControllerConfig{
			ReconcileInterval: 1 * time.Second,
		},
		mockRepo,
		mockCronClient,
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

// ---- Additional Coverage Tests ----

func TestBotScheduleController_StopWhenNotRunning(t *testing.T) {
	mockRepo := &MockDesiredSchedules{}
	mockCronClient := NewMockK8sCronJobClient()

	controller := NewBotScheduleController(
		BotScheduleControllerConfig{},
		mockRepo,
		mockCronClient,
	)

	// Stop without starting - should not panic
	controller.Stop()
	controller.Stop() // Double stop should also be safe
}

func TestBotScheduleController_ContextCancellation(t *testing.T) {
	mockRepo := &MockDesiredSchedules{}
	mockCronClient := NewMockK8sCronJobClient()

	controller := NewBotScheduleController(
		BotScheduleControllerConfig{
			ReconcileInterval: 1 * time.Second,
		},
		mockRepo,
		mockCronClient,
	)

	ctx, cancel := context.WithCancel(context.Background())

	done := make(chan error, 1)
	go func() {
		done <- controller.Start(ctx)
	}()

	time.Sleep(50 * time.Millisecond)
	cancel() // Cancel context instead of calling Stop()

	select {
	case err := <-done:
		assert.Equal(t, context.Canceled, err)
	case <-time.After(1 * time.Second):
		t.Fatal("controller did not stop after context cancellation")
	}
}

func TestBotScheduleController_Reconcile_RepoError(t *testing.T) {
	mockRepo := &MockDesiredSchedules{
		Error: errors.New("database connection lost"),
	}
	mockCronClient := NewMockK8sCronJobClient()

	controller := NewBotScheduleController(
		BotScheduleControllerConfig{Namespace: "the0"},
		mockRepo,
		mockCronClient,
	)

	err := controller.Reconcile(context.Background())

	require.Error(t, err)
	assert.Contains(t, err.Error(), "database connection lost")
	assert.Equal(t, 0, mockCronClient.ListCalled, "should not query K8s on repo error")
}

func TestBotScheduleController_Reconcile_K8sListError(t *testing.T) {
	schedule := createTestSchedule("schedule-1", "daily-report", "1.0.0", "python3.11", "0 9 * * *")

	mockRepo := &MockDesiredSchedules{
		Running: []model.BotSchedule{schedule},
	}
	mockCronClient := NewMockK8sCronJobClient()
	mockCronClient.ListError = errors.New("k8s API error")

	controller := NewBotScheduleController(
		BotScheduleControllerConfig{Namespace: "the0"},
		mockRepo,
		mockCronClient,
	)

	err := controller.Reconcile(context.Background())

	require.Error(t, err)
	assert.Contains(t, err.Error(), "k8s API error")
}

func TestBotScheduleController_Reconcile_CreateError(t *testing.T) {
	schedule := createTestSchedule("schedule-1", "daily-report", "1.0.0", "python3.11", "0 9 * * *")

	mockRepo := &MockDesiredSchedules{
		Running: []model.BotSchedule{schedule},
	}
	mockCronClient := NewMockK8sCronJobClient()
	mockCronClient.CreateError = errors.New("quota exceeded")

	controller := NewBotScheduleController(
		BotScheduleControllerConfig{Namespace: "the0"},
		mockRepo,
		mockCronClient,
	)

	// Reconcile should not fail - just log the error
	err := controller.Reconcile(context.Background())
	assert.NoError(t, err)
	assert.Equal(t, 1, mockCronClient.CreateCalled)
}

func TestBotScheduleController_Reconcile_UpdateError(t *testing.T) {
	schedule := createTestSchedule("schedule-1", "daily-report", "2.0.0", "python3.11", "0 10 * * *")

	mockRepo := &MockDesiredSchedules{
		Running: []model.BotSchedule{schedule},
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
	}
	mockCronClient.UpdateError = errors.New("conflict")

	controller := NewBotScheduleController(
		BotScheduleControllerConfig{Namespace: "the0"},
		mockRepo,
		mockCronClient,
	)

	// Reconcile should not fail - just log the error
	err := controller.Reconcile(context.Background())
	assert.NoError(t, err)
	assert.Equal(t, 1, mockCronClient.UpdateCalled)
}

func TestBotScheduleController_Reconcile_DeleteError(t *testing.T) {
	mockRepo := &MockDesiredSchedules{
		Running: []model.BotSchedule{}, // No schedules
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
	mockCronClient.DeleteError = errors.New("forbidden")

	controller := NewBotScheduleController(
		BotScheduleControllerConfig{Namespace: "the0"},
		mockRepo,
		mockCronClient,
	)

	// Reconcile should not fail - just log the error
	err := controller.Reconcile(context.Background())
	assert.NoError(t, err)
	assert.Equal(t, 1, mockCronClient.DeleteCalled)
}

func TestExtractScheduleID_NilLabels(t *testing.T) {
	cronJob := &batchv1.CronJob{
		ObjectMeta: metav1.ObjectMeta{
			Name:   "test-cronjob",
			Labels: nil, // No labels
		},
	}

	result := extractScheduleID(cronJob)
	assert.Equal(t, "", result)
}

func TestExtractScheduleID_MissingLabel(t *testing.T) {
	cronJob := &batchv1.CronJob{
		ObjectMeta: metav1.ObjectMeta{
			Name: "test-cronjob",
			Labels: map[string]string{
				"other-label": "value",
			},
		},
	}

	result := extractScheduleID(cronJob)
	assert.Equal(t, "", result)
}

func TestExtractScheduleID_WithLabel(t *testing.T) {
	cronJob := &batchv1.CronJob{
		ObjectMeta: metav1.ObjectMeta{
			Name: "test-cronjob",
			Labels: map[string]string{
				LabelScheduleID: "my-schedule-id",
			},
		},
	}

	result := extractScheduleID(cronJob)
	assert.Equal(t, "my-schedule-id", result)
}

func TestBotScheduleController_MultipleSchedules(t *testing.T) {
	schedules := []model.BotSchedule{
		createTestSchedule("schedule-1", "daily-report", "1.0.0", "python3.11", "0 9 * * *"),
		createTestSchedule("schedule-2", "weekly-summary", "1.0.0", "nodejs20", "0 0 * * 0"),
		createTestSchedule("schedule-3", "hourly-check", "2.0.0", "python3.11", "0 * * * *"),
	}

	mockRepo := &MockDesiredSchedules{
		Running: schedules,
	}
	mockCronClient := NewMockK8sCronJobClient()

	controller := NewBotScheduleController(
		BotScheduleControllerConfig{Namespace: "the0"},
		mockRepo,
		mockCronClient,
	)

	err := controller.Reconcile(context.Background())

	require.NoError(t, err)
	assert.Equal(t, 3, mockCronClient.CreateCalled)
	assert.Len(t, mockCronClient.CronJobs, 3)
}

func TestMockK8sCronJobClient_Errors(t *testing.T) {
	ctx := context.Background()

	t.Run("ListError", func(t *testing.T) {
		mock := NewMockK8sCronJobClient()
		mock.ListError = errors.New("list error")

		_, err := mock.ListCronJobs(ctx, "test")
		assert.Error(t, err)
		assert.Equal(t, "list error", err.Error())
	})

	t.Run("CreateError", func(t *testing.T) {
		mock := NewMockK8sCronJobClient()
		mock.CreateError = errors.New("create error")

		err := mock.CreateCronJob(ctx, &batchv1.CronJob{})
		assert.Error(t, err)
		assert.Equal(t, "create error", err.Error())
	})

	t.Run("UpdateError", func(t *testing.T) {
		mock := NewMockK8sCronJobClient()
		mock.UpdateError = errors.New("update error")

		err := mock.UpdateCronJob(ctx, &batchv1.CronJob{})
		assert.Error(t, err)
		assert.Equal(t, "update error", err.Error())
	})

	t.Run("DeleteError", func(t *testing.T) {
		mock := NewMockK8sCronJobClient()
		mock.DeleteError = errors.New("delete error")

		err := mock.DeleteCronJob(ctx, "ns", "name")
		assert.Error(t, err)
		assert.Equal(t, "delete error", err.Error())
	})
}

func TestBotScheduleController_Reconcile_PropagatesNATSURLToBotContainer(t *testing.T) {
	schedule := createTestSchedule("schedule-1", "daily-report", "1.0.0", "python3.11", "0 9 * * *")

	mockRepo := &MockDesiredSchedules{
		Running: []model.BotSchedule{schedule},
	}
	mockCronClient := NewMockK8sCronJobClient()

	controller := NewBotScheduleController(
		BotScheduleControllerConfig{
			Namespace:      "the0",
			ControllerName: "test-controller",
			NATSURL:        "nats://nats:4222",
		},
		mockRepo,
		mockCronClient,
	)

	err := controller.Reconcile(context.Background())

	require.NoError(t, err)
	cronJob := mockCronClient.CronJobs["the0/schedule-schedule-1"]
	require.NotNil(t, cronJob)

	containers := cronJob.Spec.JobTemplate.Spec.Template.Spec.Containers
	require.Len(t, containers, 1)

	botEnv := make(map[string]string)
	for _, env := range containers[0].Env {
		botEnv[env.Name] = env.Value
	}
	assert.Equal(t, "nats://nats:4222", botEnv["NATS_URL"],
		"scheduled bots run sync inline and need NATS_URL for live log publishing")
}

func TestBotScheduleController_Reconcile_UpdatesCronJobWhenNATSURLAdded(t *testing.T) {
	schedule := createTestSchedule("schedule-1", "daily-report", "1.0.0", "python3.11", "0 9 * * *")
	mockCronClient := NewMockK8sCronJobClient()

	// Seed a CronJob created before NATS_URL was configured
	oldController := NewBotScheduleController(
		BotScheduleControllerConfig{Namespace: "the0", ControllerName: "test-controller"},
		&MockDesiredSchedules{Running: []model.BotSchedule{schedule}},
		mockCronClient,
	)
	require.NoError(t, oldController.Reconcile(context.Background()))
	require.Equal(t, 1, mockCronClient.CreateCalled)

	// Reconcile with NATS_URL configured: the existing CronJob must be updated
	newController := NewBotScheduleController(
		BotScheduleControllerConfig{
			Namespace:      "the0",
			ControllerName: "test-controller",
			NATSURL:        "nats://nats:4222",
		},
		&MockDesiredSchedules{Running: []model.BotSchedule{schedule}},
		mockCronClient,
	)
	require.NoError(t, newController.Reconcile(context.Background()))

	assert.Equal(t, 1, mockCronClient.UpdateCalled,
		"CronJob without NATS_URL must be updated once NATS_URL is configured")

	cronJob := mockCronClient.CronJobs["the0/schedule-schedule-1"]
	require.NotNil(t, cronJob)
	botEnv := make(map[string]string)
	for _, env := range cronJob.Spec.JobTemplate.Spec.Template.Spec.Containers[0].Env {
		botEnv[env.Name] = env.Value
	}
	assert.Equal(t, "nats://nats:4222", botEnv["NATS_URL"])
}

func TestBotScheduleController_Reconcile_UpdatesCronJobWhenRuntimeImageChanges(t *testing.T) {
	schedule := createTestSchedule("schedule-1", "daily-report", "1.0.0", "python3.11", "0 9 * * *")
	mockCronClient := NewMockK8sCronJobClient()
	controllerFor := func(image string) *BotScheduleController {
		return NewBotScheduleController(
			BotScheduleControllerConfig{Namespace: "the0", ControllerName: "test-controller", RuntimeImage: image},
			&MockDesiredSchedules{Running: []model.BotSchedule{schedule}},
			mockCronClient,
		)
	}

	// CronJob created by the previous platform release
	require.NoError(t, controllerFor("ghcr.io/the0/runtime:1.14.8").Reconcile(context.Background()))
	require.Equal(t, 1, mockCronClient.CreateCalled)

	// The upgraded controller runs a newer runtime image
	require.NoError(t, controllerFor("ghcr.io/the0/runtime:1.14.9").Reconcile(context.Background()))

	assert.Equal(t, 1, mockCronClient.UpdateCalled,
		"CronJob must follow the controller's runtime image after a platform upgrade")
	cronJob := mockCronClient.CronJobs["the0/schedule-schedule-1"]
	require.NotNil(t, cronJob)
	assert.Equal(t, "ghcr.io/the0/runtime:1.14.9", cronJob.Spec.JobTemplate.Spec.Template.Spec.Containers[0].Image)
}

func TestBotScheduleController_Reconcile_CronJobReadsStorageCredentialsFromTheSecret(t *testing.T) {
	schedule := createTestSchedule("schedule-1", "daily-report", "1.0.0", "python3.11", "0 9 * * *")
	mockCronClient := NewMockK8sCronJobClient()

	controller := NewBotScheduleController(
		BotScheduleControllerConfig{
			Namespace:      "the0",
			ControllerName: "test-controller",
			Storage: launch.Storage{
				AccessKey: "access-key",
				SecretKey: "secret-key",
			},
			StorageCredentials: testStorageCredentialsSecret("the0-the0-secrets"),
		},
		&MockDesiredSchedules{Running: []model.BotSchedule{schedule}},
		mockCronClient,
	)

	require.NoError(t, controller.Reconcile(context.Background()))

	cronJob := mockCronClient.CronJobs["the0/schedule-schedule-1"]
	require.NotNil(t, cronJob)
	bot := cronJob.Spec.JobTemplate.Spec.Template.Spec.Containers[0]
	assert.Equal(t, "the0-the0-secrets", storageSecretName(t, bot.Env, "MINIO_ACCESS_KEY"))
	assert.Equal(t, "the0-the0-secrets", storageSecretName(t, bot.Env, "MINIO_SECRET_KEY"))
}

func TestBotScheduleController_Reconcile_UpdatesCronJobWhenStorageCredentialsChange(t *testing.T) {
	literal := func(secretKey string) BotScheduleControllerConfig {
		return BotScheduleControllerConfig{
			Namespace:      "the0",
			ControllerName: "test-controller",
			Storage: launch.Storage{
				AccessKey: "access-key",
				SecretKey: secretKey,
			},
		}
	}
	fromSecret := func(name string) BotScheduleControllerConfig {
		config := literal("secret-key")
		config.StorageCredentials = testStorageCredentialsSecret(name)
		return config
	}

	cases := []struct {
		name          string
		before, after BotScheduleControllerConfig
		check         func(t *testing.T, env []corev1.EnvVar)
	}{
		{
			name:   "literal secret key rotated",
			before: literal("old-secret-key"),
			after:  literal("new-secret-key"),
			check: func(t *testing.T, env []corev1.EnvVar) {
				assert.Equal(t, "new-secret-key", envToMap(env)["MINIO_SECRET_KEY"])
			},
		},
		{
			name:   "literal credentials replaced by the secret",
			before: literal("secret-key"),
			after:  fromSecret("the0-the0-secrets"),
			check: func(t *testing.T, env []corev1.EnvVar) {
				assert.Equal(t, "the0-the0-secrets", storageSecretName(t, env, "MINIO_SECRET_KEY"))
			},
		},
		{
			name:   "secret renamed",
			before: fromSecret("the0-secrets"),
			after:  fromSecret("the0-the0-secrets"),
			check: func(t *testing.T, env []corev1.EnvVar) {
				assert.Equal(t, "the0-the0-secrets", storageSecretName(t, env, "MINIO_SECRET_KEY"))
			},
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			schedule := createTestSchedule("schedule-1", "daily-report", "1.0.0", "python3.11", "0 9 * * *")
			mockCronClient := NewMockK8sCronJobClient()
			controllerWith := func(config BotScheduleControllerConfig) *BotScheduleController {
				return NewBotScheduleController(config, &MockDesiredSchedules{Running: []model.BotSchedule{schedule}}, mockCronClient)
			}

			require.NoError(t, controllerWith(tc.before).Reconcile(context.Background()))
			require.Equal(t, 1, mockCronClient.CreateCalled)

			require.NoError(t, controllerWith(tc.after).Reconcile(context.Background()))

			assert.Equal(t, 1, mockCronClient.UpdateCalled,
				"an existing CronJob must follow the controller's storage credentials")
			cronJob := mockCronClient.CronJobs["the0/schedule-schedule-1"]
			require.NotNil(t, cronJob)
			tc.check(t, cronJob.Spec.JobTemplate.Spec.Template.Spec.Containers[0].Env)
		})
	}
}

func TestBotScheduleController_Reconcile_UpdatesCronJobWhenStorageSettingsChange(t *testing.T) {
	base := BotScheduleControllerConfig{
		Namespace:      "the0",
		ControllerName: "test-controller",
		Storage: launch.Storage{
			Endpoint:   "minio:9000",
			AccessKey:  "access-key",
			SecretKey:  "secret-key",
			CodeBucket: "custom-bots",
		},
	}
	cases := []struct {
		name   string
		change func(*BotScheduleControllerConfig)
		env    string
		want   string
	}{
		{"storage endpoint", func(c *BotScheduleControllerConfig) { c.Storage.Endpoint = "storage.example:443" }, "MINIO_ENDPOINT", "storage.example:443"},
		{"code bucket", func(c *BotScheduleControllerConfig) { c.Storage.CodeBucket = "team-code" }, "MINIO_CODE_BUCKET", "team-code"},
		{"logs bucket", func(c *BotScheduleControllerConfig) { c.Storage.LogsBucket = "team-logs" }, "MINIO_LOGS_BUCKET", "team-logs"},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			schedule := createTestSchedule("schedule-1", "daily-report", "1.0.0", "python3.11", "0 9 * * *")
			mockCronClient := NewMockK8sCronJobClient()
			controllerWith := func(config BotScheduleControllerConfig) *BotScheduleController {
				return NewBotScheduleController(config, &MockDesiredSchedules{Running: []model.BotSchedule{schedule}}, mockCronClient)
			}
			after := base
			tc.change(&after)

			require.NoError(t, controllerWith(base).Reconcile(context.Background()))
			require.Equal(t, 1, mockCronClient.CreateCalled)

			require.NoError(t, controllerWith(after).Reconcile(context.Background()))

			assert.Equal(t, 1, mockCronClient.UpdateCalled,
				"an existing CronJob must follow the controller's storage settings")
			cronJob := mockCronClient.CronJobs["the0/schedule-schedule-1"]
			require.NotNil(t, cronJob)
			assert.Equal(t, tc.want, envToMap(cronJob.Spec.JobTemplate.Spec.Template.Spec.Containers[0].Env)[tc.env])
		})
	}
}

func envToMap(env []corev1.EnvVar) map[string]string {
	values := make(map[string]string, len(env))
	for _, variable := range env {
		values[variable.Name] = variable.Value
	}
	return values
}
