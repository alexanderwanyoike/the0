package controller

import (
	"context"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	corev1 "k8s.io/api/core/v1"
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"

	"runtime/internal/bot-runner/model"
	"runtime/internal/k8s/podgen"
)

// ---- Mock Implementations ----

// MockBotRepository is a mock implementation of BotRepository for testing.
type MockBotRepository struct {
	Bots    []model.Bot
	Error   error
	Called  int
}

func (m *MockBotRepository) FindAllEnabled(ctx context.Context) ([]model.Bot, error) {
	m.Called++
	if m.Error != nil {
		return nil, m.Error
	}
	return m.Bots, nil
}

// MockK8sClient is a mock implementation of K8sClient for testing.
type MockK8sClient struct {
	Pods           []corev1.Pod
	ListError      error
	CreateError    error
	DeleteError    error
	CreatedPods    []*corev1.Pod
	DeletedPods    []string
	ListCalled     int
	CreateCalled   int
	DeleteCalled   int
}

func (m *MockK8sClient) ListBotPods(ctx context.Context, namespace string) ([]corev1.Pod, error) {
	m.ListCalled++
	if m.ListError != nil {
		return nil, m.ListError
	}
	return m.Pods, nil
}

func (m *MockK8sClient) CreatePod(ctx context.Context, pod *corev1.Pod) error {
	m.CreateCalled++
	if m.CreateError != nil {
		return m.CreateError
	}
	m.CreatedPods = append(m.CreatedPods, pod)
	return nil
}

func (m *MockK8sClient) DeletePod(ctx context.Context, namespace, name string) error {
	m.DeleteCalled++
	if m.DeleteError != nil {
		return m.DeleteError
	}
	m.DeletedPods = append(m.DeletedPods, name)
	return nil
}

func (m *MockK8sClient) GetPod(ctx context.Context, namespace, name string) (*corev1.Pod, error) {
	for i := range m.Pods {
		if m.Pods[i].Name == name && m.Pods[i].Namespace == namespace {
			return &m.Pods[i], nil
		}
	}
	return nil, nil
}

// MockImageBuilder is a mock implementation of ImageBuilder for testing.
type MockImageBuilder struct {
	ImageRef string
	Error    error
	Called   int
}

func (m *MockImageBuilder) EnsureImage(ctx context.Context, bot model.Bot) (string, error) {
	m.Called++
	if m.Error != nil {
		return "", m.Error
	}
	if m.ImageRef != "" {
		return m.ImageRef, nil
	}
	return "mock-registry/the0/bots/test:1.0.0", nil
}

// ---- Helper Functions ----

func createTestBot(id, name, version, runtime string) model.Bot {
	enabled := true
	return model.Bot{
		ID: id,
		Config: map[string]interface{}{
			"test_key": "test_value",
		},
		CustomBotVersion: model.CustomBotVersion{
			Version: version,
			Config: model.APIBotConfig{
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

func createTestPod(botID string, phase corev1.PodPhase) corev1.Pod {
	return corev1.Pod{
		ObjectMeta: metav1.ObjectMeta{
			Name:      "bot-" + botID,
			Namespace: "the0",
			Labels: map[string]string{
				podgen.LabelBotID:     botID,
				podgen.LabelManagedBy: "the0-bot-controller",
			},
		},
		Status: corev1.PodStatus{
			Phase: phase,
		},
	}
}

// ---- Tests ----

func TestBotController_Reconcile_CreatesPodForNewBot(t *testing.T) {
	mockRepo := &MockBotRepository{
		Bots: []model.Bot{
			createTestBot("bot-1", "price-alerts", "1.0.0", "python3.11"),
		},
	}
	mockK8s := &MockK8sClient{
		Pods: []corev1.Pod{}, // No existing pods
	}
	mockImageBuilder := &MockImageBuilder{
		ImageRef: "registry/the0/bots/price-alerts:1.0.0",
	}

	controller := NewBotController(
		BotControllerConfig{
			Namespace: "the0",
		},
		mockRepo,
		mockK8s,
		mockImageBuilder,
	)

	ctx := context.Background()
	err := controller.Reconcile(ctx)

	require.NoError(t, err)
	assert.Equal(t, 1, mockRepo.Called)
	assert.Equal(t, 1, mockK8s.ListCalled)
	assert.Equal(t, 1, mockK8s.CreateCalled)
	assert.Equal(t, 1, mockImageBuilder.Called)
	assert.Len(t, mockK8s.CreatedPods, 1)
	assert.Equal(t, "bot-bot-1", mockK8s.CreatedPods[0].Name)
}

func TestBotController_Reconcile_DeletesPodForRemovedBot(t *testing.T) {
	mockRepo := &MockBotRepository{
		Bots: []model.Bot{}, // No desired bots
	}
	mockK8s := &MockK8sClient{
		Pods: []corev1.Pod{
			createTestPod("orphan-bot", corev1.PodRunning),
		},
	}
	mockImageBuilder := &MockImageBuilder{}

	controller := NewBotController(
		BotControllerConfig{
			Namespace: "the0",
		},
		mockRepo,
		mockK8s,
		mockImageBuilder,
	)

	ctx := context.Background()
	err := controller.Reconcile(ctx)

	require.NoError(t, err)
	assert.Equal(t, 1, mockK8s.DeleteCalled)
	assert.Contains(t, mockK8s.DeletedPods, "bot-orphan-bot")
}

func TestBotController_Reconcile_NoActionForHealthyMatchingPod(t *testing.T) {
	bot := createTestBot("bot-1", "price-alerts", "1.0.0", "python3.11")

	// Generate the expected pod to get the correct config hash
	generator := podgen.NewPodGenerator(podgen.PodGeneratorConfig{
		Namespace:      "the0",
		ControllerName: "the0-bot-controller",
	})
	expectedPod, err := generator.GeneratePod(bot, "registry/the0/bots/price-alerts:1.0.0")
	require.NoError(t, err)

	// Create a pod with matching config hash
	existingPod := corev1.Pod{
		ObjectMeta: metav1.ObjectMeta{
			Name:      expectedPod.Name,
			Namespace: expectedPod.Namespace,
			Labels:    expectedPod.Labels,
			Annotations: map[string]string{
				podgen.AnnotationConfigHash: expectedPod.Annotations[podgen.AnnotationConfigHash],
			},
		},
		Status: corev1.PodStatus{
			Phase: corev1.PodRunning,
		},
	}

	mockRepo := &MockBotRepository{
		Bots: []model.Bot{bot},
	}
	mockK8s := &MockK8sClient{
		Pods: []corev1.Pod{existingPod},
	}
	mockImageBuilder := &MockImageBuilder{}

	controller := NewBotController(
		BotControllerConfig{
			Namespace: "the0",
		},
		mockRepo,
		mockK8s,
		mockImageBuilder,
	)

	ctx := context.Background()
	err = controller.Reconcile(ctx)

	require.NoError(t, err)
	assert.Equal(t, 0, mockK8s.CreateCalled, "should not create any pods")
	assert.Equal(t, 0, mockK8s.DeleteCalled, "should not delete any pods")
}

func TestBotController_Reconcile_RecreatesPodOnConfigChange(t *testing.T) {
	bot := createTestBot("bot-1", "price-alerts", "1.0.0", "python3.11")

	// Create existing pod with different config hash
	existingPod := corev1.Pod{
		ObjectMeta: metav1.ObjectMeta{
			Name:      "bot-bot-1",
			Namespace: "the0",
			Labels: map[string]string{
				podgen.LabelBotID:     "bot-1",
				podgen.LabelManagedBy: "the0-bot-controller",
			},
			Annotations: map[string]string{
				podgen.AnnotationConfigHash: "old-hash-12345",
			},
		},
		Status: corev1.PodStatus{
			Phase: corev1.PodRunning,
		},
	}

	mockRepo := &MockBotRepository{
		Bots: []model.Bot{bot},
	}
	mockK8s := &MockK8sClient{
		Pods: []corev1.Pod{existingPod},
	}
	mockImageBuilder := &MockImageBuilder{
		ImageRef: "registry/the0/bots/price-alerts:1.0.0",
	}

	controller := NewBotController(
		BotControllerConfig{
			Namespace: "the0",
		},
		mockRepo,
		mockK8s,
		mockImageBuilder,
	)

	ctx := context.Background()
	err := controller.Reconcile(ctx)

	require.NoError(t, err)
	assert.Equal(t, 1, mockK8s.DeleteCalled, "should delete pod due to config change")
	assert.Contains(t, mockK8s.DeletedPods, "bot-bot-1")
	// Note: pod creation happens on next reconcile cycle
}

func TestBotController_Reconcile_RecreatesFailedPod(t *testing.T) {
	bot := createTestBot("bot-1", "price-alerts", "1.0.0", "python3.11")

	// Generate pod with correct hash but failed status
	generator := podgen.NewPodGenerator(podgen.PodGeneratorConfig{
		Namespace:      "the0",
		ControllerName: "the0-bot-controller",
	})
	expectedPod, _ := generator.GeneratePod(bot, "registry/the0/bots/price-alerts:1.0.0")

	failedPod := corev1.Pod{
		ObjectMeta: metav1.ObjectMeta{
			Name:        expectedPod.Name,
			Namespace:   expectedPod.Namespace,
			Labels:      expectedPod.Labels,
			Annotations: expectedPod.Annotations,
		},
		Status: corev1.PodStatus{
			Phase: corev1.PodFailed,
		},
	}

	mockRepo := &MockBotRepository{
		Bots: []model.Bot{bot},
	}
	mockK8s := &MockK8sClient{
		Pods: []corev1.Pod{failedPod},
	}
	mockImageBuilder := &MockImageBuilder{
		ImageRef: "registry/the0/bots/price-alerts:1.0.0",
	}

	controller := NewBotController(
		BotControllerConfig{
			Namespace: "the0",
		},
		mockRepo,
		mockK8s,
		mockImageBuilder,
	)

	ctx := context.Background()
	err := controller.Reconcile(ctx)

	require.NoError(t, err)
	assert.Equal(t, 1, mockK8s.DeleteCalled, "should delete failed pod for recreation")
}

func TestBotController_Reconcile_MultipleBots(t *testing.T) {
	mockRepo := &MockBotRepository{
		Bots: []model.Bot{
			createTestBot("bot-1", "price-alerts", "1.0.0", "python3.11"),
			createTestBot("bot-2", "sma-crossover", "2.0.0", "nodejs20"),
			createTestBot("bot-3", "mean-reversion", "1.5.0", "rust-stable"),
		},
	}
	mockK8s := &MockK8sClient{
		Pods: []corev1.Pod{}, // No existing pods
	}
	mockImageBuilder := &MockImageBuilder{
		ImageRef: "registry/the0/bots/test:latest",
	}

	controller := NewBotController(
		BotControllerConfig{
			Namespace: "the0",
		},
		mockRepo,
		mockK8s,
		mockImageBuilder,
	)

	ctx := context.Background()
	err := controller.Reconcile(ctx)

	require.NoError(t, err)
	assert.Equal(t, 3, mockK8s.CreateCalled, "should create 3 pods")
	assert.Len(t, mockK8s.CreatedPods, 3)
}

func TestBotController_Reconcile_MixedState(t *testing.T) {
	bot1 := createTestBot("bot-1", "price-alerts", "1.0.0", "python3.11")
	bot2 := createTestBot("bot-2", "sma-crossover", "1.0.0", "python3.11")

	// Generate pod for bot-2 with correct hash
	generator := podgen.NewPodGenerator(podgen.PodGeneratorConfig{
		Namespace:      "the0",
		ControllerName: "the0-bot-controller",
	})
	pod2, _ := generator.GeneratePod(bot2, "registry/the0/bots/sma-crossover:1.0.0")

	mockRepo := &MockBotRepository{
		Bots: []model.Bot{bot1, bot2}, // Want bot-1 and bot-2
	}
	mockK8s := &MockK8sClient{
		Pods: []corev1.Pod{
			// bot-2 exists and is healthy
			{
				ObjectMeta: metav1.ObjectMeta{
					Name:        pod2.Name,
					Namespace:   pod2.Namespace,
					Labels:      pod2.Labels,
					Annotations: pod2.Annotations,
				},
				Status: corev1.PodStatus{Phase: corev1.PodRunning},
			},
			// bot-3 exists but is not desired
			createTestPod("bot-3", corev1.PodRunning),
		},
	}
	mockImageBuilder := &MockImageBuilder{
		ImageRef: "registry/the0/bots/test:1.0.0",
	}

	controller := NewBotController(
		BotControllerConfig{
			Namespace: "the0",
		},
		mockRepo,
		mockK8s,
		mockImageBuilder,
	)

	ctx := context.Background()
	err := controller.Reconcile(ctx)

	require.NoError(t, err)
	assert.Equal(t, 1, mockK8s.CreateCalled, "should create 1 pod for bot-1")
	assert.Equal(t, 1, mockK8s.DeleteCalled, "should delete 1 pod for bot-3")
	assert.Contains(t, mockK8s.DeletedPods, "bot-bot-3")
}

func TestBotController_Start_RunsReconciliation(t *testing.T) {
	mockRepo := &MockBotRepository{
		Bots: []model.Bot{
			createTestBot("bot-1", "test", "1.0.0", "python3.11"),
		},
	}
	mockK8s := &MockK8sClient{}
	mockImageBuilder := &MockImageBuilder{}

	controller := NewBotController(
		BotControllerConfig{
			Namespace:         "the0",
			ReconcileInterval: 50 * time.Millisecond, // Short interval for testing
		},
		mockRepo,
		mockK8s,
		mockImageBuilder,
	)

	ctx, cancel := context.WithTimeout(context.Background(), 200*time.Millisecond)
	defer cancel()

	go func() {
		_ = controller.Start(ctx)
	}()

	// Wait for at least 2 reconciliation cycles
	time.Sleep(150 * time.Millisecond)

	// Should have run initial reconciliation + at least 1 timer-based
	assert.GreaterOrEqual(t, mockRepo.Called, 2, "should have run multiple reconciliations")
}

func TestBotController_Stop(t *testing.T) {
	mockRepo := &MockBotRepository{}
	mockK8s := &MockK8sClient{}
	mockImageBuilder := &MockImageBuilder{}

	controller := NewBotController(
		BotControllerConfig{
			ReconcileInterval: 100 * time.Millisecond,
		},
		mockRepo,
		mockK8s,
		mockImageBuilder,
	)

	ctx := context.Background()
	done := make(chan struct{})

	go func() {
		_ = controller.Start(ctx)
		close(done)
	}()

	// Let it run briefly
	time.Sleep(50 * time.Millisecond)

	// Stop the controller
	controller.Stop()

	// Should stop within reasonable time
	select {
	case <-done:
		// Success
	case <-time.After(500 * time.Millisecond):
		t.Fatal("controller did not stop in time")
	}
}

func TestNewBotController_DefaultConfig(t *testing.T) {
	mockRepo := &MockBotRepository{}
	mockK8s := &MockK8sClient{}
	mockImageBuilder := &MockImageBuilder{}

	controller := NewBotController(
		BotControllerConfig{}, // Empty config
		mockRepo,
		mockK8s,
		mockImageBuilder,
	)

	assert.Equal(t, "the0", controller.config.Namespace)
	assert.Equal(t, 30*time.Second, controller.config.ReconcileInterval)
	assert.Equal(t, "the0-bot-controller", controller.config.ControllerName)
}

func TestIsPodHealthy(t *testing.T) {
	tests := []struct {
		name     string
		phase    corev1.PodPhase
		expected bool
	}{
		{"Running", corev1.PodRunning, true},
		{"Pending", corev1.PodPending, true},
		{"Succeeded", corev1.PodSucceeded, false},
		{"Failed", corev1.PodFailed, false},
		{"Unknown", corev1.PodUnknown, false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			pod := &corev1.Pod{
				Status: corev1.PodStatus{Phase: tt.phase},
			}
			result := isPodHealthy(pod)
			assert.Equal(t, tt.expected, result)
		})
	}
}

func TestNoOpImageBuilder_EnsureImage(t *testing.T) {
	t.Run("with default image ref", func(t *testing.T) {
		builder := NewNoOpImageBuilder("my-registry/test:v1")
		bot := createTestBot("bot-1", "test", "1.0.0", "python3.11")

		imageRef, err := builder.EnsureImage(context.Background(), bot)

		require.NoError(t, err)
		assert.Equal(t, "my-registry/test:v1", imageRef)
	})

	t.Run("without default image ref", func(t *testing.T) {
		builder := NewNoOpImageBuilder("")
		bot := createTestBot("bot-1", "price-alerts", "2.0.0", "python3.11")

		imageRef, err := builder.EnsureImage(context.Background(), bot)

		require.NoError(t, err)
		assert.Equal(t, "localhost:5000/the0/bots/price-alerts:2.0.0", imageRef)
	})

	t.Run("with empty bot name", func(t *testing.T) {
		builder := NewNoOpImageBuilder("")
		bot := createTestBot("bot-1", "", "1.0.0", "python3.11")

		imageRef, err := builder.EnsureImage(context.Background(), bot)

		require.NoError(t, err)
		assert.Equal(t, "localhost:5000/the0/bots/unknown:1.0.0", imageRef)
	})
}
