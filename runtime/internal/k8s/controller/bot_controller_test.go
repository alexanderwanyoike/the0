package controller

import (
	"context"
	"errors"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	corev1 "k8s.io/api/core/v1"
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"

	"runtime/internal/model"
	"runtime/internal/k8s/podgen"
)

// ---- Mock Implementations ----

// MockBotRepository is a mock implementation of BotRepository for testing.
type MockBotRepository struct {
	Bots   []model.Bot
	Error  error
	Called int
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
	Pods         []corev1.Pod
	ListError    error
	CreateError  error
	DeleteError  error
	CreatedPods  []*corev1.Pod
	DeletedPods  []string
	ListCalled   int
	CreateCalled int
	DeleteCalled int
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

// ---- Helper Functions ----

func createTestBot(id, name, version, runtime string) model.Bot {
	enabled := true
	return model.Bot{
		ID: id,
		Config: map[string]interface{}{
			"test_key": "test_value",
		},
		CustomBotVersion: model.CustomBotVersion{
			Version:  version,
			FilePath: name + "/" + version,
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

func testControllerConfig() BotControllerConfig {
	return BotControllerConfig{
		Namespace:      "the0",
		MinIOEndpoint:  "minio:9000",
		MinIOAccessKey: "access-key",
		MinIOSecretKey: "secret-key",
		MinIOBucket:    "custom-bots",
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

	controller := NewBotController(
		testControllerConfig(),
		mockRepo,
		mockK8s,
	)

	ctx := context.Background()
	err := controller.Reconcile(ctx)

	require.NoError(t, err)
	assert.Equal(t, 1, mockRepo.Called)
	assert.Equal(t, 1, mockK8s.ListCalled)
	assert.Equal(t, 1, mockK8s.CreateCalled)
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

	controller := NewBotController(
		testControllerConfig(),
		mockRepo,
		mockK8s,
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
		MinIOEndpoint:  "minio:9000",
		MinIOAccessKey: "access-key",
		MinIOSecretKey: "secret-key",
		MinIOBucket:    "custom-bots",
	})
	expectedPod, err := generator.GeneratePod(bot)
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

	controller := NewBotController(
		testControllerConfig(),
		mockRepo,
		mockK8s,
	)

	ctx := context.Background()
	err = controller.Reconcile(ctx)

	require.NoError(t, err)
	assert.Equal(t, 0, mockK8s.CreateCalled)
	assert.Equal(t, 0, mockK8s.DeleteCalled)
}

func TestBotController_Reconcile_RecreatesPodOnConfigChange(t *testing.T) {
	bot := createTestBot("bot-1", "price-alerts", "1.0.0", "python3.11")

	// Create a pod with different config hash (simulating config change)
	existingPod := corev1.Pod{
		ObjectMeta: metav1.ObjectMeta{
			Name:      "bot-bot-1",
			Namespace: "the0",
			Labels: map[string]string{
				podgen.LabelBotID:     "bot-1",
				podgen.LabelManagedBy: "the0-bot-controller",
			},
			Annotations: map[string]string{
				podgen.AnnotationConfigHash: "old-hash-different",
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

	controller := NewBotController(
		testControllerConfig(),
		mockRepo,
		mockK8s,
	)

	ctx := context.Background()
	err := controller.Reconcile(ctx)

	require.NoError(t, err)
	// Config changed, should delete old pod (will recreate next cycle)
	assert.Equal(t, 1, mockK8s.DeleteCalled)
	assert.Contains(t, mockK8s.DeletedPods, "bot-bot-1")
}

func TestBotController_Reconcile_RecreatesFailedPod(t *testing.T) {
	bot := createTestBot("bot-1", "price-alerts", "1.0.0", "python3.11")

	// Generate pod with correct hash
	generator := podgen.NewPodGenerator(podgen.PodGeneratorConfig{
		Namespace:      "the0",
		ControllerName: "the0-bot-controller",
		MinIOEndpoint:  "minio:9000",
		MinIOAccessKey: "access-key",
		MinIOSecretKey: "secret-key",
		MinIOBucket:    "custom-bots",
	})
	expectedPod, _ := generator.GeneratePod(bot)

	// Create a failed pod
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
			Phase: corev1.PodFailed, // Pod has failed
		},
	}

	mockRepo := &MockBotRepository{
		Bots: []model.Bot{bot},
	}
	mockK8s := &MockK8sClient{
		Pods: []corev1.Pod{existingPod},
	}

	controller := NewBotController(
		testControllerConfig(),
		mockRepo,
		mockK8s,
	)

	ctx := context.Background()
	err := controller.Reconcile(ctx)

	require.NoError(t, err)
	// Failed pod should be deleted for recreation
	assert.Equal(t, 1, mockK8s.DeleteCalled)
}

func TestBotController_Reconcile_RepositoryError(t *testing.T) {
	mockRepo := &MockBotRepository{
		Error: errors.New("database connection lost"),
	}
	mockK8s := &MockK8sClient{}

	controller := NewBotController(
		testControllerConfig(),
		mockRepo,
		mockK8s,
	)

	ctx := context.Background()
	err := controller.Reconcile(ctx)

	require.Error(t, err)
	assert.Contains(t, err.Error(), "database connection lost")
	assert.Equal(t, 0, mockK8s.ListCalled, "should not query K8s on repo error")
}

func TestBotController_Reconcile_K8sListError(t *testing.T) {
	mockRepo := &MockBotRepository{
		Bots: []model.Bot{
			createTestBot("bot-1", "price-alerts", "1.0.0", "python3.11"),
		},
	}
	mockK8s := &MockK8sClient{
		ListError: errors.New("k8s API error"),
	}

	controller := NewBotController(
		testControllerConfig(),
		mockRepo,
		mockK8s,
	)

	ctx := context.Background()
	err := controller.Reconcile(ctx)

	require.Error(t, err)
	assert.Contains(t, err.Error(), "k8s API error")
}

func TestBotController_Start_Stop(t *testing.T) {
	mockRepo := &MockBotRepository{
		Bots: []model.Bot{},
	}
	mockK8s := &MockK8sClient{}

	config := testControllerConfig()
	config.ReconcileInterval = 50 * time.Millisecond

	controller := NewBotController(
		config,
		mockRepo,
		mockK8s,
	)

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Start in goroutine
	go controller.Start(ctx)

	// Wait for at least one reconciliation
	time.Sleep(100 * time.Millisecond)

	// Stop
	controller.Stop()

	// Verify at least one reconciliation happened
	assert.GreaterOrEqual(t, mockRepo.Called, 1)
}

