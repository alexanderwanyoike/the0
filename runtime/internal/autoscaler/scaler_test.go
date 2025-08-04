package autoscaler

import (
	"context"
	"testing"
	"time"

	"runtime/internal/metrics"

	"github.com/stretchr/testify/assert"
	appsv1 "k8s.io/api/apps/v1"
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"
	"k8s.io/apimachinery/pkg/runtime"
	"k8s.io/client-go/kubernetes/fake"
	ktesting "k8s.io/client-go/testing"
)

func TestScaler_CalculateDesiredReplicas(t *testing.T) {
	tests := []struct {
		name            string
		metrics         metrics.SegmentMetrics
		currentReplicas int32
		expectedDesired int32
		description     string
	}{
		{
			name: "normal operation - 10 segments",
			metrics: metrics.SegmentMetrics{
				TotalSegments:    10,
				AssignedSegments: 10,
				OrphanedSegments: 0,
				AvailableWorkers: 10,
				SegmentBacklog:   0,
			},
			currentReplicas: 10,
			expectedDesired: 10, // 10 segments = 10 workers, already at target
			description:     "Normal operation should maintain current replicas",
		},
		{
			name: "scale up - new segments added",
			metrics: metrics.SegmentMetrics{
				TotalSegments:    15,
				AssignedSegments: 10,
				OrphanedSegments: 0,
				AvailableWorkers: 10,
				SegmentBacklog:   5, // 5 new segments in backlog
			},
			currentReplicas: 11,
			expectedDesired: 12, // Target=16 (15+1), but gradual scaling: current + 1
			description:     "Should scale up gradually when new segments added",
		},
		{
			name: "scale down - segments removed",
			metrics: metrics.SegmentMetrics{
				TotalSegments:    5,
				AssignedSegments: 5,
				OrphanedSegments: 0,
				AvailableWorkers: 10,
				SegmentBacklog:   0,
			},
			currentReplicas: 11,
			expectedDesired: 10, // Gradual scaling: current - 1
			description:     "Should scale down gradually when segments removed",
		},
		{
			name: "worker died - orphaned segment",
			metrics: metrics.SegmentMetrics{
				TotalSegments:    10,
				AssignedSegments: 9,
				OrphanedSegments: 1,
				AvailableWorkers: 9,
				SegmentBacklog:   1, // Orphaned segment in backlog
			},
			currentReplicas: 10, // One pod died, Kubernetes will restart it
			expectedDesired: 10, // Target=10, already at target
			description:     "Should maintain replicas when worker dies and segment is orphaned",
		},
		{
			name: "min replicas constraint",
			metrics: metrics.SegmentMetrics{
				TotalSegments:    1,
				AssignedSegments: 1,
				OrphanedSegments: 0,
				AvailableWorkers: 1,
				SegmentBacklog:   0,
			},
			currentReplicas: 3,
			expectedDesired: 3, // Min replicas = 3
			description:     "Should respect minimum replicas constraint",
		},
		{
			name: "no segments - minimum replicas",
			metrics: metrics.SegmentMetrics{
				TotalSegments:    0,
				AssignedSegments: 0,
				OrphanedSegments: 0,
				AvailableWorkers: 3,
				SegmentBacklog:   0,
			},
			currentReplicas: 5,
			expectedDesired: 4, // Gradual scale down toward min (3)
			description:     "Should scale down to minimum when no segments",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			scaler := &Scaler{
				minReplicas: 3,
				maxReplicas: 40, // Increased to allow the test scenarios to work
			}

			result := scaler.calculateDesiredReplicas(tt.metrics, tt.currentReplicas)
			assert.Equal(t, tt.expectedDesired, result, tt.description)
		})
	}
}

func TestScaler_ScalingLimits(t *testing.T) {
	scaler := &Scaler{
		minReplicas: 3,
		maxReplicas: 4,
	}

	t.Run("max replicas constraint", func(t *testing.T) {
		metrics := metrics.SegmentMetrics{
			TotalSegments: 100, // Would normally require 101 replicas
		}
		result := scaler.calculateDesiredReplicas(metrics, 40)
		assert.Equal(t, int32(4), result, "Should not exceed max replicas")
	})

	t.Run("gradual scaling up", func(t *testing.T) {
		metrics := metrics.SegmentMetrics{
			TotalSegments: 20, // Would normally require 21 replicas
		}
		result := scaler.calculateDesiredReplicas(metrics, 10)
		assert.Equal(t, int32(4), result, "Should scale up but be constrained by max replicas")
	})

	t.Run("gradual scaling down", func(t *testing.T) {
		metrics := metrics.SegmentMetrics{
			TotalSegments: 5, // Would normally require 6 replicas
		}
		result := scaler.calculateDesiredReplicas(metrics, 20)
		assert.Equal(t, int32(4), result, "Should scale down but be constrained by max replicas")
	})
}

func TestScaler_GradualScaling(t *testing.T) {
	// Test gradual scaling without constraint interference
	scaler := &Scaler{
		minReplicas: 1,
		maxReplicas: 100, // High enough to not interfere
	}

	t.Run("gradual scaling up", func(t *testing.T) {
		metrics := metrics.SegmentMetrics{
			TotalSegments: 20, // Would normally require 21 replicas
		}
		result := scaler.calculateDesiredReplicas(metrics, 10)
		assert.Equal(t, int32(11), result, "Should scale up by only 1 replica at a time")
	})

	t.Run("gradual scaling down", func(t *testing.T) {
		metrics := metrics.SegmentMetrics{
			TotalSegments: 5, // Would normally require 6 replicas
		}
		result := scaler.calculateDesiredReplicas(metrics, 20)
		assert.Equal(t, int32(19), result, "Should scale down by only 1 replica at a time")
	})
}

func TestScaler_Integration(t *testing.T) {
	// Create fake Kubernetes client
	client := fake.NewSimpleClientset()

	// Create initial deployment
	deployment := &appsv1.Deployment{
		ObjectMeta: metav1.ObjectMeta{
			Name:      "runtime-worker",
			Namespace: "default",
		},
		Spec: appsv1.DeploymentSpec{
			Replicas: int32Ptr(5),
		},
	}
	_, err := client.AppsV1().Deployments("default").Create(
		context.Background(),
		deployment,
		metav1.CreateOptions{},
	)
	assert.NoError(t, err)

	// Create scaler without MongoDB (no segments means scale down to min)
	scaler := &Scaler{
		client:         client,
		namespace:      "default",
		deployment:     "runtime-worker",
		minReplicas:    3,
		maxReplicas:    40,
		cooldownPeriod: 1 * time.Millisecond, // Very short for testing
		mongoClient:    nil,                  // No MongoDB in test
	}

	// Test scaling (with no MongoDB, totalSegments=0, should scale down)
	err = scaler.checkAndScale()
	assert.NoError(t, err)

	// Verify deployment was updated (should scale down from 5 to 4)
	updatedDeployment, err := client.AppsV1().Deployments("default").Get(
		context.Background(),
		"runtime-worker",
		metav1.GetOptions{},
	)
	assert.NoError(t, err)
	assert.Equal(t, int32(4), *updatedDeployment.Spec.Replicas, "Should scale down by 1")
}

func TestScaler_CooldownPeriod(t *testing.T) {
	client := fake.NewSimpleClientset()

	// Create deployment
	deployment := &appsv1.Deployment{
		ObjectMeta: metav1.ObjectMeta{
			Name:      "runtime-worker",
			Namespace: "default",
		},
		Spec: appsv1.DeploymentSpec{
			Replicas: int32Ptr(5),
		},
	}
	_, err := client.AppsV1().Deployments("default").Create(
		context.Background(),
		deployment,
		metav1.CreateOptions{},
	)
	assert.NoError(t, err)

	scaler := &Scaler{
		client:         client,
		namespace:      "default",
		deployment:     "runtime-worker",
		minReplicas:    3,
		maxReplicas:    40,
		cooldownPeriod: 1 * time.Hour, // Long cooldown
		lastScaleTime:  time.Now(),    // Just scaled
		mongoClient:    nil,           // No MongoDB in test
	}

	// Should not scale due to cooldown
	err = scaler.checkAndScale()
	assert.NoError(t, err)

	// Verify deployment was NOT updated
	updatedDeployment, err := client.AppsV1().Deployments("default").Get(
		context.Background(),
		"runtime-worker",
		metav1.GetOptions{},
	)
	assert.NoError(t, err)
	assert.Equal(t, int32(5), *updatedDeployment.Spec.Replicas, "Should not scale during cooldown")
}

func TestScaler_ErrorHandling(t *testing.T) {
	client := fake.NewSimpleClientset()

	// Simulate API error
	client.PrependReactor("get", "deployments", func(action ktesting.Action) (handled bool, ret runtime.Object, err error) {
		return true, nil, assert.AnError
	})

	scaler := &Scaler{
		client:      client,
		namespace:   "default",
		deployment:  "runtime",
		mongoClient: nil, // No MongoDB in test
	}

	err := scaler.checkAndScale()
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "failed to get deployment")
}

func TestScaler_NoMetricsProvider(t *testing.T) {
	client := fake.NewSimpleClientset()

	scaler := &Scaler{
		client:      client,
		namespace:   "default",
		deployment:  "runtime",
		mongoClient: nil, // No MongoDB in test
	}

	err := scaler.checkAndScale()
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "failed to get deployment")
}

// Helper function
func int32Ptr(i int32) *int32 {
	return &i
}

// Benchmark test for scaling calculations
func BenchmarkCalculateDesiredReplicas(b *testing.B) {
	scaler := &Scaler{
		minReplicas: 3,
		maxReplicas: 40,
	}

	testMetrics := metrics.SegmentMetrics{
		TotalSegments:    15,
		AssignedSegments: 10,
		SegmentBacklog:   5,
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		scaler.calculateDesiredReplicas(testMetrics, 12)
	}
}
