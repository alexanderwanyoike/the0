package controller

import (
	"context"
	"fmt"

	batchv1 "k8s.io/api/batch/v1"
)

// MockK8sCronJobClient is a mock implementation for testing.
type MockK8sCronJobClient struct {
	CronJobs     map[string]*batchv1.CronJob
	ListError    error
	CreateError  error
	UpdateError  error
	DeleteError  error
	ListCalled   int
	CreateCalled int
	UpdateCalled int
	DeleteCalled int
}

// NewMockK8sCronJobClient creates a new MockK8sCronJobClient.
func NewMockK8sCronJobClient() *MockK8sCronJobClient {
	return &MockK8sCronJobClient{
		CronJobs: make(map[string]*batchv1.CronJob),
	}
}

// ListCronJobs returns mock CronJobs.
func (m *MockK8sCronJobClient) ListCronJobs(ctx context.Context, namespace string) ([]batchv1.CronJob, error) {
	m.ListCalled++
	if m.ListError != nil {
		return nil, m.ListError
	}
	result := make([]batchv1.CronJob, 0, len(m.CronJobs))
	for _, cj := range m.CronJobs {
		result = append(result, *cj)
	}
	return result, nil
}

// CreateCronJob creates a mock CronJob.
func (m *MockK8sCronJobClient) CreateCronJob(ctx context.Context, cronJob *batchv1.CronJob) error {
	m.CreateCalled++
	if m.CreateError != nil {
		return m.CreateError
	}
	key := fmt.Sprintf("%s/%s", cronJob.Namespace, cronJob.Name)
	m.CronJobs[key] = cronJob
	return nil
}

// UpdateCronJob updates a mock CronJob.
func (m *MockK8sCronJobClient) UpdateCronJob(ctx context.Context, cronJob *batchv1.CronJob) error {
	m.UpdateCalled++
	if m.UpdateError != nil {
		return m.UpdateError
	}
	key := fmt.Sprintf("%s/%s", cronJob.Namespace, cronJob.Name)
	m.CronJobs[key] = cronJob
	return nil
}

// DeleteCronJob deletes a mock CronJob.
func (m *MockK8sCronJobClient) DeleteCronJob(ctx context.Context, namespace, name string) error {
	m.DeleteCalled++
	if m.DeleteError != nil {
		return m.DeleteError
	}
	key := fmt.Sprintf("%s/%s", namespace, name)
	delete(m.CronJobs, key)
	return nil
}

// GetCronJob gets a mock CronJob.
func (m *MockK8sCronJobClient) GetCronJob(ctx context.Context, namespace, name string) (*batchv1.CronJob, error) {
	key := fmt.Sprintf("%s/%s", namespace, name)
	return m.CronJobs[key], nil
}
