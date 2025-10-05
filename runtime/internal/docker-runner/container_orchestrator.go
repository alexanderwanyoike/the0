/**
 * Container Orchestrator
 *
 * Encapsulates all the orchestration logic for running containers
 */
package dockerrunner

import (
	"archive/tar"
	"bytes"
	"context"
	"fmt"
	"io"
	"runtime/internal/util"
	"time"

	"github.com/docker/docker/api/types/container"
	"github.com/docker/docker/api/types/filters"
	"github.com/docker/docker/api/types/image"
	"github.com/docker/docker/client"
	"github.com/docker/docker/pkg/stdcopy"
)

type ContainerRunResult struct {
	ExitCode           int
	Logs               string
	ResultFileContents []byte
}

// ContainerStatus represents the current status of a container
type ContainerStatus struct {
	ContainerID string            `json:"container_id"`
	Status      string            `json:"status"` // "running", "exited", "paused", "restarting", "dead", "not_found"
	ExitCode    int               `json:"exit_code,omitempty"`
	StartedAt   string            `json:"started_at,omitempty"`
	FinishedAt  string            `json:"finished_at,omitempty"`
	Error       string            `json:"error,omitempty"`
	Labels      map[string]string `json:"labels,omitempty"`
}

type ContainerOrchestrator interface {
	// PullImage ensures the Docker image is available locally
	PullImage(ctx context.Context, image string) error
	// CreateAndStart creates and starts a container with the given configuration returns the container ID
	CreateAndStart(
		ctx context.Context,
		config *container.Config,
		hostConfig *container.HostConfig,
	) (string, error)
	// RunAndWait runs a container and waits for it to finish, returning the result
	RunAndWait(
		ctx context.Context,
		config *container.Config,
		hostConfig *container.HostConfig,
		resultFilePath string,
	) (*ContainerRunResult, error)
	// Stop Gracefully stops a running container
	Stop(ctx context.Context, containerID string, timeout time.Duration) error
	// GetLogs retrieves the logs of a specific container
	GetLogs(ctx context.Context, containerID string, tail int) (string, error)
	// CopyFromContainer copies files from the container
	CopyFromContainer(ctx context.Context, containerID, srcPath string) ([]byte, error)
	GetStatus(ctx context.Context, containerID string) (*ContainerStatus, error)
	ListContainer(ctx context.Context, filter filters.Args) ([]*ContainerInfo, error)
}

type dockerOrchestrator struct {
	dockerClient *client.Client
	logger       util.Logger
}

func NewDockerOrchestrator(dockerClient *client.Client, logger util.Logger) ContainerOrchestrator {
	return &dockerOrchestrator{
		dockerClient: dockerClient,
		logger:       logger,
	}
}

func (r *dockerOrchestrator) PullImage(ctx context.Context, imageName string) error {
	r.logger.Info("Pulling Docker image", "image", imageName)

	reader, err := r.dockerClient.ImagePull(ctx, imageName, image.PullOptions{})
	if err != nil {
		return fmt.Errorf("failed to pull image: %v", err)
	}
	defer reader.Close()

	// Read the response to ensure the pull completes
	_, err = io.ReadAll(reader)
	if err != nil {
		return fmt.Errorf("failed to read pull response: %v", err)
	}

	r.logger.Info("Successfully pulled Docker image", "image", imageName)
	return nil
}

func (r *dockerOrchestrator) CreateAndStart(
	ctx context.Context,
	config *container.Config,
	hostConfig *container.HostConfig,
) (string, error) {
	resp, err := r.dockerClient.ContainerCreate(ctx, config, hostConfig, nil, nil, "")
	if err != nil {
		return "", fmt.Errorf("failed to create container: %v", err)
	}

	if err := r.dockerClient.ContainerStart(ctx, resp.ID, container.StartOptions{}); err != nil {
		return "", fmt.Errorf("failed to start container: %v", err)
	}

	return resp.ID, nil
}

func (r *dockerOrchestrator) RunAndWait(
	ctx context.Context,
	config *container.Config,
	hostConfig *container.HostConfig,
	resultFilePath string,
) (*ContainerRunResult, error) {
	resp, err := r.dockerClient.ContainerCreate(ctx, config, hostConfig, nil, nil, "")
	if err != nil {
		return nil, fmt.Errorf("failed to create container: %v", err)
	}

	defer func() {
		// Ensure container is removed in case of early return
		if err := r.dockerClient.ContainerRemove(context.Background(), resp.ID, container.RemoveOptions{
			Force: true,
		}); err != nil {
			r.logger.Info("Warning: Failed to remove container", "container_id", resp.ID, "error", err.Error())
		} else {
			r.logger.Info("Container removed successfully", "container_id", resp.ID)
		}
	}()

	// Start container
	if err := r.dockerClient.ContainerStart(ctx, resp.ID, container.StartOptions{}); err != nil {
		return nil, fmt.Errorf("failed to start container: %v", err)
	}

	// Wait for container to complete
	statusCh, errCh := r.dockerClient.ContainerWait(ctx, resp.ID, container.WaitConditionNotRunning)

	select {
	case err := <-errCh:
		return nil, fmt.Errorf("error while waiting for container: %v", err)
	case status := <-statusCh:
		r.logger.Info("Container completed", "container_id", resp.ID, "exit_code", status.StatusCode)

		// Get logs
		logs, err := r.GetLogs(ctx, resp.ID, 1000)
		if err != nil {
			logs = fmt.Sprintf("Failed to get logs: %v", err)
		}

		// Try to copy result file before cleanup
		var resultFileContents []byte
		if resultFilePath != "" {
			resultFileContents, err = r.CopyFromContainer(ctx, resp.ID, resultFilePath)
			if err != nil {
				r.logger.Info("Warning: Failed to store analysis", "error", err.Error())
			}
		}

		return &ContainerRunResult{
			ExitCode:           int(status.StatusCode),
			Logs:               logs,
			ResultFileContents: resultFileContents,
		}, nil
	case <-ctx.Done():
		return nil, ctx.Err()
	}
}

func (r *dockerOrchestrator) Stop(ctx context.Context, containerID string, timeout time.Duration) error {
	timeoutSeconds := int(timeout.Seconds())
	if err := r.dockerClient.ContainerStop(ctx, containerID, container.StopOptions{Timeout: &timeoutSeconds}); err != nil {
		// This can happen if the container is already stopped or doesnt exist.
		if client.IsErrNotFound(err) {
			r.logger.Info("Container not found when attempting to stop", "container_id", containerID)
			return nil
		}
		return fmt.Errorf("failed to stop container: %s, error: %v", containerID, err)
	}

	statusCh, errCh := r.dockerClient.ContainerWait(ctx, containerID, container.WaitConditionNotRunning)

	select {
	case err := <-errCh:
		return fmt.Errorf("error while waiting for container %s to stop: %v", containerID, err)
	case status := <-statusCh:
		r.logger.Info("Container stopped", "container_id", containerID, "exit_code", status.StatusCode)
	case <-ctx.Done():
		return fmt.Errorf("context timed out while waiting for container %s to stop: %v", containerID, ctx.Err())
	}
	return nil
}

func (r *dockerOrchestrator) GetLogs(ctx context.Context, containerID string, tail int) (string, error) {
	logOptions := container.LogsOptions{
		ShowStdout: true,
		ShowStderr: true,
		Tail:       fmt.Sprintf("%d", tail),
		Follow:     false, // Don't follow, just get current logs
		Timestamps: false, // Don't include timestamps for cleaner output
	}

	// 1. Request the log stream from the Docker daemon
	logs, err := r.dockerClient.ContainerLogs(ctx, containerID, logOptions)
	if err != nil {
		return "", fmt.Errorf("failed to get logs for container %s: %v", containerID, err)
	}
	defer logs.Close()

	// 2. Demux the stream
	// Docker multiplexes stdout and stderr into a single stream with a special header.
	// We use stdcopy.StdCopy to demux it into separate buffers.
	var stdout, stderr bytes.Buffer
	_, err = stdcopy.StdCopy(&stdout, &stderr, logs)
	if err != nil {
		return "", fmt.Errorf("failed to read logs for container %s: %v", containerID, err)
	}

	// 3. Combine stdout and stderr for return
	return stdout.String() + stderr.String(), nil
}

func (r *dockerOrchestrator) CopyFromContainer(ctx context.Context, containerID, srcPath string) ([]byte, error) {
	reader, _, err := r.dockerClient.CopyFromContainer(ctx, containerID, srcPath)
	if err != nil {
		return nil, fmt.Errorf("failed to copy from container: %v", err)
	}
	defer reader.Close()

	// Read the tar archive
	tarReader := tar.NewReader(reader)

	// We expect a single file, so read the first entry
	_, err = tarReader.Next()
	if err != nil {
		return nil, fmt.Errorf("failed to read tar entry: %v", err)
	}

	// Read the file contents
	fileContents, err := io.ReadAll(tarReader)
	if err != nil {
		return nil, fmt.Errorf("failed to read file contents: %v", err)
	}

	return fileContents, nil
}

func (r *dockerOrchestrator) GetStatus(ctx context.Context, containerID string) (*ContainerStatus, error) {
	inspect, err := r.dockerClient.ContainerInspect(ctx, containerID)
	if err != nil {
		if client.IsErrNotFound(err) {
			return &ContainerStatus{
				ContainerID: containerID,
				Status:      "not_found",
				Error:       err.Error(),
			}, nil

		}
		return nil, fmt.Errorf("failed to inspect container %s: %v", containerID, err)
	}

	status := &ContainerStatus{
		ContainerID: containerID,
		StartedAt:   inspect.State.StartedAt,
		FinishedAt:  inspect.State.FinishedAt,
		Labels:      inspect.Config.Labels,
	}

	if inspect.State.Running {
		status.Status = "running"
	} else if inspect.State.Paused {
		status.Status = "paused"
	} else if inspect.State.Restarting {
		status.Status = "restarting"
	} else if inspect.State.Dead {
		status.Status = "dead"
	} else {
		status.Status = "exited"
		status.ExitCode = inspect.State.ExitCode
	}

	r.logger.Info("Container status retrieved", "container_id", containerID, "status", status.Status)

	return status, nil
}

func (r *dockerOrchestrator) ListContainer(ctx context.Context, filter filters.Args) ([]*ContainerInfo, error) {
	containers, err := r.dockerClient.ContainerList(ctx, container.ListOptions{
		Filters: filter,
	})
	if err != nil {
		return nil, fmt.Errorf("failed to list containers: %v", err)
	}

	result := make([]*ContainerInfo, 0, len(containers))
	for _, cont := range containers {
		if cont.Labels["runtime.id"] == "" {
			continue // Skip containers without the expected label
		}
		result = append(result, &ContainerInfo{
			ContainerID: cont.ID,
			ID:          cont.Labels["runtime.id"],
			Entrypoint:  cont.Labels["runtime.entrypoint"],
			Status:      cont.State,
		})
	}
	return result, nil
}
