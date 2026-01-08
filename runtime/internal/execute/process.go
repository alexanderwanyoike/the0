package execute

import (
	"context"
	"io"
	"os"
	"os/exec"
	"path/filepath"

	"runtime/internal/util"
)

// ProcessExecutor handles subprocess execution.
// This abstraction enables testing without actually spawning processes.
type ProcessExecutor interface {
	// Start starts the command and returns immediately
	Start(cmd *exec.Cmd) error

	// Wait waits for the command to complete
	Wait(cmd *exec.Cmd) error

	// Kill forcefully terminates the command
	Kill(cmd *exec.Cmd) error
}

// FileWriter handles file I/O operations.
// This abstraction enables testing without actual file system operations.
type FileWriter interface {
	// Create creates or opens a file for writing
	Create(path string) (io.WriteCloser, error)
}

// Dependencies holds injectable dependencies for process execution.
type Dependencies struct {
	ProcessExecutor ProcessExecutor
	FileWriter      FileWriter
}

// NewDefaultDependencies creates production dependencies.
func NewDefaultDependencies() Dependencies {
	return Dependencies{
		ProcessExecutor: &DefaultProcessExecutor{},
		FileWriter:      &DefaultFileWriter{},
	}
}

// DefaultProcessExecutor is the production implementation of ProcessExecutor.
type DefaultProcessExecutor struct{}

// Start starts the command.
func (d *DefaultProcessExecutor) Start(cmd *exec.Cmd) error {
	return cmd.Start()
}

// Wait waits for the command to complete.
func (d *DefaultProcessExecutor) Wait(cmd *exec.Cmd) error {
	return cmd.Wait()
}

// Kill forcefully terminates the command.
func (d *DefaultProcessExecutor) Kill(cmd *exec.Cmd) error {
	if cmd.Process == nil {
		return nil
	}
	return cmd.Process.Kill()
}

// DefaultFileWriter is the production implementation of FileWriter.
type DefaultFileWriter struct{}

// Create creates or opens a file for writing.
func (d *DefaultFileWriter) Create(path string) (io.WriteCloser, error) {
	// Create directory if it doesn't exist
	dir := filepath.Dir(path)
	if dir != "" && dir != "." {
		if err := os.MkdirAll(dir, 0755); err != nil {
			return nil, err
		}
	}
	return os.OpenFile(path, os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0644)
}

// ProcessRunner executes bot and query processes with proper signal handling.
type ProcessRunner struct {
	logger util.Logger
	deps   Dependencies
}

// NewProcessRunner creates a new ProcessRunner with the given dependencies.
func NewProcessRunner(logger util.Logger, deps Dependencies) *ProcessRunner {
	if logger == nil {
		logger = &util.DefaultLogger{}
	}
	return &ProcessRunner{
		logger: logger,
		deps:   deps,
	}
}

// RunProcess executes a command with context cancellation support.
// Returns the exit code of the process.
func (r *ProcessRunner) RunProcess(ctx context.Context, cmd *exec.Cmd) int {
	r.logger.Info("Executing: %s", cmd.Path)

	if err := r.deps.ProcessExecutor.Start(cmd); err != nil {
		r.logger.Info("Failed to start process: %v", err)
		return 1
	}

	// Wait for process or context cancellation
	done := make(chan error, 1)
	go func() {
		done <- r.deps.ProcessExecutor.Wait(cmd)
	}()

	select {
	case <-ctx.Done():
		r.logger.Info("Context cancelled, terminating process")
		r.deps.ProcessExecutor.Kill(cmd)
		return 1
	case err := <-done:
		if err != nil {
			if exitErr, ok := err.(*exec.ExitError); ok {
				return exitErr.ExitCode()
			}
			r.logger.Info("Process error: %v", err)
			return 1
		}
		return 0
	}
}

// SetupLogFile creates a log file for bot output persistence.
// Returns nil if log file creation fails (bot should still run).
func (r *ProcessRunner) SetupLogFile(logsPath string) *os.File {
	logFilePath := filepath.Join(logsPath, "bot.log")
	file, err := r.deps.FileWriter.Create(logFilePath)
	if err != nil {
		r.logger.Info("Failed to create log file: %v", err)
		return nil
	}

	// Type assert to *os.File for compatibility with io.MultiWriter
	if osFile, ok := file.(*os.File); ok {
		return osFile
	}

	r.logger.Info("FileWriter.Create() did not return *os.File")
	return nil
}

// RunWithOutput executes a command with output going to stdout/stderr and optionally a log file.
func (r *ProcessRunner) RunWithOutput(ctx context.Context, cmd *exec.Cmd, logFile *os.File) int {
	// Set up output: both stdout and optionally log file
	if logFile != nil {
		cmd.Stdout = io.MultiWriter(os.Stdout, logFile)
		cmd.Stderr = io.MultiWriter(os.Stderr, logFile)
	} else {
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
	}

	return r.RunProcess(ctx, cmd)
}
