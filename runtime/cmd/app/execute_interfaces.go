package main

import (
	"io"
	"os"
	"os/exec"
)

// ProcessExecutor handles subprocess execution
// This is the core abstraction that enables testing execute.go
type ProcessExecutor interface {
	// Start starts the command and returns immediately
	Start(cmd *exec.Cmd) error

	// Wait waits for the command to complete
	Wait(cmd *exec.Cmd) error

	// Kill forcefully terminates the command
	Kill(cmd *exec.Cmd) error
}

// FileWriter handles file I/O operations
// This abstracts file operations to enable testing
type FileWriter interface {
	// Create creates or opens a file for writing
	Create(path string) (io.WriteCloser, error)

	// Write writes data to the writer
	Write(w io.Writer, data []byte) (int, error)
}

// defaultProcessExecutor is the production implementation
type defaultProcessExecutor struct{}

func (d *defaultProcessExecutor) Start(cmd *exec.Cmd) error {
	return cmd.Start()
}

func (d *defaultProcessExecutor) Wait(cmd *exec.Cmd) error {
	return cmd.Wait()
}

func (d *defaultProcessExecutor) Kill(cmd *exec.Cmd) error {
	if cmd.Process == nil {
		return nil
	}
	return cmd.Process.Kill()
}

// defaultFileWriter is the production implementation
type defaultFileWriter struct{}

func (d *defaultFileWriter) Create(path string) (io.WriteCloser, error) {
	// Create directory if it doesn't exist
	dir := path[:len(path)-len("/"+path[len(path)-1:])]
	if dir != "" {
		if err := os.MkdirAll(dir, 0755); err != nil {
			return nil, err
		}
	}
	return os.OpenFile(path, os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0644)
}

func (d *defaultFileWriter) Write(w io.Writer, data []byte) (int, error) {
	return w.Write(data)
}
