package dev

import (
	"bufio"
	"context"
	"errors"
	"fmt"
	"io"
	"os/exec"
	"strings"
	"sync"
	"syscall"
	"time"
)

// RunSpec wraps a pre-built *exec.Cmd for the Runner. Kept as a struct
// (rather than taking *exec.Cmd directly) to leave room for future fields
// without changing the NewRunner signature. The env contract that used
// to live here (BOT_ID, BOT_CONFIG, CODE_MOUNT_DIR, STATE_DIR) now goes
// through docker `-e` flags in docker.go, since the CLI launches the bot
// inside the runtime image rather than directly.
type RunSpec struct {
	Cmd *exec.Cmd
}

// EventSink receives typed events as they arrive. Implementations must be
// safe for concurrent Emit calls because stdout and stderr are scanned in
// parallel.
type EventSink interface {
	Emit(Event)
}

// Runner executes a pre-built *exec.Cmd (the `docker run ...` invocation),
// streams its stdout/stderr through the event parser, and fans events out
// to every registered sink. It returns the process exit code; a non-zero
// exit is not considered a runner error.
type Runner struct {
	spec  *RunSpec
	sinks []EventSink
}

func NewRunner(spec *RunSpec, sinks ...EventSink) *Runner {
	return &Runner{spec: spec, sinks: sinks}
}

func (r *Runner) emit(ev Event) {
	for _, s := range r.sinks {
		s.Emit(ev)
	}
}

// Run starts the process and blocks until it exits or ctx is cancelled.
// On ctx cancel, the docker CLI process receives SIGINT (which docker
// forwards to the container; `--init` then forwards to the bot process
// where the Python/Node wrappers handle graceful shutdown). If the
// docker CLI hasn't exited after 3 s we fall back to SIGKILL on it;
// --rm will then clean up the container.
func (r *Runner) Run(ctx context.Context) (int, error) {
	stdout, err := r.spec.Cmd.StdoutPipe()
	if err != nil {
		return -1, fmt.Errorf("stdout pipe: %w", err)
	}
	stderr, err := r.spec.Cmd.StderrPipe()
	if err != nil {
		return -1, fmt.Errorf("stderr pipe: %w", err)
	}

	// Put the child into its own process group so we can signal the whole
	// subtree on ctx cancel. Defensive: docker CLI with --init already
	// forwards signals correctly to the container, but pgroup targeting
	// handles the shell-wrapped fallback cases (e.g. tests that run
	// `sh -c "sleep 30"` as a docker stand-in).
	if r.spec.Cmd.SysProcAttr == nil {
		r.spec.Cmd.SysProcAttr = &syscall.SysProcAttr{}
	}
	r.spec.Cmd.SysProcAttr.Setpgid = true

	if err := r.spec.Cmd.Start(); err != nil {
		return -1, fmt.Errorf("start: %w", err)
	}

	var wg sync.WaitGroup
	wg.Add(2)
	go r.scan(&wg, StreamStdout, stdout)
	go r.scan(&wg, StreamStderr, stderr)

	// Must drain pipes before calling Cmd.Wait (per os/exec docs: it is
	// incorrect to call Wait before reads from StdoutPipe/StderrPipe
	// complete — Wait may close the pipe while there's still buffered
	// data).
	waitDone := make(chan error, 1)
	go func() {
		wg.Wait()
		waitDone <- r.spec.Cmd.Wait()
	}()

	select {
	case err := <-waitDone:
		return exitCode(err), nil
	case <-ctx.Done():
		r.terminate()
		err := <-waitDone
		return exitCode(err), nil
	}
}

// scan reads the given pipe line-by-line with an unbounded buffer (so
// lines >64KB are handled) and emits one Event per line to all sinks.
func (r *Runner) scan(wg *sync.WaitGroup, stream Stream, rc io.ReadCloser) {
	defer wg.Done()
	reader := bufio.NewReader(rc)
	for {
		line, err := reader.ReadString('\n')
		if len(line) > 0 {
			line = strings.TrimRight(line, "\n")
			r.emit(ParseLine(stream, line, time.Now()))
		}
		if err != nil {
			return
		}
	}
}

// terminate sends SIGINT to the child process group (docker CLI + any
// shell-wrapped subtree), then escalates to SIGKILL after 3 s if the
// process is still alive. Targeting the pgroup handles shell wrappers
// that don't forward signals on their own.
func (r *Runner) terminate() {
	if r.spec.Cmd.Process == nil {
		return
	}
	pgid := r.spec.Cmd.Process.Pid
	_ = syscall.Kill(-pgid, syscall.SIGINT)
	time.AfterFunc(3*time.Second, func() {
		if r.spec.Cmd.ProcessState == nil {
			_ = syscall.Kill(-pgid, syscall.SIGKILL)
		}
	})
}

func exitCode(err error) int {
	if err == nil {
		return 0
	}
	var exitErr *exec.ExitError
	if errors.As(err, &exitErr) {
		return exitErr.ExitCode()
	}
	return -1
}
