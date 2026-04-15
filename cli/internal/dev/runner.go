package dev

import (
	"bufio"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"strings"
	"sync"
	"time"
)

// RunSpec is a pre-built command + paths that the Runner will execute. Each
// per-language dispatcher is responsible for populating Cmd; RunSpec handles
// the env-var plumbing (BOT_ID, BOT_CONFIG, CODE_MOUNT_DIR, STATE_DIR) so no
// dispatcher forgets the CODE_MOUNT_DIR leading-slash trick.
type RunSpec struct {
	Cmd       *exec.Cmd
	BotID     string
	BotConfig json.RawMessage
	CodeDir   string
	StateDir  string
	ExtraEnv  []string
}

// applyEnv injects the standard the0 env contract onto spec.Cmd.Env.
//
// CODE_MOUNT_DIR is the one awkward case: the SDKs unconditionally prepend
// "/" when resolving result.json paths, so we export the host path WITHOUT
// its leading slash. The SDK's "/" + CODE_MOUNT_DIR reconstruction yields the
// correct absolute host path. This lets local dev work against unmodified
// SDKs.
//
// STATE_DIR is consumed as-is by the SDKs, so we export it verbatim.
func (s *RunSpec) applyEnv() {
	if s.Cmd.Env == nil {
		s.Cmd.Env = append([]string(nil), os.Environ()...)
	}
	set := func(k, v string) {
		prefix := k + "="
		for i, kv := range s.Cmd.Env {
			if strings.HasPrefix(kv, prefix) {
				s.Cmd.Env[i] = prefix + v
				return
			}
		}
		s.Cmd.Env = append(s.Cmd.Env, prefix+v)
	}

	if s.BotID != "" {
		set("BOT_ID", s.BotID)
	}
	if len(s.BotConfig) > 0 {
		set("BOT_CONFIG", string(s.BotConfig))
	}
	if s.CodeDir != "" {
		set("CODE_MOUNT_DIR", strings.TrimPrefix(s.CodeDir, "/"))
	}
	if s.StateDir != "" {
		set("STATE_DIR", s.StateDir)
	}
	s.Cmd.Env = append(s.Cmd.Env, s.ExtraEnv...)
}

// EventSink receives typed events as they arrive. Implementations must be
// safe for concurrent Emit calls because stdout and stderr are scanned in
// parallel.
type EventSink interface {
	Emit(Event)
}

// Runner executes a pre-built *exec.Cmd, streams its stdout/stderr through
// the event parser, and fans events out to every registered sink. It returns
// the process exit code; a non-zero exit is not considered a runner error.
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
// On ctx cancel, the process receives SIGINT, then SIGTERM after 3s, then
// SIGKILL as a last resort.
func (r *Runner) Run(ctx context.Context) (int, error) {
	r.spec.applyEnv()

	stdout, err := r.spec.Cmd.StdoutPipe()
	if err != nil {
		return -1, fmt.Errorf("stdout pipe: %w", err)
	}
	stderr, err := r.spec.Cmd.StderrPipe()
	if err != nil {
		return -1, fmt.Errorf("stderr pipe: %w", err)
	}

	if err := r.spec.Cmd.Start(); err != nil {
		return -1, fmt.Errorf("start: %w", err)
	}

	var wg sync.WaitGroup
	wg.Add(2)
	go r.scan(&wg, StreamStdout, stdout)
	go r.scan(&wg, StreamStderr, stderr)

	waitErr := make(chan error, 1)
	go func() { waitErr <- r.spec.Cmd.Wait() }()

	select {
	case err := <-waitErr:
		wg.Wait()
		return exitCode(err), nil
	case <-ctx.Done():
		r.terminate()
		<-waitErr
		wg.Wait()
		return exitCode(nil), nil
	}
}

// scan reads the given pipe line-by-line with an unbounded buffer (so lines
// >64KB are handled) and emits one Event per line to all sinks.
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

// terminate is the SIGINT -> SIGTERM -> SIGKILL escalation used when the
// parent context is cancelled.
func (r *Runner) terminate() {
	if r.spec.Cmd.Process == nil {
		return
	}
	_ = r.spec.Cmd.Process.Signal(os.Interrupt)
	time.AfterFunc(3*time.Second, func() {
		if r.spec.Cmd.ProcessState == nil {
			_ = r.spec.Cmd.Process.Kill()
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
