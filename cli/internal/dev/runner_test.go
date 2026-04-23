package dev

import (
	"context"
	"os/exec"
	"strings"
	"sync"
	"testing"
	"time"
)

type captureSink struct {
	mu     sync.Mutex
	events []Event
}

func (c *captureSink) Emit(ev Event) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.events = append(c.events, ev)
}

func (c *captureSink) snapshot() []Event {
	c.mu.Lock()
	defer c.mu.Unlock()
	out := make([]Event, len(c.events))
	copy(out, c.events)
	return out
}

func TestRunner_CapturesStdoutAndStderr(t *testing.T) {
	cmd := exec.Command("sh", "-c", `
		echo '{"_metric":"price","value":42}'
		echo '{"level":"INFO","message":"hi"}' >&2
		echo 'plain output'
	`)
	sink := &captureSink{}
	r := NewRunner(&RunSpec{Cmd: cmd}, sink)

	exitCode, err := r.Run(context.Background())
	if err != nil {
		t.Fatalf("Run error = %v", err)
	}
	if exitCode != 0 {
		t.Errorf("exit = %d, want 0", exitCode)
	}

	events := sink.snapshot()
	if len(events) != 3 {
		t.Fatalf("expected 3 events, got %d: %+v", len(events), events)
	}

	var sawMetric, sawLog, sawPrint bool
	for _, ev := range events {
		switch ev.Kind {
		case EventMetric:
			sawMetric = true
			if ev.Stream != StreamStdout {
				t.Errorf("metric Stream = %q, want stdout", ev.Stream)
			}
		case EventLog:
			sawLog = true
			if ev.Stream != StreamStderr {
				t.Errorf("log Stream = %q, want stderr", ev.Stream)
			}
		case EventPrint:
			sawPrint = true
		}
	}
	if !sawMetric || !sawLog || !sawPrint {
		t.Errorf("missing events: metric=%v log=%v print=%v", sawMetric, sawLog, sawPrint)
	}
}

func TestRunner_NonZeroExitCodePreserved(t *testing.T) {
	cmd := exec.Command("sh", "-c", "exit 7")
	r := NewRunner(&RunSpec{Cmd: cmd}, &captureSink{})

	exitCode, err := r.Run(context.Background())
	if err != nil {
		t.Fatalf("Run returned error for non-zero exit (should succeed): %v", err)
	}
	if exitCode != 7 {
		t.Errorf("exit = %d, want 7", exitCode)
	}
}

func TestRunner_ContextCancelStopsProcess(t *testing.T) {
	cmd := exec.Command("sh", "-c", "sleep 30")
	r := NewRunner(&RunSpec{Cmd: cmd}, &captureSink{})

	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan struct{})
	go func() {
		_, _ = r.Run(ctx)
		close(done)
	}()

	time.Sleep(50 * time.Millisecond) // let the cmd actually start
	cancel()

	select {
	case <-done:
		// ok
	case <-time.After(5 * time.Second):
		t.Fatal("Run did not return after context cancel")
	}
}

func TestRunner_EmitsEventsInOrderPerStream(t *testing.T) {
	cmd := exec.Command("sh", "-c", `
		echo 'first'
		echo 'second'
		echo 'third'
	`)
	sink := &captureSink{}
	r := NewRunner(&RunSpec{Cmd: cmd}, sink)

	if _, err := r.Run(context.Background()); err != nil {
		t.Fatalf("Run: %v", err)
	}

	events := sink.snapshot()
	var stdoutLines []string
	for _, ev := range events {
		if ev.Stream == StreamStdout {
			stdoutLines = append(stdoutLines, ev.Raw)
		}
	}
	if got := strings.Join(stdoutLines, ","); got != "first,second,third" {
		t.Errorf("order = %q, want first,second,third", got)
	}
}

func TestRunner_LongLineHandled(t *testing.T) {
	// 100KB line — well beyond the default bufio.Scanner 64KB cap. The runner
	// must use a buffered reader that can grow.
	long := strings.Repeat("x", 100_000)
	cmd := exec.Command("sh", "-c", "printf '%s\\n' \""+long+"\"")
	sink := &captureSink{}
	r := NewRunner(&RunSpec{Cmd: cmd}, sink)

	if _, err := r.Run(context.Background()); err != nil {
		t.Fatalf("Run: %v", err)
	}

	events := sink.snapshot()
	if len(events) != 1 {
		t.Fatalf("expected 1 event, got %d", len(events))
	}
	if len(events[0].Raw) != len(long) {
		t.Errorf("Raw length = %d, want %d", len(events[0].Raw), len(long))
	}
}

func TestRunner_EmptyOutputIsFine(t *testing.T) {
	cmd := exec.Command("true")
	sink := &captureSink{}
	r := NewRunner(&RunSpec{Cmd: cmd}, sink)

	exitCode, err := r.Run(context.Background())
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if exitCode != 0 {
		t.Errorf("exit = %d, want 0", exitCode)
	}
	if len(sink.snapshot()) != 0 {
		t.Errorf("unexpected events: %+v", sink.snapshot())
	}
}

