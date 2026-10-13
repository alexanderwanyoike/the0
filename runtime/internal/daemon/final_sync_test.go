package daemon

import (
	"context"
	"os"
	"path/filepath"
	"sync"
	"syscall"
	"testing"
	"time"

	"github.com/stretchr/testify/require"

	"runtime/internal/util"
)

// syncCall is what a Syncer saw of its context as a Sync began.
type syncCall struct {
	at          time.Time
	cancelled   bool
	deadline    time.Time
	hasDeadline bool
}

// fakeSyncer records every Sync. With hangFirst, the first Sync blocks
// until its context ends, as an upload to a stalled store does.
type fakeSyncer struct {
	finalTimeout time.Duration
	hangFirst    bool

	calls     chan syncCall
	hangStart chan struct{}
	hangOnce  sync.Once
}

func newFakeSyncer(finalTimeout time.Duration, hangFirst bool) *fakeSyncer {
	return &fakeSyncer{
		finalTimeout: finalTimeout,
		hangFirst:    hangFirst,
		calls:        make(chan syncCall, 64),
		hangStart:    make(chan struct{}),
	}
}

func (f *fakeSyncer) Sync(ctx context.Context) bool {
	deadline, hasDeadline := ctx.Deadline()
	f.calls <- syncCall{at: time.Now(), cancelled: ctx.Err() != nil, deadline: deadline, hasDeadline: hasDeadline}
	if f.hangFirst {
		hang := false
		f.hangOnce.Do(func() {
			hang = true
			close(f.hangStart)
		})
		if hang {
			<-ctx.Done()
		}
	}
	return true
}

func (f *fakeSyncer) FinalSyncTimeout() time.Duration {
	return f.finalTimeout
}

// syncersOf is a syncerFactory that hands out syncer, and closes built once
// it has.
func syncersOf(syncer Syncer, built chan struct{}) syncerFactory {
	return func(context.Context, SyncOptions, util.Logger) ([]Syncer, func(), error) {
		close(built)
		return []Syncer{syncer}, func() {}, nil
	}
}

func syncOptions(t *testing.T, interval time.Duration) SyncOptions {
	dir := t.TempDir()
	return SyncOptions{
		BotID:     "bot-1",
		StatePath: filepath.Join(dir, "state"),
		LogsPath:  filepath.Join(dir, "logs"),
		Interval:  interval,
	}
}

// requireFinalSync waits for the final sync: one whose context is live and
// ends within the syncer's FinalSyncTimeout. A sync that never gets one, or
// gets it later than within, has lost the run's last state and logs.
func requireFinalSync(t *testing.T, syncer *fakeSyncer, stoppedAt time.Time, within time.Duration) {
	t.Helper()
	timeout := time.After(within)
	for {
		select {
		case call := <-syncer.calls:
			if call.cancelled || !call.hasDeadline {
				continue
			}
			require.WithinDuration(t, call.at.Add(syncer.finalTimeout), call.deadline, time.Second,
				"the final sync's context should end after the syncer's FinalSyncTimeout")
			require.WithinDuration(t, stoppedAt, call.at, within)
			return
		case <-timeout:
			t.Fatalf("no final sync with a live context bounded by FinalSyncTimeout within %s of stopping", within)
		}
	}
}

func TestSyncRunner_StoppedMakesItsFinalSyncWithAFreshDeadline(t *testing.T) {
	syncer := newFakeSyncer(3*time.Second, false)
	built := make(chan struct{})
	runner := newSyncRunner(syncOptions(t, time.Hour), &util.NopLogger{}, syncersOf(syncer, built))

	runner.Start()
	require.NoError(t, runner.WaitReady(5*time.Second))
	stoppedAt := time.Now()
	runner.Stop()

	requireFinalSync(t, syncer, stoppedAt, 2*time.Second)
}

func TestSidecar_CancelledStillMakesItsFinalSyncWithinTheDeadline(t *testing.T) {
	syncer := newFakeSyncer(3*time.Second, false)
	built := make(chan struct{})
	ctx, cancel := context.WithCancel(context.Background())
	stopped := make(chan error, 1)
	go func() {
		stopped <- runSidecar(ctx, syncOptions(t, time.Hour), &util.NopLogger{}, syncersOf(syncer, built), noSignals)
	}()
	<-built

	stoppedAt := time.Now()
	cancel()

	requireFinalSync(t, syncer, stoppedAt, 2*time.Second)
	requireStopped(t, stopped)
}

func TestSidecar_SignalledWhileASyncHangsStillMakesItsFinalSyncWithinTheDeadline(t *testing.T) {
	syncer := newFakeSyncer(3*time.Second, true)
	built := make(chan struct{})
	signals := make(chan os.Signal, 1)
	stopped := make(chan error, 1)
	go func() {
		stopped <- runSidecar(context.Background(), syncOptions(t, 10*time.Millisecond), &util.NopLogger{},
			syncersOf(syncer, built), func() <-chan os.Signal { return signals })
	}()
	<-syncer.hangStart

	stoppedAt := time.Now()
	signals <- syscall.SIGTERM

	requireFinalSync(t, syncer, stoppedAt, 2*time.Second)
	requireStopped(t, stopped)
}

func noSignals() <-chan os.Signal {
	return make(chan os.Signal)
}

func requireStopped(t *testing.T, stopped <-chan error) {
	t.Helper()
	select {
	case err := <-stopped:
		require.NoError(t, err)
	case <-time.After(5 * time.Second):
		t.Fatal("the sidecar did not exit after its final sync")
	}
}
