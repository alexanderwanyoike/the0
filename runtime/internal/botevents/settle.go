package botevents

import (
	"errors"
	"time"
)

type verdict int

const (
	verdictAck verdict = iota
	verdictRetry
	verdictDrop
)

type backoff struct {
	first, max time.Duration
}

var defaultBackoff = backoff{first: time.Second, max: time.Minute}

func (b backoff) delay(delivered uint64) time.Duration {
	delay := b.first
	for i := uint64(1); i < delivered && delay < b.max; i++ {
		delay *= 2
	}
	return min(delay, b.max)
}

type permanentError struct{ err error }

func (e permanentError) Error() string { return e.err.Error() }
func (e permanentError) Unwrap() error { return e.err }

// Permanent marks a handler error that no redelivery can fix, such as a
// malformed payload, so the event is dropped instead of retried.
func Permanent(err error) error {
	return permanentError{err: err}
}

func settle(err error, delivered uint64, retry backoff) (verdict, time.Duration) {
	if err == nil {
		return verdictAck, 0
	}
	var permanent permanentError
	if errors.As(err, &permanent) {
		return verdictDrop, 0
	}
	return verdictRetry, retry.delay(delivered)
}
