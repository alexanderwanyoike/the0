package botevents

import (
	"errors"
	"fmt"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
)

func TestSettle_HandledEventIsAcked(t *testing.T) {
	v, _ := settle(nil, 1, defaultBackoff)
	assert.Equal(t, verdictAck, v)

	v, _ = settle(nil, 25, defaultBackoff)
	assert.Equal(t, verdictAck, v, "an event that succeeds after many failures is acked too")
}

func TestSettle_FailedEventIsRetriedWithGrowingDelays(t *testing.T) {
	failure := errors.New("mongo unavailable")
	want := map[uint64]time.Duration{
		1:  time.Second,
		2:  2 * time.Second,
		3:  4 * time.Second,
		6:  32 * time.Second,
		7:  time.Minute,
		19: time.Minute,
	}
	for delivered, delay := range want {
		v, got := settle(failure, delivered, defaultBackoff)
		assert.Equal(t, verdictRetry, v, "delivery %d", delivered)
		assert.Equal(t, delay, got, "delivery %d", delivered)
	}
}

func TestSettle_FailedEventIsRetriedHoweverOftenItFails(t *testing.T) {
	for _, delivered := range []uint64{20, 21, 1000} {
		v, delay := settle(errors.New("mongo unavailable"), delivered, defaultBackoff)
		assert.Equal(t, verdictRetry, v, "delivery %d: only a permanent failure may drop an event", delivered)
		assert.Equal(t, time.Minute, delay, "delivery %d", delivered)
	}
}

func TestSettle_EventThatCanNeverSucceedIsDroppedAtOnce(t *testing.T) {
	malformed := Permanent(errors.New("invalid JSON"))
	v, _ := settle(fmt.Errorf("handling the0.bot.created: %w", malformed), 1, defaultBackoff)
	assert.Equal(t, verdictDrop, v)
}

func TestPermanent_KeepsTheCause(t *testing.T) {
	cause := errors.New("invalid JSON")
	err := Permanent(cause)
	assert.ErrorIs(t, err, cause)
	assert.Equal(t, cause.Error(), err.Error())
}
