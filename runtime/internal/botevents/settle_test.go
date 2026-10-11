package botevents

import (
	"errors"
	"fmt"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
)

func TestSettle_HandledEventIsAcked(t *testing.T) {
	v, _ := settle(nil, 1)
	assert.Equal(t, verdictAck, v)

	v, _ = settle(nil, maxDeliver)
	assert.Equal(t, verdictAck, v, "a last attempt that succeeds is acked too")
}

func TestSettle_FailedEventIsRetriedWithGrowingDelays(t *testing.T) {
	failure := errors.New("mongo unavailable")
	want := map[uint64]time.Duration{
		1:              time.Second,
		2:              2 * time.Second,
		3:              4 * time.Second,
		6:              32 * time.Second,
		7:              time.Minute,
		maxDeliver - 1: time.Minute,
	}
	for delivered, delay := range want {
		v, got := settle(failure, delivered)
		assert.Equal(t, verdictRetry, v, "delivery %d", delivered)
		assert.Equal(t, delay, got, "delivery %d", delivered)
	}
}

func TestSettle_FailedEventIsDroppedAfterTheLastDelivery(t *testing.T) {
	v, _ := settle(errors.New("mongo unavailable"), maxDeliver)
	assert.Equal(t, verdictDrop, v)
}

func TestSettle_EventThatCanNeverSucceedIsDroppedAtOnce(t *testing.T) {
	malformed := Permanent(errors.New("invalid JSON"))
	v, _ := settle(fmt.Errorf("handling the0.bot.created: %w", malformed), 1)
	assert.Equal(t, verdictDrop, v)
}

func TestPermanent_KeepsTheCause(t *testing.T) {
	cause := errors.New("invalid JSON")
	err := Permanent(cause)
	assert.ErrorIs(t, err, cause)
	assert.Equal(t, cause.Error(), err.Error())
}
