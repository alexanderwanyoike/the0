package botevents

import "time"

// SetRetryBackoff shortens a consumer's retry delays so a test can fail an
// event many times quickly.
func SetRetryBackoff(c *Consumer, first, max time.Duration) {
	c.retry = backoff{first: first, max: max}
}

// SetClock fixes the time a consumer creates its durable consumer at, so a
// test can place the start window between two stored events.
func SetClock(c *Consumer, now func() time.Time) {
	c.now = now
}

const StartWindow = startWindow
