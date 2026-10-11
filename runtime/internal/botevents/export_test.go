package botevents

import "time"

// SetRetryBackoff shortens a consumer's retry delays so a test can fail an
// event many times quickly.
func SetRetryBackoff(c *Consumer, first, max time.Duration) {
	c.retry = backoff{first: first, max: max}
}
