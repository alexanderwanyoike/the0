package util

import (
	"fmt"
	"math"
	"time"
)

// RetryWithBackoff executes a function with exponential backoff retry logic
// maxRetries: maximum number of retry attempts (default 20)
// Returns the last error if all retries fail
func RetryWithBackoff(fn func() error, maxRetries int) error {
	if maxRetries <= 0 {
		maxRetries = 20
	}

	var lastErr error
	for attempt := 0; attempt < maxRetries; attempt++ {
		if err := fn(); err != nil {
			lastErr = err
			if attempt == maxRetries-1 {
				return fmt.Errorf("failed after %d attempts: %w", maxRetries, err)
			}

			// Exponential backoff: 1s, 2s, 4s, 8s, 16s, 32s, then cap at 60s
			backoff := time.Duration(math.Min(math.Pow(2, float64(attempt)), 60)) * time.Second
			time.Sleep(backoff)
			continue
		}
		return nil // Success
	}
	return lastErr
}

// RetryWithBackoffLogger is like RetryWithBackoff but logs retry attempts
func RetryWithBackoffLogger(fn func() error, maxRetries int, operation string) error {
	if maxRetries <= 0 {
		maxRetries = 20
	}

	var lastErr error
	for attempt := 0; attempt < maxRetries; attempt++ {
		if err := fn(); err != nil {
			lastErr = err
			LogWorker("Retry %d/%d failed for %s: %v", attempt+1, maxRetries, operation, err)

			if attempt == maxRetries-1 {
				LogWorker("All retries exhausted for %s", operation)
				return fmt.Errorf("failed after %d attempts: %w", maxRetries, err)
			}

			// Exponential backoff: 1s, 2s, 4s, 8s, 16s, 32s, then cap at 60s
			backoff := time.Duration(math.Min(math.Pow(2, float64(attempt)), 60)) * time.Second
			LogWorker("Retrying %s in %v...", operation, backoff)
			time.Sleep(backoff)
			continue
		}
		if attempt > 0 {
			LogWorker("Retry successful for %s after %d attempts", operation, attempt+1)
		}
		return nil // Success
	}
	return lastErr
}
