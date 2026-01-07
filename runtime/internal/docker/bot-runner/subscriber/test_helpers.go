package natssubscriber

import (
	"context"
	"fmt"
	"time"

	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/mongo"
)

// WaitFor waits for a condition to become true or times out.
// condition: function that returns true when the condition is met
// timeout: maximum duration to wait
// interval: how often to check the condition
// Returns error if timeout is reached before condition becomes true.
func WaitFor(condition func() bool, timeout time.Duration, interval time.Duration) error {
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()

	ticker := time.NewTicker(interval)
	defer ticker.Stop()

	// Check immediately first
	if condition() {
		return nil
	}

	for {
		select {
		case <-ctx.Done():
			return fmt.Errorf("timeout waiting for condition")
		case <-ticker.C:
			if condition() {
				return nil
			}
		}
	}
}

// WaitForDocument waits for a MongoDB document matching the filter to appear.
func WaitForDocument(collection *mongo.Collection, filter bson.M, timeout time.Duration) error {
	return WaitFor(func() bool {
		count, err := collection.CountDocuments(context.Background(), filter)
		return err == nil && count > 0
	}, timeout, 50*time.Millisecond)
}

// WaitForDocumentCount waits for a specific count of MongoDB documents matching the filter.
func WaitForDocumentCount(collection *mongo.Collection, filter bson.M, expectedCount int64, timeout time.Duration) error {
	return WaitFor(func() bool {
		count, err := collection.CountDocuments(context.Background(), filter)
		return err == nil && count == expectedCount
	}, timeout, 50*time.Millisecond)
}
