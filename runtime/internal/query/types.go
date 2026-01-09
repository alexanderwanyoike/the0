// Package query provides shared types and utilities for bot query execution.
package query

import (
	"encoding/json"
	"time"
)

// Request represents a query to execute against a bot.
type Request struct {
	BotID      string                 `json:"bot_id"`
	QueryPath  string                 `json:"query_path"`
	Params     map[string]interface{} `json:"params,omitempty"`
	TimeoutSec int                    `json:"timeout_sec,omitempty"` // Default: 30 seconds
}

// Response represents the result of a query execution.
type Response struct {
	Status    string          `json:"status"`              // "ok" or "error"
	Data      json.RawMessage `json:"data,omitempty"`      // Query result data
	Error     string          `json:"error,omitempty"`     // Error message if status is "error"
	Duration  time.Duration   `json:"duration"`            // Execution time
	Timestamp time.Time       `json:"timestamp"`           // When the query was executed
}

// ErrorResponse creates an error response with the given message.
func ErrorResponse(err string, start time.Time) *Response {
	return &Response{
		Status:    "error",
		Error:     err,
		Duration:  time.Since(start),
		Timestamp: time.Now(),
	}
}

// DefaultTimeout is the default query timeout in seconds.
const DefaultTimeout = 30

// DefaultQueryPort is the default port where bot query servers listen.
const DefaultQueryPort = 9476
