// Package query provides shared types and utilities for bot query execution.
package query

import (
	"encoding/json"
	"errors"
	"fmt"
	"net/http"
	"time"
)

// ErrNoQueryEntrypoint marks a bot whose config declares no query entrypoint,
// so it has nothing to answer queries with. It is kept apart from "bot not
// found" because the bot does exist, and callers report it differently.
// Nothing runs the bot entrypoint in its place: that would run the bot
// itself, trading included, just to answer a question.
var ErrNoQueryEntrypoint = errors.New("bot has no query entrypoint")

// NoQueryEntrypointError reports that the given bot has no query entrypoint.
func NoQueryEntrypointError(botID string) error {
	return fmt.Errorf("%w: %s", ErrNoQueryEntrypoint, botID)
}

// NoQueryEntrypointResponse is the response for a bot with no query
// entrypoint, answered as 422 like the resolver's rejection.
func NoQueryEntrypointResponse(botID string, start time.Time) *Response {
	response := ErrorResponse(NoQueryEntrypointError(botID).Error(), start)
	response.httpStatus = http.StatusUnprocessableEntity
	return response
}

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

	httpStatus int
}

// HTTPStatus is the status the query server answers with: 200 for a result,
// 500 for an error, unless the response was built with a more specific one.
func (r *Response) HTTPStatus() int {
	if r.httpStatus != 0 {
		return r.httpStatus
	}
	if r.Status == "error" {
		return http.StatusInternalServerError
	}
	return http.StatusOK
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

// FailedRunResponse builds the response for a query process that exited
// non-zero. SDKs write their own error to the result file before exiting, such
// as an unknown path along with the paths the bot does handle, and that says
// more than the process output, which is only read when no result was written.
func FailedRunResponse(result []byte, processOutput func() string, start time.Time) *Response {
	if len(result) > 0 {
		return ParseQueryOutput(result, start)
	}
	return ErrorResponse(processOutput(), start)
}

// DefaultTimeout is the default query timeout in seconds.
const DefaultTimeout = 30

// DefaultQueryPort is the default port where bot query servers listen.
const DefaultQueryPort = 9476
