package query

import (
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
)

func TestFailedRunResponse(t *testing.T) {
	t.Run("returns the error the bot wrote to its result", func(t *testing.T) {
		response := FailedRunResponse(
			[]byte(`{"status":"error","error":"No handler for path: /status"}`),
			func() string {
				t.Fatal("fallback should not run when the bot wrote a result")
				return ""
			},
			time.Now(),
		)

		assert.Equal(t, "error", response.Status)
		assert.Equal(t, "No handler for path: /status", response.Error)
	})

	t.Run("falls back to the process output when the bot wrote no result", func(t *testing.T) {
		response := FailedRunResponse(nil, func() string { return "exited with code 1: Traceback" }, time.Now())

		assert.Equal(t, "error", response.Status)
		assert.Equal(t, "exited with code 1: Traceback", response.Error)
	})
}
