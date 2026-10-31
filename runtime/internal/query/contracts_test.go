package query

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"net/http/httptest"
	"sort"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"runtime/internal/contracts"
)

type queryContract struct {
	Port      int             `json:"port"`
	Path      string          `json:"path"`
	Request   json.RawMessage `json:"request"`
	Responses []struct {
		Case   string                 `json:"case"`
		Status int                    `json:"status"`
		Body   map[string]interface{} `json:"body"`
	} `json:"responses"`
}

type stubResolver struct{ err error }

func (r stubResolver) ResolveBot(context.Context, string) (string, error) { return "", r.err }

type stubExecutor struct {
	response func(Request) *Response
	got      *Request
}

func (e stubExecutor) ExecuteQuery(_ context.Context, req Request, _ string) *Response {
	*e.got = req
	return e.response(req)
}

type queryServerSetup struct {
	resolveErr error
	answer     func(Request) *Response
}

// queryServerSetups makes the server reach each case of the contract the way
// the Docker and Kubernetes backends make it: a resolver refusing the bot, or
// an executor's answer. Every case in the contract needs one here.
func queryServerSetups(botID string, start time.Time) map[string]queryServerSetup {
	return map[string]queryServerSetup{
		"answered": {answer: func(Request) *Response {
			return ParseQueryOutput([]byte(`{"status":"ok","data":{"trades":3,"pnl":12.5}}`), start)
		}},
		"bot not found":      {resolveErr: fmt.Errorf("bot not found: %s", botID)},
		"no queries defined": {resolveErr: NoQueryEntrypointError(botID)},
		"no handler for the path": {answer: func(Request) *Response {
			sdkResult := `{"status":"error","error":"No handler for path: /status","available":["/today","/history","/health","/info"]}`
			return FailedRunResponse([]byte(sdkResult), func() string { return "" }, start)
		}},
		"bot not running": {answer: func(Request) *Response {
			return ErrorResponse("failed to get container IP: container is not running", start)
		}},
	}
}

func keysOf(m map[string]interface{}) []string {
	keys := make([]string, 0, len(m))
	for key := range m {
		keys = append(keys, key)
	}
	sort.Strings(keys)
	return keys
}

func TestContracts_QueryServerListensOnTheApisDefaultPort(t *testing.T) {
	var contract queryContract
	contracts.Load(t, "query.json", &contract)

	assert.Equal(t, contract.Port, NewServer(ServerConfig{}).port)
}

func TestContracts_QueryServerAnswersAsTheApiExpects(t *testing.T) {
	var contract queryContract
	contracts.Load(t, "query.json", &contract)
	var sent struct {
		BotID      string                 `json:"bot_id"`
		QueryPath  string                 `json:"query_path"`
		Params     map[string]interface{} `json:"params"`
		TimeoutSec int                    `json:"timeout_sec"`
	}
	require.NoError(t, json.Unmarshal(contract.Request, &sent))
	setups := queryServerSetups(sent.BotID, time.Now())

	for _, want := range contract.Responses {
		t.Run(want.Case, func(t *testing.T) {
			setup, ok := setups[want.Case]
			require.True(t, ok, "no runtime setup for contract case %q", want.Case)
			var got Request
			server := NewServer(ServerConfig{
				Resolver: stubResolver{err: setup.resolveErr},
				Executor: stubExecutor{response: setup.answer, got: &got},
			})
			w := httptest.NewRecorder()

			server.HandleQuery(w, httptest.NewRequest(http.MethodPost, contract.Path, bytes.NewReader(contract.Request)))

			assert.Equal(t, want.Status, w.Code)
			var body map[string]interface{}
			require.NoError(t, json.Unmarshal(w.Body.Bytes(), &body))
			assert.Equal(t, keysOf(want.Body), keysOf(body))
			for _, field := range []string{"status", "error", "data"} {
				assert.Equal(t, want.Body[field], body[field], field)
			}
			assert.IsType(t, float64(0), body["duration"], "duration is a number of nanoseconds")
			_, err := time.Parse(time.RFC3339Nano, fmt.Sprint(body["timestamp"]))
			assert.NoError(t, err, "timestamp")
			if setup.answer != nil {
				assert.Equal(t, Request{
					BotID: sent.BotID, QueryPath: sent.QueryPath, Params: sent.Params, TimeoutSec: sent.TimeoutSec,
				}, got)
			}
		})
	}
}
