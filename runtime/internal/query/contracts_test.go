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

type queryAnswer struct {
	Case string `json:"case"`
	// Method and Request, when set, replace the POST of the contract's
	// request: the api never sends anything else, but the server answers it.
	Method  string                 `json:"method"`
	Request json.RawMessage        `json:"request"`
	Status  int                    `json:"status"`
	Body    map[string]interface{} `json:"body"`
}

type queryContract struct {
	Port      int             `json:"port"`
	Path      string          `json:"path"`
	Request   json.RawMessage `json:"request"`
	Responses []queryAnswer   `json:"responses"`
}

type stubResolver struct{ err error }

func (r stubResolver) ResolveBot(context.Context, string) (string, error) { return "", r.err }

type stubExecutor struct {
	response func() *Response
	got      *Request
}

func (e stubExecutor) ExecuteQuery(_ context.Context, req Request, _ string) *Response {
	*e.got = req
	return e.response()
}

type queryServerSetup struct {
	resolveErr error
	answer     func() *Response
}

// queryServerSetups makes the server reach each case of the contract the way
// the Docker and Kubernetes backends make it: a resolver refusing the bot, or
// an executor's answer. A case the server refuses before resolving needs no
// setup, but every case in the contract needs an entry here.
func queryServerSetups(t *testing.T, botID string, answered queryAnswer, start time.Time) map[string]queryServerSetup {
	sdkAnswer, err := json.Marshal(map[string]interface{}{"status": "ok", "data": answered.Body["data"]})
	require.NoError(t, err)
	return map[string]queryServerSetup{
		"answered": {answer: func() *Response {
			return ParseQueryOutput(sdkAnswer, start)
		}},
		"bot not found":      {resolveErr: fmt.Errorf("bot not found: %s", botID)},
		"no queries defined": {resolveErr: NoQueryEntrypointError(botID)},
		"no handler for the path": {answer: func() *Response {
			sdkResult := `{"status":"error","error":"No handler for path: /status","available":["/today","/history","/health","/info"]}`
			return FailedRunResponse([]byte(sdkResult), func() string { return "" }, start)
		}},
		"bot not running": {answer: func() *Response {
			return ErrorResponse("failed to get container IP: container is not running", start)
		}},
		"no query path":      {},
		"method not allowed": {},
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

func answerOf(t *testing.T, contract queryContract, name string) queryAnswer {
	t.Helper()
	for _, answer := range contract.Responses {
		if answer.Case == name {
			return answer
		}
	}
	t.Fatalf("query.json has no %q case", name)
	return queryAnswer{}
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
	setups := queryServerSetups(t, sent.BotID, answerOf(t, contract, "answered"), time.Now())

	for _, want := range contract.Responses {
		t.Run(want.Case, func(t *testing.T) {
			setup, ok := setups[want.Case]
			require.True(t, ok, "no runtime setup for contract case %q", want.Case)
			var got Request
			server := NewServer(ServerConfig{
				Resolver: stubResolver{err: setup.resolveErr},
				Executor: stubExecutor{response: setup.answer, got: &got},
			})
			method, request := http.MethodPost, contract.Request
			if want.Method != "" {
				method = want.Method
			}
			if want.Request != nil {
				request = want.Request
			}
			w := httptest.NewRecorder()

			server.Handler().ServeHTTP(w, httptest.NewRequest(method, contract.Path, bytes.NewReader(request)))

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
