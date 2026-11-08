package cmd

import (
	"io"
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/spf13/cobra"
	"the0/internal"
)

// fakeAPI accepts the saved key "good" and answers route with status and
// body. It returns the methods route was called with.
func fakeAPI(t *testing.T, route string, status int, body string) *[]string {
	t.Helper()
	calls := &[]string{}
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		switch r.URL.Path {
		case "/auth/validate-api-key":
			_, _ = w.Write([]byte(`{"success":true,"data":{"valid":true}}`))
		case route:
			*calls = append(*calls, r.Method)
			w.WriteHeader(status)
			_, _ = w.Write([]byte(body))
		default:
			http.NotFound(w, r)
		}
	}))
	t.Cleanup(srv.Close)

	internal.SetConfigDir(t.TempDir())
	t.Cleanup(func() { internal.SetConfigDir("") })
	t.Setenv("THE0_API_URL", srv.URL)
	if err := internal.SaveAuth(&internal.Auth{APIKey: "good"}); err != nil {
		t.Fatal(err)
	}
	return calls
}

func TestBotCommands_ReturnTheServersReason(t *testing.T) {
	t.Run("query a bot that defines no queries", func(t *testing.T) {
		captureLogger(t)
		fakeAPI(t, "/query/bot-1", 422,
			`{"statusCode":422,"message":"This bot defines no queries. Add a query entrypoint (entrypoints.query in bot-config.yaml) to answer them.","error":"Unprocessable Entity"}`)

		err := executeBotQuery("bot-1", "/status", "{}", 1, false)

		want := "API error 422: This bot defines no queries. Add a query entrypoint (entrypoints.query in bot-config.yaml) to answer them."
		if err == nil || err.Error() != want {
			t.Errorf("error = %v, want %q", err, want)
		}
	})

	t.Run("delete a bot the api does not know", func(t *testing.T) {
		_, errOut := captureLogger(t)
		fakeAPI(t, "/bot/bot-1", 400, `{"statusCode":400,"message":"Bot not found","error":"Bad Request"}`)

		err := deleteBotInstance(&cobra.Command{}, []string{"bot-1"}, true)

		if want := "API error 400: Bot not found"; err == nil || err.Error() != want {
			t.Errorf("error = %v, want %q", err, want)
		}
		if got, want := errOut.String(), "x Deletion failed\n"; got != want {
			t.Errorf("printed %q before returning, want only the headline %q", got, want)
		}
	})

	t.Run("reject bad query params before calling the api", func(t *testing.T) {
		captureLogger(t)

		err := executeBotQuery("bot-1", "/status", "{not json", 1, false)

		if err == nil {
			t.Fatal("error = nil, want the params to be rejected")
		}
	})
}

func TestBotDisableAll_ReportsWhatItDisabled(t *testing.T) {
	t.Run("lists the bots it disabled and counts the ones already disabled", func(t *testing.T) {
		out, _ := captureLogger(t)
		calls := fakeAPI(t, "/bot/disable-all", 200,
			`{"disabled":[{"id":"bot-1","name":"Grid"},{"id":"bot-2","name":"SMA"}],"alreadyDisabled":[{"id":"bot-3","name":"Old"}]}`)

		err := disableAllBots()

		if err != nil {
			t.Fatalf("error = %v, want nil", err)
		}
		if len(*calls) != 1 || (*calls)[0] != "POST" {
			t.Errorf("calls = %v, want one POST", *calls)
		}
		want := "* Disabling bots...\n" +
			"v Disabled 2 bots: Grid, SMA (1 already disabled)\n" +
			"  Grid (bot-1)\n" +
			"  SMA (bot-2)\n" +
			"  Stop sent to the runtime. Open positions with brokers are not closed.\n" +
			"  Re-enable a bot with 'the0 bot enable <bot_id>'.\n"
		if got := out.String(); got != want {
			t.Errorf("printed\n%s\nwant\n%s", got, want)
		}
	})

	t.Run("says nothing changed when every bot was already disabled", func(t *testing.T) {
		out, _ := captureLogger(t)
		fakeAPI(t, "/bot/disable-all", 200,
			`{"disabled":[],"alreadyDisabled":[{"id":"bot-1","name":"Grid"},{"id":"bot-2","name":"SMA"}]}`)

		err := disableAllBots()

		if err != nil {
			t.Fatalf("error = %v, want nil", err)
		}
		if got, want := out.String(), "* Disabling bots...\nv Disabled 0 bots (2 already disabled)\n"; got != want {
			t.Errorf("printed %q, want %q", got, want)
		}
	})

	t.Run("returns the api's reason when nothing could be disabled", func(t *testing.T) {
		_, errOut := captureLogger(t)
		fakeAPI(t, "/bot/disable-all", 500,
			`{"statusCode":500,"message":"Failed to write to the database","error":"Internal Server Error"}`)

		err := disableAllBots()

		if want := "API error 500: Failed to write to the database"; err == nil || err.Error() != want {
			t.Errorf("error = %v, want %q", err, want)
		}
		if got, want := errOut.String(), "x Disabling bots failed\n"; got != want {
			t.Errorf("printed %q, want %q", got, want)
		}
	})
}

func TestBotDisable_AllSendsTheDisableAllRequest(t *testing.T) {
	captureLogger(t)
	calls := fakeAPI(t, "/bot/disable-all", 200, `{"disabled":[],"alreadyDisabled":[]}`)
	cmd := NewBotDisableCmd()
	cmd.SetArgs([]string{"--all"})
	cmd.SetOut(io.Discard)
	cmd.SetErr(io.Discard)

	if err := cmd.Execute(); err != nil {
		t.Fatalf("error = %v, want nil", err)
	}
	if len(*calls) != 1 || (*calls)[0] != "POST" {
		t.Errorf("calls to /bot/disable-all = %v, want one POST", *calls)
	}
}

func TestBotDisable_TakesABotIDOrAll(t *testing.T) {
	for _, tc := range []struct {
		name string
		args []string
	}{
		{"neither a bot id nor --all", []string{}},
		{"a bot id with --all", []string{"--all", "bot-1"}},
		{"two bot ids", []string{"bot-1", "bot-2"}},
	} {
		t.Run("refuses "+tc.name, func(t *testing.T) {
			cmd := NewBotDisableCmd()
			cmd.SetArgs(tc.args)
			cmd.SetOut(io.Discard)
			cmd.SetErr(io.Discard)
			cmd.RunE = func(*cobra.Command, []string) error {
				t.Fatal("ran the command, want the arguments refused")
				return nil
			}

			if err := cmd.Execute(); err == nil {
				t.Error("error = nil, want the arguments refused")
			}
		})
	}
}
