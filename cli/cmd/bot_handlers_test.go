package cmd

import (
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/spf13/cobra"
	"the0/internal"
)

// fakeAPI accepts the saved key "good" and answers route with status and body.
func fakeAPI(t *testing.T, route string, status int, body string) {
	t.Helper()
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		switch r.URL.Path {
		case "/auth/validate-api-key":
			_, _ = w.Write([]byte(`{"success":true,"data":{"valid":true}}`))
		case route:
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
