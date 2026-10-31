package internal

import (
	"errors"
	"io"
	"net/http"
	"net/http/httptest"
	"testing"
	"time"

	"the0/internal"
)

// keyServer answers 401 unless the request carries the given key.
func keyServer(t *testing.T, goodKey string, seen *[]string) *httptest.Server {
	t.Helper()
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		body, _ := io.ReadAll(r.Body)
		*seen = append(*seen, r.Header.Get("Authorization")+" "+string(body))
		if r.Header.Get("Authorization") != "ApiKey "+goodKey {
			w.WriteHeader(http.StatusUnauthorized)
			_, _ = w.Write([]byte(`{"statusCode":401,"message":"Invalid API key"}`))
			return
		}
		_, _ = w.Write([]byte(`{"id":"bot-1","name":"b"}`))
	}))
	t.Cleanup(srv.Close)
	return srv
}

func TestAPIClient_LogsInAgainOnceWhenTheKeyIsRejected(t *testing.T) {
	t.Run("retries the call with the new key and keeps it for later calls", func(t *testing.T) {
		var seen []string
		srv := keyServer(t, "fresh", &seen)

		client := internal.NewAPIClient(srv.URL)
		logins := 0
		client.Relogin = func() (*internal.Auth, error) {
			logins++
			return &internal.Auth{APIKey: "fresh", CreatedAt: time.Now()}, nil
		}
		auth := &internal.Auth{APIKey: "stale"}

		bot, err := client.DeployBotInstance(auth, &internal.BotDeployRequest{Name: "b"})
		if err != nil {
			t.Fatalf("DeployBotInstance() error = %v", err)
		}
		if bot.ID != "bot-1" {
			t.Errorf("bot.ID = %q, want bot-1", bot.ID)
		}
		if logins != 1 {
			t.Errorf("Relogin called %d times, want 1", logins)
		}
		if auth.APIKey != "fresh" {
			t.Errorf("auth.APIKey = %q, want the new key kept for later calls", auth.APIKey)
		}
		want := []string{
			`ApiKey stale {"name":"b","config":null}`,
			`ApiKey fresh {"name":"b","config":null}`,
		}
		if len(seen) != len(want) || seen[0] != want[0] || seen[1] != want[1] {
			t.Errorf("requests = %q, want %q", seen, want)
		}
	})

	t.Run("gives up when the new key is rejected too", func(t *testing.T) {
		var seen []string
		srv := keyServer(t, "never", &seen)

		client := internal.NewAPIClient(srv.URL)
		logins := 0
		client.Relogin = func() (*internal.Auth, error) {
			logins++
			return &internal.Auth{APIKey: "also-bad"}, nil
		}

		_, err := client.ListBots(&internal.Auth{APIKey: "stale"})
		if !internal.IsAuthError(err) {
			t.Fatalf("error = %v, want an auth error", err)
		}
		if logins != 1 {
			t.Errorf("Relogin called %d times, want 1", logins)
		}
		if len(seen) != 2 {
			t.Errorf("made %d requests, want 2", len(seen))
		}
	})

	t.Run("reports a failed login as a ReloginError", func(t *testing.T) {
		var seen []string
		srv := keyServer(t, "fresh", &seen)

		client := internal.NewAPIClient(srv.URL)
		cause := errors.New("API key cannot be empty")
		client.Relogin = func() (*internal.Auth, error) { return nil, cause }

		_, err := client.ListBots(&internal.Auth{APIKey: "stale"})
		var reloginErr *internal.ReloginError
		if !errors.As(err, &reloginErr) {
			t.Fatalf("error = %v (%T), want *ReloginError", err, err)
		}
		if !errors.Is(err, cause) {
			t.Errorf("error = %v, want it to wrap %v", err, cause)
		}
		if err.Error() != cause.Error() {
			t.Errorf("error text = %q, want %q", err.Error(), cause.Error())
		}
	})

	t.Run("does not log in again without a Relogin hook", func(t *testing.T) {
		var seen []string
		srv := keyServer(t, "fresh", &seen)

		_, err := internal.NewAPIClient(srv.URL).ListBots(&internal.Auth{APIKey: "stale"})
		if !internal.IsAuthError(err) {
			t.Fatalf("error = %v, want an auth error", err)
		}
		if len(seen) != 1 {
			t.Errorf("made %d requests, want 1", len(seen))
		}
	})

	t.Run("does not log in again when testing a key", func(t *testing.T) {
		var seen []string
		srv := keyServer(t, "fresh", &seen)

		client := internal.NewAPIClient(srv.URL)
		client.Relogin = func() (*internal.Auth, error) {
			t.Error("Relogin called while testing a key")
			return &internal.Auth{APIKey: "fresh"}, nil
		}

		if err := client.TestAPIKey(&internal.Auth{APIKey: "stale"}); !internal.IsAuthError(err) {
			t.Fatalf("TestAPIKey() error = %v, want an auth error", err)
		}
	})

	t.Run("does not log in again for other failures", func(t *testing.T) {
		srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			w.WriteHeader(http.StatusBadRequest)
			_, _ = w.Write([]byte(`{"message":"Bot not found"}`))
		}))
		defer srv.Close()

		client := internal.NewAPIClient(srv.URL)
		client.Relogin = func() (*internal.Auth, error) {
			t.Error("Relogin called for a 400")
			return nil, nil
		}

		if _, err := client.ListBots(&internal.Auth{APIKey: "k"}); err == nil {
			t.Fatal("ListBots() error = nil, want the 400")
		}
	})
}
