package frontend

import (
	"io"
	"net"
	"net/http"
	"net/http/httptest"
	"net/url"
	"strconv"
	"strings"
	"testing"
	"time"

	"the0/internal/dev"
)

// TestNew_BindsToLoopback asserts that New binds only to 127.0.0.1, not
// to all interfaces. Prior to this, the server listened on :<port> which
// made the bot event stream reachable from any host on the LAN.
func TestNew_BindsToLoopback(t *testing.T) {
	srv, err := New("/tmp", "", "test-bot", 0)
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	defer srv.ln.Close()

	addr := srv.Addr()
	if !strings.HasPrefix(addr, "127.0.0.1:") {
		t.Errorf("Addr() = %q, want 127.0.0.1:PORT", addr)
	}
}

// TestCheckOrigin_LocalhostAllowed asserts the WebSocket origin check
// accepts requests from 127.0.0.1 and localhost (the shell page), and
// accepts requests with no Origin header (curl, same-origin fetch).
func TestCheckOrigin_LocalhostAllowed(t *testing.T) {
	cases := []struct {
		name   string
		origin string
		want   bool
	}{
		{"empty (same-origin)", "", true},
		{"127.0.0.1 http", "http://127.0.0.1:12345", true},
		{"localhost http", "http://localhost:54321", true},
		{"evil.com rejected", "http://evil.com", false},
		{"https external rejected", "https://other.host/", false},
		{"localhost subdomain rejected", "http://foo.localhost:80", false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			r := httptest.NewRequest("GET", "/events", nil)
			if tc.origin != "" {
				r.Header.Set("Origin", tc.origin)
			}
			got := upgrader.CheckOrigin(r)
			if got != tc.want {
				t.Errorf("CheckOrigin(%q) = %v, want %v", tc.origin, got, tc.want)
			}
		})
	}
}

// TestServeQuery_ProxiesToQueryPort spins up a test upstream on a free
// port, hands that port to a Server via its queryPort field, and hits
// /query/status on the server. The upstream should receive the request
// at /status (with /query stripped).
func TestServeQuery_ProxiesToQueryPort(t *testing.T) {
	var gotPath string
	upstream := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		gotPath = r.URL.Path
		_, _ = w.Write([]byte(`{"ok":true}`))
	}))
	defer upstream.Close()

	// Parse port out of upstream.URL
	port := portFromURL(t, upstream.URL)

	srv, err := New("/tmp", "", "test-bot", 0)
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	srv.queryPort = port
	defer srv.ln.Close()

	// Front-of-house HTTP mux (same as Run() sets up)
	mux := http.NewServeMux()
	mux.HandleFunc("/query/", srv.serveQuery)
	front := httptest.NewServer(mux)
	defer front.Close()

	resp, err := http.Get(front.URL + "/query/status")
	if err != nil {
		t.Fatalf("GET /query/status: %v", err)
	}
	defer resp.Body.Close()
	body, _ := io.ReadAll(resp.Body)

	if resp.StatusCode != 200 {
		t.Errorf("status = %d, want 200", resp.StatusCode)
	}
	if got := string(body); got != `{"ok":true}` {
		t.Errorf("body = %q, want upstream response", got)
	}
	if gotPath != "/status" {
		t.Errorf("upstream saw %q, want %q (/query prefix must be stripped)", gotPath, "/status")
	}
}

// TestEmit_BroadcastsToSubscribers asserts that Emit fans events out via
// the broker so concurrent WebSocket subscribers all see them. We exercise
// only the broker layer here (WS framing is gorilla's concern).
func TestEmit_BroadcastsToSubscribers(t *testing.T) {
	srv, err := New("/tmp", "", "test-bot", 0)
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	defer srv.ln.Close()

	sub1 := srv.events.subscribe()
	sub2 := srv.events.subscribe()
	defer srv.events.unsubscribe(sub1)
	defer srv.events.unsubscribe(sub2)

	srv.Emit(dev.Event{Kind: dev.EventMetric, Timestamp: time.Now(), MetricType: "x"})

	for i, ch := range []chan []byte{sub1, sub2} {
		select {
		case b := <-ch:
			if !strings.Contains(string(b), `"kind":"metric"`) {
				t.Errorf("sub %d payload = %q, missing metric kind", i, b)
			}
		case <-time.After(100 * time.Millisecond):
			t.Errorf("sub %d never received broadcast", i)
		}
	}
}

func portFromURL(t *testing.T, u string) int {
	t.Helper()
	parsed, err := url.Parse(u)
	if err != nil {
		t.Fatalf("parse %q: %v", u, err)
	}
	_, portStr, err := net.SplitHostPort(parsed.Host)
	if err != nil {
		t.Fatalf("split host %q: %v", parsed.Host, err)
	}
	port, err := strconv.Atoi(portStr)
	if err != nil {
		t.Fatalf("atoi %q: %v", portStr, err)
	}
	return port
}
