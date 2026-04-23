package frontend

import (
	"context"
	_ "embed"
	"encoding/json"
	"fmt"
	"html/template"
	"net"
	"net/http"
	"net/http/httputil"
	"net/url"
	"path/filepath"
	"strings"
	"sync"
	"time"

	"github.com/gorilla/websocket"
	"the0/internal/dev"
)

//go:embed shell.html
var shellHTML string

//go:embed shell.js
var shellJS string

// Server is the HTTP + WebSocket server that backs `the0 dev --frontend`.
// It serves the user's bundled dashboard at /, the compiled bundle at
// /bundle.js, a real-time event stream at /events, and proxies
// /query/* to the bot's in-container query server.
type Server struct {
	entry     string
	botID     string
	addr      string
	ln        net.Listener
	events    *broker
	tmpl      *template.Template
	bundle    []byte
	bundleM   sync.RWMutex
	queryPort int // forwarded to 127.0.0.1:queryPort; defaults to dev.QueryPort
}

type shellData struct {
	BotID   string
	WsPath  string
	ShellJS template.JS
}

// New stands up the server, binding to loopback on the given port
// (0 = OS-assigned). Binding only on 127.0.0.1 means LAN peers cannot
// reach the bot's live event stream or query endpoint.
func New(projectDir, entry, botID string, port int) (*Server, error) {
	ln, err := net.Listen("tcp", fmt.Sprintf("127.0.0.1:%d", port))
	if err != nil {
		return nil, err
	}
	if entry == "" {
		entry = filepath.Join(projectDir, "frontend", "index.tsx")
	}
	tmpl, err := template.New("shell").Parse(shellHTML)
	if err != nil {
		return nil, err
	}
	return &Server{
		entry:     entry,
		botID:     botID,
		addr:      ln.Addr().String(),
		ln:        ln,
		events:    newBroker(),
		tmpl:      tmpl,
		queryPort: dev.QueryPort,
	}, nil
}

// Addr returns the bound address (host:port) so the caller can print the URL.
func (s *Server) Addr() string { return s.addr }

// Emit implements dev.EventSink — every runner event is forwarded to every
// connected WS client verbatim.
func (s *Server) Emit(ev dev.Event) { s.events.broadcast(ev) }

// Run serves until ctx is cancelled. Builds the user's bundle on first
// request; call RebuildBundle to re-run esbuild (e.g. from a file watcher).
func (s *Server) Run(ctx context.Context) error {
	mux := http.NewServeMux()
	mux.HandleFunc("/", s.serveShell)
	mux.HandleFunc("/bundle.js", s.serveBundle)
	mux.HandleFunc("/events", s.serveEvents)
	mux.HandleFunc("/query/", s.serveQuery)

	srv := &http.Server{Handler: mux}
	go func() {
		<-ctx.Done()
		shutCtx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
		defer cancel()
		_ = srv.Shutdown(shutCtx)
	}()
	if err := srv.Serve(s.ln); err != nil && err != http.ErrServerClosed {
		return err
	}
	return nil
}

// RebuildBundle re-runs esbuild on the user's entry and caches the output.
// Called on first request and after file-watcher restarts.
func (s *Server) RebuildBundle() error {
	b, err := BuildBundle(s.entry, false)
	if err != nil {
		return err
	}
	s.bundleM.Lock()
	s.bundle = b
	s.bundleM.Unlock()
	return nil
}

func (s *Server) serveShell(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "text/html; charset=utf-8")
	_ = s.tmpl.Execute(w, shellData{
		BotID:   s.botID,
		WsPath:  "/events",
		ShellJS: template.JS(shellJS),
	})
}

func (s *Server) serveBundle(w http.ResponseWriter, r *http.Request) {
	s.bundleM.RLock()
	b := s.bundle
	s.bundleM.RUnlock()
	if b == nil {
		if err := s.RebuildBundle(); err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		s.bundleM.RLock()
		b = s.bundle
		s.bundleM.RUnlock()
	}
	w.Header().Set("Content-Type", "application/javascript")
	w.Header().Set("Cache-Control", "no-store")
	_, _ = w.Write(b)
}

// upgrader restricts WebSocket connections to origins served by the
// local dev shell (or no Origin at all, e.g. curl). Without this check
// any webpage in the user's browser could connect to /events while
// `the0 dev --frontend` is running.
var upgrader = websocket.Upgrader{
	CheckOrigin: func(r *http.Request) bool {
		o := r.Header.Get("Origin")
		if o == "" {
			return true
		}
		// Strict: scheme://host[:port], no trailing path.
		return strings.HasPrefix(o, "http://127.0.0.1:") ||
			strings.HasPrefix(o, "http://localhost:") ||
			o == "http://localhost" ||
			o == "http://127.0.0.1"
	},
}

// serveQuery reverse-proxies /query/<path> to the bot's in-container
// query server (127.0.0.1:queryPort), stripping the /query prefix. The
// runtime image exposes the query subprocess on loopback so this
// proxy gives the browser-side dashboard a same-origin path that the
// runtime forwards to the bot's SDK-owned query router.
func (s *Server) serveQuery(w http.ResponseWriter, r *http.Request) {
	target, err := url.Parse(fmt.Sprintf("http://127.0.0.1:%d", s.queryPort))
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	proxy := httputil.NewSingleHostReverseProxy(target)
	// Default director sets scheme/host but keeps r.URL.Path; we strip the
	// /query prefix so the upstream sees the logical path.
	r.URL.Path = strings.TrimPrefix(r.URL.Path, "/query")
	if r.URL.Path == "" {
		r.URL.Path = "/"
	}
	proxy.ServeHTTP(w, r)
}

func (s *Server) serveEvents(w http.ResponseWriter, r *http.Request) {
	ws, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		return
	}
	defer ws.Close()

	ch := s.events.subscribe()
	defer s.events.unsubscribe(ch)

	ping := time.NewTicker(15 * time.Second)
	defer ping.Stop()

	for {
		select {
		case ev, ok := <-ch:
			if !ok {
				return
			}
			_ = ws.SetWriteDeadline(time.Now().Add(5 * time.Second))
			if err := ws.WriteMessage(websocket.TextMessage, ev); err != nil {
				return
			}
		case <-ping.C:
			_ = ws.SetWriteDeadline(time.Now().Add(5 * time.Second))
			if err := ws.WriteMessage(websocket.PingMessage, nil); err != nil {
				return
			}
		}
	}
}

// broker is a tiny WS fan-out: each subscriber gets every event, late
// subscribers miss earlier events (no replay here; replay happens from
// events.jsonl when the user refreshes).
type broker struct {
	mu   sync.Mutex
	subs map[chan []byte]struct{}
}

func newBroker() *broker {
	return &broker{subs: make(map[chan []byte]struct{})}
}

func (b *broker) subscribe() chan []byte {
	ch := make(chan []byte, 128)
	b.mu.Lock()
	b.subs[ch] = struct{}{}
	b.mu.Unlock()
	return ch
}

func (b *broker) unsubscribe(ch chan []byte) {
	b.mu.Lock()
	delete(b.subs, ch)
	close(ch)
	b.mu.Unlock()
}

func (b *broker) broadcast(ev dev.Event) {
	payload, err := json.Marshal(ev)
	if err != nil {
		return
	}
	b.mu.Lock()
	defer b.mu.Unlock()
	for ch := range b.subs {
		select {
		case ch <- payload:
		default:
			// drop for slow subscribers rather than blocking the runner
		}
	}
}
