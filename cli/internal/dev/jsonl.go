package dev

import (
	"encoding/json"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sync"
)

// JSONLSink appends each event as a single-line JSON record to a file. This
// is the replay log for `.the0/dev/<botId>/events.jsonl`.
type JSONLSink struct {
	mu sync.Mutex
	f  io.WriteCloser
}

func NewJSONLSink(path string) (*JSONLSink, error) {
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return nil, fmt.Errorf("ensure dir: %w", err)
	}
	f, err := os.OpenFile(path, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0o644)
	if err != nil {
		return nil, fmt.Errorf("open %s: %w", path, err)
	}
	return &JSONLSink{f: f}, nil
}

func (j *JSONLSink) Emit(ev Event) {
	j.mu.Lock()
	defer j.mu.Unlock()
	b, err := json.Marshal(ev)
	if err != nil {
		return
	}
	_, _ = j.f.Write(b)
	_, _ = j.f.Write([]byte{'\n'})
}

func (j *JSONLSink) Close() error {
	return j.f.Close()
}
