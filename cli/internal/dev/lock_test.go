package dev

import (
	"path/filepath"
	"strings"
	"testing"
)

func TestAcquireLock_BlocksSecondCaller(t *testing.T) {
	path := filepath.Join(t.TempDir(), ".lock")

	l1, err := AcquireLock(path)
	if err != nil {
		t.Fatalf("first AcquireLock: %v", err)
	}
	defer l1.Release()

	_, err = AcquireLock(path)
	if err == nil {
		t.Fatal("expected second AcquireLock to fail while first is held")
	}
	if !strings.Contains(err.Error(), "the0 dev") {
		t.Errorf("error should mention running the0 dev: %v", err)
	}
}

func TestAcquireLock_ReacquiresAfterRelease(t *testing.T) {
	path := filepath.Join(t.TempDir(), ".lock")

	l1, err := AcquireLock(path)
	if err != nil {
		t.Fatalf("first: %v", err)
	}
	if err := l1.Release(); err != nil {
		t.Fatalf("release: %v", err)
	}

	l2, err := AcquireLock(path)
	if err != nil {
		t.Fatalf("reacquire: %v", err)
	}
	l2.Release()
}
