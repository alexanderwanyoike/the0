//go:build !windows

package dev

import (
	"errors"
	"fmt"
	"os"
	"strconv"
	"syscall"
)

// Lock is a best-effort file lock preventing two concurrent `the0 dev`
// invocations on the same bot from stomping each other's state dir.
//
// It uses flock(LOCK_EX|LOCK_NB) and writes the holder's PID into the lock
// file so a second invocation can report who already has it.
type Lock struct {
	file *os.File
}

// AcquireLock tries to take the lock at path. If another process already
// holds it, it returns an error naming that process's PID.
func AcquireLock(path string) (*Lock, error) {
	f, err := os.OpenFile(path, os.O_RDWR|os.O_CREATE, 0o644)
	if err != nil {
		return nil, err
	}
	if err := syscall.Flock(int(f.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); err != nil {
		existing, _ := os.ReadFile(path)
		_ = f.Close()
		if errors.Is(err, syscall.EWOULDBLOCK) {
			pid := "unknown"
			if p := string(existing); p != "" {
				pid = p
			}
			return nil, fmt.Errorf("another `the0 dev` is running for this bot (PID %s); stop it or use a different --bot-id", pid)
		}
		return nil, err
	}
	if err := f.Truncate(0); err != nil {
		_ = f.Close()
		return nil, err
	}
	if _, err := f.Seek(0, 0); err != nil {
		_ = f.Close()
		return nil, err
	}
	if _, err := f.WriteString(strconv.Itoa(os.Getpid())); err != nil {
		_ = f.Close()
		return nil, err
	}
	return &Lock{file: f}, nil
}

// Release drops the lock and removes the file.
func (l *Lock) Release() error {
	if l == nil || l.file == nil {
		return nil
	}
	path := l.file.Name()
	_ = syscall.Flock(int(l.file.Fd()), syscall.LOCK_UN)
	_ = l.file.Close()
	return os.Remove(path)
}
