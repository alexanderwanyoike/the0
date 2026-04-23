//go:build windows

package dev

import "fmt"

// Lock is a stub on Windows. WSL users get real flock via the Linux build.
type Lock struct{}

func AcquireLock(_ string) (*Lock, error) {
	return &Lock{}, nil
}

func (l *Lock) Release() error {
	return nil
}

func init() {
	fmt.Println("warning: per-bot flock is not supported on Windows; use WSL for safe concurrency")
}
