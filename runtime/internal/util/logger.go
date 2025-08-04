package util

import (
	"fmt"
	"testing"
)

type Logger interface {
	Info(msg string, fields ...interface{})
	Error(msg string, fields ...interface{})
	Debug(msg string, fields ...interface{})
}

type DefaultLogger struct{}

func (l *DefaultLogger) Info(msg string, fields ...interface{}) {
	fmt.Println("[INFO]:", msg, fields)
}

func (l *DefaultLogger) Error(msg string, fields ...interface{}) {
	fmt.Println("[ERROR]:", msg, fields)
}

func (l *DefaultLogger) Debug(msg string, fields ...interface{}) {
	fmt.Println("[DEBUG]:", msg, fields)
}

// Simple helper functions for master and worker logging
func LogMaster(msg string, args ...interface{}) {
	if len(args) > 0 {
		fmt.Printf("MASTER: "+msg+"\n", args...)
	} else {
		fmt.Println("MASTER:", msg)
	}
}

func LogWorker(msg string, args ...interface{}) {
	if len(args) > 0 {
		fmt.Printf("WORKER: "+msg+"\n", args...)
	} else {
		fmt.Println("WORKER:", msg)
	}
}

type TestLogger struct {
	T *testing.T
}

func (l *TestLogger) Info(msg string, fields ...interface{}) {
	if len(fields) > 0 {
		l.T.Logf("INFO: "+msg, fields...)
	} else {
		l.T.Log("INFO: " + msg)
	}
}

func (l *TestLogger) Error(msg string, fields ...interface{}) {
	if len(fields) > 0 {
		l.T.Logf("ERROR: "+msg, fields...)
	} else {
		l.T.Log("ERROR: " + msg)
	}
}

func (l *TestLogger) Debug(msg string, fields ...interface{}) {
	if len(fields) > 0 {
		l.T.Logf("DEBUG: "+msg, fields...)
	} else {
		l.T.Log("DEBUG: " + msg)
	}
}
