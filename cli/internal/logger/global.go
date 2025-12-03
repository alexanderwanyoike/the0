package logger

import "sync"

var (
	globalLogger *Logger
	once         sync.Once
)

// SetGlobal sets the global logger instance
func SetGlobal(l *Logger) {
	globalLogger = l
}

// Global returns the global logger instance
func Global() *Logger {
	once.Do(func() {
		if globalLogger == nil {
			globalLogger = Default()
		}
	})
	return globalLogger
}

// Convenience functions that use global logger

func Success(format string, args ...interface{}) { Global().Success(format, args...) }
func Error(format string, args ...interface{})   { Global().Error(format, args...) }
func Warning(format string, args ...interface{}) { Global().Warning(format, args...) }
func Info(format string, args ...interface{})    { Global().Info(format, args...) }
func Verbose(format string, args ...interface{}) { Global().Verbose(format, args...) }
func Debug(format string, args ...interface{})   { Global().Debug(format, args...) }
func Print(format string, args ...interface{})   { Global().Print(format, args...) }
func Printf(format string, args ...interface{})  { Global().Printf(format, args...) }
func Newline()                                   { Global().Newline() }

func StartSpinner(message string)           { Global().StartSpinner(message) }
func UpdateSpinner(message string)          { Global().UpdateSpinner(message) }
func StopSpinner()                          { Global().StopSpinner() }
func StopSpinnerWithSuccess(message string) { Global().StopSpinnerWithSuccess(message) }
func StopSpinnerWithError(message string)   { Global().StopSpinnerWithError(message) }

func ErrorWithDetails(msg string, details string) { Global().ErrorWithDetails(msg, details) }
func IsVerbose() bool                             { return Global().IsVerbose() }
