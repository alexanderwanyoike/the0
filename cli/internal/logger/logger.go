package logger

import (
	"fmt"
	"io"
	"os"
	"sync"
	"time"

	"github.com/briandowns/spinner"
	"github.com/fatih/color"
)

// Level represents log verbosity levels
type Level int

const (
	LevelNormal Level = iota
	LevelVerbose
	LevelDebug
)

// Logger provides structured CLI output with spinners and colored messages
type Logger struct {
	mu        sync.Mutex
	out       io.Writer
	errOut    io.Writer
	level     Level
	spinner   *spinner.Spinner
	noColor   bool
	noSpinner bool

	// Color helpers
	green  *color.Color
	red    *color.Color
	yellow *color.Color
	blue   *color.Color
	cyan   *color.Color
	white  *color.Color
}

// Config holds logger configuration
type Config struct {
	Out       io.Writer
	ErrOut    io.Writer
	Verbose   bool
	Debug     bool
	NoColor   bool
	NoSpinner bool
}

// New creates a new logger instance
func New(cfg Config) *Logger {
	if cfg.Out == nil {
		cfg.Out = os.Stdout
	}
	if cfg.ErrOut == nil {
		cfg.ErrOut = os.Stderr
	}

	level := LevelNormal
	if cfg.Debug {
		level = LevelDebug
	} else if cfg.Verbose {
		level = LevelVerbose
	}

	l := &Logger{
		out:       cfg.Out,
		errOut:    cfg.ErrOut,
		level:     level,
		noColor:   cfg.NoColor || os.Getenv("NO_COLOR") != "",
		noSpinner: cfg.NoSpinner || os.Getenv("THE0_NO_SPINNER") != "",
		green:     color.New(color.FgGreen),
		red:       color.New(color.FgRed),
		yellow:    color.New(color.FgYellow),
		blue:      color.New(color.FgBlue),
		cyan:      color.New(color.FgCyan),
		white:     color.New(color.FgWhite),
	}

	if cfg.NoColor {
		color.NoColor = true
	}

	return l
}

// Default returns a logger with default configuration
func Default() *Logger {
	return New(Config{})
}

// --- Core Output Methods ---

// Success prints a success message with green checkmark
func (l *Logger) Success(format string, args ...interface{}) {
	l.stopSpinnerIfActive()
	msg := fmt.Sprintf(format, args...)
	l.green.Fprintf(l.out, "v %s\n", msg)
}

// Error prints an error message with red X
func (l *Logger) Error(format string, args ...interface{}) {
	l.stopSpinnerIfActive()
	msg := fmt.Sprintf(format, args...)
	l.red.Fprintf(l.errOut, "x %s\n", msg)
}

// ErrorWithDetails prints error with additional context (always shown, even in non-verbose mode)
func (l *Logger) ErrorWithDetails(msg string, details string) {
	l.stopSpinnerIfActive()
	l.red.Fprintf(l.errOut, "x %s\n", msg)
	if details != "" {
		fmt.Fprintf(l.errOut, "  Details: %s\n", details)
	}
}

// Warning prints a warning message with yellow !
func (l *Logger) Warning(format string, args ...interface{}) {
	l.stopSpinnerIfActive()
	msg := fmt.Sprintf(format, args...)
	l.yellow.Fprintf(l.out, "! %s\n", msg)
}

// Info prints an info message with blue i
func (l *Logger) Info(format string, args ...interface{}) {
	l.stopSpinnerIfActive()
	msg := fmt.Sprintf(format, args...)
	l.blue.Fprintf(l.out, "i %s\n", msg)
}

// Print prints a plain message (no icon)
func (l *Logger) Print(format string, args ...interface{}) {
	l.stopSpinnerIfActive()
	fmt.Fprintf(l.out, format+"\n", args...)
}

// Printf prints without newline
func (l *Logger) Printf(format string, args ...interface{}) {
	l.stopSpinnerIfActive()
	fmt.Fprintf(l.out, format, args...)
}

// --- Verbose Output (only shown with -v flag) ---

// Verbose prints only when verbose mode is enabled
func (l *Logger) Verbose(format string, args ...interface{}) {
	if l.level >= LevelVerbose {
		l.stopSpinnerIfActive()
		msg := fmt.Sprintf(format, args...)
		l.cyan.Fprintf(l.out, "  %s\n", msg)
	}
}

// Debug prints only when debug mode is enabled
func (l *Logger) Debug(format string, args ...interface{}) {
	if l.level >= LevelDebug {
		l.stopSpinnerIfActive()
		msg := fmt.Sprintf(format, args...)
		l.white.Fprintf(l.out, "[DEBUG] %s\n", msg)
	}
}

// --- Spinner Methods ---

// StartSpinner starts a spinner with the given message
func (l *Logger) StartSpinner(message string) {
	l.mu.Lock()
	defer l.mu.Unlock()

	if l.noSpinner {
		// Fallback to text indicator
		fmt.Fprintf(l.out, "* %s...\n", message)
		return
	}

	if l.spinner != nil {
		l.spinner.Stop()
	}

	l.spinner = spinner.New(spinner.CharSets[14], 100*time.Millisecond)
	l.spinner.Suffix = " " + message
	l.spinner.Writer = l.out
	l.spinner.Start()
}

// UpdateSpinner updates the spinner message
func (l *Logger) UpdateSpinner(message string) {
	l.mu.Lock()
	defer l.mu.Unlock()

	if l.spinner != nil {
		l.spinner.Suffix = " " + message
	}
}

// StopSpinner stops the spinner
func (l *Logger) StopSpinner() {
	l.mu.Lock()
	defer l.mu.Unlock()

	if l.spinner != nil {
		l.spinner.Stop()
		l.spinner = nil
	}
}

// StopSpinnerWithSuccess stops spinner and shows success message
func (l *Logger) StopSpinnerWithSuccess(message string) {
	l.StopSpinner()
	l.Success("%s", message)
}

// StopSpinnerWithError stops spinner and shows error
func (l *Logger) StopSpinnerWithError(message string) {
	l.StopSpinner()
	l.Error("%s", message)
}

func (l *Logger) stopSpinnerIfActive() {
	l.mu.Lock()
	defer l.mu.Unlock()
	if l.spinner != nil {
		l.spinner.Stop()
		l.spinner = nil
	}
}

// --- Utility Methods ---

// SetLevel sets the logging level
func (l *Logger) SetLevel(level Level) {
	l.level = level
}

// IsVerbose returns true if verbose logging is enabled
func (l *Logger) IsVerbose() bool {
	return l.level >= LevelVerbose
}

// Newline prints an empty line
func (l *Logger) Newline() {
	l.stopSpinnerIfActive()
	fmt.Fprintln(l.out)
}
