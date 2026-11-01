package cmd

import (
	"bytes"
	"errors"
	"strings"
	"testing"

	"github.com/spf13/cobra"
	"the0/internal/logger"
)

// captureLogger points the global logger at buffers for the test.
func captureLogger(t *testing.T) (out, errOut *bytes.Buffer) {
	t.Helper()
	out, errOut = &bytes.Buffer{}, &bytes.Buffer{}
	previous := logger.Global()
	logger.SetGlobal(logger.New(logger.Config{Out: out, ErrOut: errOut, NoColor: true, NoSpinner: true}))
	t.Cleanup(func() { logger.SetGlobal(previous) })
	return out, errOut
}

func testRoot(run func(*cobra.Command, []string) error) *cobra.Command {
	root := &cobra.Command{Use: "the0"}
	root.AddCommand(&cobra.Command{
		Use:  "thing <id>",
		Args: cobra.ExactArgs(1),
		RunE: run,
	})
	return root
}

func TestExecute_ReportsACommandsErrorOnce(t *testing.T) {
	t.Run("a returned error is printed once as an error line, without usage", func(t *testing.T) {
		_, errOut := captureLogger(t)
		root := testRoot(func(*cobra.Command, []string) error { return errors.New("API error 404: Bot not found") })
		var cobraOut, stderr bytes.Buffer
		root.SetOut(&cobraOut)
		root.SetErr(&cobraOut)
		root.SetArgs([]string{"thing", "bot-1"})

		code := Execute(root, &stderr)

		if code != 1 {
			t.Errorf("exit code = %d, want 1", code)
		}
		if got, want := errOut.String(), "x API error 404: Bot not found\n"; got != want {
			t.Errorf("logger error output = %q, want %q", got, want)
		}
		if cobraOut.Len() != 0 || stderr.Len() != 0 {
			t.Errorf("printed more than the error line: cobra %q, stderr %q", cobraOut.String(), stderr.String())
		}
	})

	t.Run("a command that printed its own failure exits 1 without another line", func(t *testing.T) {
		out, errOut := captureLogger(t)
		root := testRoot(func(*cobra.Command, []string) error {
			logger.Error("API key is invalid")
			logger.Print("Check your API key and permissions")
			return errReported
		})
		var cobraOut, stderr bytes.Buffer
		root.SetOut(&cobraOut)
		root.SetErr(&cobraOut)
		root.SetArgs([]string{"thing", "bot-1"})

		if code := Execute(root, &stderr); code != 1 {
			t.Errorf("exit code = %d, want 1", code)
		}
		if got, want := errOut.String(), "x API key is invalid\n"; got != want {
			t.Errorf("logger error output = %q, want %q", got, want)
		}
		if got, want := out.String(), "Check your API key and permissions\n"; got != want {
			t.Errorf("logger output = %q, want %q", got, want)
		}
		if cobraOut.Len() != 0 || stderr.Len() != 0 {
			t.Errorf("printed more: cobra %q, stderr %q", cobraOut.String(), stderr.String())
		}
	})

	t.Run("a command that succeeds exits 0 and prints nothing extra", func(t *testing.T) {
		_, errOut := captureLogger(t)
		root := testRoot(func(*cobra.Command, []string) error { return nil })
		var cobraOut, stderr bytes.Buffer
		root.SetOut(&cobraOut)
		root.SetErr(&cobraOut)
		root.SetArgs([]string{"thing", "bot-1"})

		if code := Execute(root, &stderr); code != 0 {
			t.Errorf("exit code = %d, want 0", code)
		}
		if errOut.Len()+cobraOut.Len()+stderr.Len() != 0 {
			t.Errorf("unexpected output: %q %q %q", errOut.String(), cobraOut.String(), stderr.String())
		}
	})

	t.Run("wrong arguments still show cobra's error and the usage", func(t *testing.T) {
		captureLogger(t)
		root := testRoot(func(*cobra.Command, []string) error {
			t.Error("command ran with wrong arguments")
			return nil
		})
		var cobraOut, stderr bytes.Buffer
		root.SetOut(&cobraOut)
		root.SetErr(&cobraOut)
		root.SetArgs([]string{"thing"})

		if code := Execute(root, &stderr); code != 1 {
			t.Errorf("exit code = %d, want 1", code)
		}
		if !strings.Contains(cobraOut.String(), "Error: accepts 1 arg(s), received 0") || !strings.Contains(cobraOut.String(), "Usage:") {
			t.Errorf("cobra output = %q, want its error and the usage", cobraOut.String())
		}
		if got, want := stderr.String(), "Error: accepts 1 arg(s), received 0\n"; got != want {
			t.Errorf("stderr = %q, want %q", got, want)
		}
	})
}
