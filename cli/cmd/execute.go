package cmd

import (
	"errors"
	"fmt"
	"io"

	"github.com/spf13/cobra"
	"the0/internal/logger"
)

// commandError marks an error a command's RunE returned, as opposed to one
// cobra raised before running it (wrong arguments, unknown flags).
type commandError struct {
	err error
}

func (e *commandError) Error() string { return e.err.Error() }
func (e *commandError) Unwrap() error { return e.err }

// Execute runs root and returns the process exit code. It is the only
// place the CLI decides how a failed command ends.
func Execute(root *cobra.Command, stderr io.Writer) int {
	markCommandErrors(root)

	err := root.Execute()
	if err == nil {
		return 0
	}

	var cmdErr *commandError
	if errors.As(err, &cmdErr) {
		logger.Error("%v", cmdErr.err)
		return 1
	}
	fmt.Fprintf(stderr, "Error: %v\n", err)
	return 1
}

// markCommandErrors wraps every RunE in the tree so its errors are told
// apart from cobra's own, and silences cobra's report and usage for them.
// Silencing has to happen once the command has run: set up front, it
// would also hide the usage that wrong arguments should show.
func markCommandErrors(c *cobra.Command) {
	if run := c.RunE; run != nil {
		c.RunE = func(cmd *cobra.Command, args []string) error {
			err := run(cmd, args)
			if err == nil {
				return nil
			}
			cmd.SilenceErrors = true
			cmd.SilenceUsage = true
			return &commandError{err: err}
		}
	}
	for _, sub := range c.Commands() {
		markCommandErrors(sub)
	}
}
