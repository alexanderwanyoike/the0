package cmd

import (
	"fmt"
	"io"

	"github.com/spf13/cobra"
)

// Execute runs root and returns the process exit code. It is the only
// place the CLI decides how a failed command ends.
func Execute(root *cobra.Command, stderr io.Writer) int {
	if err := root.Execute(); err != nil {
		fmt.Fprintf(stderr, "Error: %v\n", err)
		return 1
	}
	return 0
}
