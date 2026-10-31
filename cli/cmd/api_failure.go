package cmd

import (
	"errors"

	"the0/internal"
)

// failureTitle heads the error of a failed api call. When logging in again
// is what failed, that is the failure, whatever the command was doing.
func failureTitle(title string, err error) string {
	var reloginErr *internal.ReloginError
	if errors.As(err, &reloginErr) {
		return "Authentication failed"
	}
	return title
}
