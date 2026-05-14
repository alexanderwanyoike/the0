//go:build unix

package local

import (
	"fmt"
	"os"
	"syscall"
)

func detectDockerSocketGID() string {
	info, err := os.Stat("/var/run/docker.sock")
	if err != nil {
		return "0"
	}
	stat, ok := info.Sys().(*syscall.Stat_t)
	if !ok {
		return "0"
	}
	return fmt.Sprintf("%d", stat.Gid)
}
