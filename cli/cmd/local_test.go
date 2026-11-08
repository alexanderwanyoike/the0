package cmd

import (
	"os"
	"strings"
	"testing"
)

func TestPromptForAdminPassword_ReadsFromNonTTYStdin(t *testing.T) {
	originalStdin := os.Stdin
	reader, writer, err := os.Pipe()
	if err != nil {
		t.Fatalf("failed to create pipe: %v", err)
	}
	defer reader.Close()
	os.Stdin = reader
	defer func() {
		os.Stdin = originalStdin
	}()

	if _, err := writer.WriteString("secret123\n"); err != nil {
		t.Fatalf("failed to write password: %v", err)
	}
	if err := writer.Close(); err != nil {
		t.Fatalf("failed to close writer: %v", err)
	}

	password, err := promptForAdminPassword()
	if err != nil {
		t.Fatalf("promptForAdminPassword failed: %v", err)
	}
	if password != "secret123" {
		t.Fatalf("expected password from stdin, got %q", password)
	}
}

func TestInitRepoPath_ResolvesSourceFlag(t *testing.T) {
	tests := []struct {
		name          string
		args          []string
		wantRepoPath  string
		wantUseSource bool
	}{
		{"uses prebuilt images without --source", []string{"--email", "a@b.c"}, "", false},
		{"uses the current directory for a bare --source", []string{"--source"}, ".", true},
		{"uses the path given with --source=path", []string{"--source=/repo"}, "/repo", true},
		{"uses the path given after --source", []string{"--source", "/repo"}, "/repo", true},
		{"uses the path given after --source among other flags", []string{"--source", "/repo", "--email", "a@b.c"}, "/repo", true},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			cmd := newLocalInitCmd()
			if err := cmd.ParseFlags(tt.args); err != nil {
				t.Fatalf("failed to parse %v: %v", tt.args, err)
			}

			repoPath, useSource, err := initRepoPath(cmd, cmd.Flags().Args())
			if err != nil {
				t.Fatalf("initRepoPath(%v) failed: %v", tt.args, err)
			}
			if repoPath != tt.wantRepoPath || useSource != tt.wantUseSource {
				t.Errorf("initRepoPath(%v) = (%q, %v), want (%q, %v)", tt.args, repoPath, useSource, tt.wantRepoPath, tt.wantUseSource)
			}
		})
	}
}

func TestInitRepoPath_RefusesAStrayArgument(t *testing.T) {
	tests := []struct {
		name string
		args []string
	}{
		{"a path without --source", []string{"/repo"}},
		{"a second path after --source=path", []string{"--source=/repo", "/other"}},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			cmd := newLocalInitCmd()
			if err := cmd.ParseFlags(tt.args); err != nil {
				t.Fatalf("failed to parse %v: %v", tt.args, err)
			}

			_, _, err := initRepoPath(cmd, cmd.Flags().Args())
			if err == nil {
				t.Fatalf("initRepoPath(%v) accepted a stray argument", tt.args)
			}
			if !strings.Contains(err.Error(), "--source") {
				t.Errorf("error %q does not say how to pass the path with --source", err)
			}
		})
	}
}
