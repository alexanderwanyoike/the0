package cmd

import (
	"os"
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
