package local

import (
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"
)

var adminEmailPattern = regexp.MustCompile(`^[A-Za-z0-9.!#$%&'*+/=?^_{|}~-]+@[A-Za-z0-9-]+(?:\.[A-Za-z0-9-]+)+$`)

const (
	adminEmailEnvKey    = "THE0_ADMIN_EMAIL"
	adminPasswordEnvKey = "THE0_ADMIN_PASSWORD"
)

// SetAdminEmail updates THE0_ADMIN_EMAIL in the local compose .env file.
func SetAdminEmail(composeDir string, email string) error {
	return SetAdminCredentials(composeDir, email, "")
}

// SetAdminPassword updates THE0_ADMIN_PASSWORD while preserving the configured
// admin email.
func SetAdminPassword(composeDir string, password string) error {
	if err := ValidateAdminPassword(password); err != nil {
		return err
	}

	envPath := filepath.Join(composeDir, ".env")
	data, err := os.ReadFile(envPath)
	if err != nil {
		if os.IsNotExist(err) {
			return fmt.Errorf(".env not found in %s; run `the0 local init` first", composeDir)
		}
		return fmt.Errorf("failed to read .env file: %w", err)
	}

	content := string(data)
	if envValue(content, adminEmailEnvKey) == "" {
		return fmt.Errorf("THE0_ADMIN_EMAIL is not configured; run `the0 local admin set --email you@example.com` first")
	}

	content = upsertEnvValue(content, adminPasswordEnvKey, password)
	if err := os.WriteFile(envPath, []byte(content), 0600); err != nil {
		return fmt.Errorf("failed to write .env file: %w", err)
	}

	return nil
}

// SetAdminCredentials updates THE0_ADMIN_EMAIL and THE0_ADMIN_PASSWORD in the
// local compose .env file.
func SetAdminCredentials(composeDir string, email string, password string) error {
	email = strings.TrimSpace(email)
	if err := ValidateAdminEmail(email); err != nil {
		return err
	}
	if password != "" {
		if err := ValidateAdminPassword(password); err != nil {
			return err
		}
	}

	envPath := filepath.Join(composeDir, ".env")
	data, err := os.ReadFile(envPath)
	if err != nil {
		if os.IsNotExist(err) {
			return fmt.Errorf(".env not found in %s; run `the0 local init` first", composeDir)
		}
		return fmt.Errorf("failed to read .env file: %w", err)
	}

	content := upsertEnvValue(string(data), adminEmailEnvKey, email)
	if password != "" {
		content = upsertEnvValue(content, adminPasswordEnvKey, password)
	}

	if err := os.WriteFile(envPath, []byte(content), 0600); err != nil {
		return fmt.Errorf("failed to write .env file: %w", err)
	}

	return nil
}

// AdminCredentialsConfigured returns whether local compose has both root admin
// credentials set.
func AdminCredentialsConfigured(composeDir string) (bool, error) {
	envPath := filepath.Join(composeDir, ".env")
	data, err := os.ReadFile(envPath)
	if err != nil {
		if os.IsNotExist(err) {
			return false, fmt.Errorf(".env not found in %s; run `the0 local init` first", composeDir)
		}
		return false, fmt.Errorf("failed to read .env file: %w", err)
	}

	content := string(data)
	return envValue(content, adminEmailEnvKey) != "" &&
		envValue(content, adminPasswordEnvKey) != "", nil
}

// ValidateAdminEmail checks that an admin email can be safely written to .env.
func ValidateAdminEmail(email string) error {
	if email == "" {
		return fmt.Errorf("email is required")
	}
	if strings.ContainsAny(email, "\r\n=#") || !adminEmailPattern.MatchString(email) {
		return fmt.Errorf("email must be a valid single-line email address")
	}
	return nil
}

// ValidateAdminPassword checks that an admin password can be safely written to .env.
// The API owns password policy validation and fails startup when the configured
// root admin password is invalid.
func ValidateAdminPassword(password string) error {
	if password == "" {
		return fmt.Errorf("password is required")
	}
	if strings.TrimSpace(password) != password {
		return fmt.Errorf("password must not start or end with whitespace")
	}
	if strings.ContainsAny(password, "\r\n#") {
		return fmt.Errorf("password must not contain newlines or #")
	}
	return nil
}
