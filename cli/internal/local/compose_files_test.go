package local

import (
	"regexp"
	"strings"
	"testing"

	"gopkg.in/yaml.v3"
)

type composeService struct {
	Environment map[string]string `yaml:"environment"`
	Ports       []string          `yaml:"ports"`
	Healthcheck struct {
		Test []string `yaml:"test"`
	} `yaml:"healthcheck"`
}

var composeVariable = regexp.MustCompile(`\$\{([A-Za-z_][A-Za-z0-9_]*)(:-([^}]*))?\}`)

// loadCompose resolves a compose file the way ExtractComposeFiles and docker
// compose would: the repo path placeholder, then ${VAR} and ${VAR:-default}
// from vars, then YAML aliases.
func loadCompose(t *testing.T, file []byte, vars map[string]string) map[string]composeService {
	t.Helper()
	text := strings.ReplaceAll(string(file), "{{REPO_PATH}}", "/repo")
	text = composeVariable.ReplaceAllStringFunc(text, func(ref string) string {
		m := composeVariable.FindStringSubmatch(ref)
		if v, ok := vars[m[1]]; ok && v != "" {
			return v
		}
		return m[3]
	})
	var doc struct {
		Services map[string]composeService `yaml:"services"`
	}
	if err := yaml.Unmarshal([]byte(text), &doc); err != nil {
		t.Fatalf("parse compose file: %v", err)
	}
	return doc.Services
}

var baseComposeFiles = map[string][]byte{
	"docker-compose.yml":          embeddedComposeFile,
	"docker-compose.prebuilt.yml": embeddedComposePrebuiltFile,
}

// The api and the runtime read these settings under different names (the
// MinIO endpoint also in a different shape), so the compose files must hand
// both sides the same value.
func TestComposeFiles_RuntimeServicesReadTheApisSettings(t *testing.T) {
	pairs := []struct {
		setting string
		api     func(env map[string]string) string
		runtime string
		readers []string
	}{
		{"NATS URL", func(env map[string]string) string { return env["NATS_URLS"] }, "NATS_URL",
			[]string{"bot-runner", "bot-scheduler"}},
		{"MinIO endpoint", func(env map[string]string) string {
			return env["MINIO_ENDPOINT"] + ":" + env["MINIO_PORT"]
		}, "MINIO_ENDPOINT", []string{"bot-runner", "bot-scheduler", "gc", "query-server"}},
		{"MinIO SSL", func(env map[string]string) string { return env["MINIO_USE_SSL"] }, "MINIO_USE_SSL",
			[]string{"bot-runner", "bot-scheduler", "gc", "query-server"}},
		{"logs bucket", func(env map[string]string) string { return env["LOG_BUCKET"] }, "MINIO_LOGS_BUCKET",
			[]string{"bot-runner", "bot-scheduler", "gc"}},
		{"code bucket", func(env map[string]string) string { return env["CUSTOM_BOTS_BUCKET"] }, "MINIO_CODE_BUCKET",
			[]string{"bot-runner", "bot-scheduler", "query-server"}},
	}

	for name, file := range baseComposeFiles {
		services := loadCompose(t, file, nil)
		api := services["the0-api"].Environment
		for _, pair := range pairs {
			want := pair.api(api)
			for _, svc := range pair.readers {
				got, ok := services[svc].Environment[pair.runtime]
				if !ok {
					t.Errorf("%s: %s does not set %s", name, svc, pair.runtime)
					continue
				}
				if got != want {
					t.Errorf("%s: %s %s = %q, the api's %s is %q", name, svc, pair.runtime, got, pair.setting, want)
				}
			}
		}
	}
}

// YAML aliases do not reach across files, so the dev overlay leaves the
// shared settings to the base file rather than repeating them. NATS_URL is
// the runtime's name, which the api never reads. The overlay's MinIO host is
// deliberately different (the api reaches MinIO through the host).
func TestComposeDevOverlay_LeavesSharedSettingsToTheBaseFile(t *testing.T) {
	api := loadCompose(t, embeddedComposeDevFile, nil)["the0-api"].Environment
	for _, name := range []string{"NATS_URLS", "NATS_URL", "LOG_BUCKET", "CUSTOM_BOTS_BUCKET", "RUNTIME_QUERY_URL"} {
		if value, ok := api[name]; ok {
			t.Errorf("docker-compose.dev.yml sets the api's %s to %q; leave it to docker-compose.yml", name, value)
		}
	}
}

func TestComposeFiles_QueryPortAgrees(t *testing.T) {
	for name, file := range baseComposeFiles {
		services := loadCompose(t, file, nil)
		assertQueryPort(t, name, services, "9477")
	}
}

func assertQueryPort(t *testing.T, name string, services map[string]composeService, port string) {
	t.Helper()
	query := services["query-server"]
	if got := query.Environment["QUERY_SERVER_PORT"]; got != port {
		t.Errorf("%s: query-server QUERY_SERVER_PORT = %q, want %q", name, got, port)
	}
	if !containsString(query.Ports, port+":"+port) {
		t.Errorf("%s: query-server ports %v do not publish %s", name, query.Ports, port)
	}
	if health := strings.Join(query.Healthcheck.Test, " "); !strings.Contains(health, "localhost:"+port+"/") {
		t.Errorf("%s: query-server healthcheck %q does not probe port %s", name, health, port)
	}
	want := "http://query-server:" + port
	if got := services["the0-api"].Environment["RUNTIME_QUERY_URL"]; got != want {
		t.Errorf("%s: api RUNTIME_QUERY_URL = %q, want %q", name, got, want)
	}
}

func containsString(values []string, want string) bool {
	for _, v := range values {
		if v == want {
			return true
		}
	}
	return false
}
