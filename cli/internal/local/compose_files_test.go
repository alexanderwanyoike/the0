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

var runtimeServices = []string{"bot-runner", "bot-scheduler", "gc", "query-server"}

// The api and the runtime read these settings under different names (the
// MinIO endpoint also in a different shape), so the compose files must hand
// both sides the same value.
func TestComposeFiles_RuntimeServicesReadTheApisSettings(t *testing.T) {
	pairs := []struct {
		setting string
		api     func(env map[string]string) string
		runtime string
	}{
		{"NATS URL", func(env map[string]string) string { return env["NATS_URLS"] }, "NATS_URL"},
		{"MinIO endpoint", func(env map[string]string) string {
			return env["MINIO_ENDPOINT"] + ":" + env["MINIO_PORT"]
		}, "MINIO_ENDPOINT"},
		{"MinIO SSL", func(env map[string]string) string { return env["MINIO_USE_SSL"] }, "MINIO_USE_SSL"},
		{"logs bucket", func(env map[string]string) string { return env["LOG_BUCKET"] }, "MINIO_LOGS_BUCKET"},
		{"code bucket", func(env map[string]string) string { return env["CUSTOM_BOTS_BUCKET"] }, "MINIO_CODE_BUCKET"},
	}

	for name, file := range baseComposeFiles {
		services := loadCompose(t, file, nil)
		api := services["the0-api"].Environment
		for _, pair := range pairs {
			want := pair.api(api)
			readers := 0
			for _, svc := range runtimeServices {
				got, ok := services[svc].Environment[pair.runtime]
				if !ok {
					continue
				}
				readers++
				if got != want {
					t.Errorf("%s: %s %s = %q, the api's %s is %q", name, svc, pair.runtime, got, pair.setting, want)
				}
			}
			if readers == 0 {
				t.Errorf("%s: no runtime service sets %s", name, pair.runtime)
			}
		}
	}
}

func TestComposeFiles_QueryPortAgrees(t *testing.T) {
	for name, file := range baseComposeFiles {
		services := loadCompose(t, file, nil)
		assertQueryPort(t, name, services, "9477")
	}
}

func TestComposeFiles_QueryServerPortMovesEveryQueryPort(t *testing.T) {
	for name, file := range baseComposeFiles {
		services := loadCompose(t, file, map[string]string{"QUERY_SERVER_PORT": "9999"})
		assertQueryPort(t, name, services, "9999")
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
