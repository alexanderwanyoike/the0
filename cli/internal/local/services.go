package local

import (
	"fmt"
	"sort"
	"strings"

	"the0/internal/logger"
)

// ServiceInfo describes a service in the compose stack
type ServiceInfo struct {
	ComposeService string
	FriendlyName   string
	Port           int
	URL            string
	Category       string // "infra", "app", "runtime", "dev"
}

// ServiceRegistry maps friendly names to service info
var ServiceRegistry = map[string]ServiceInfo{
	"api": {
		ComposeService: "the0-api",
		FriendlyName:   "API",
		Port:           3000,
		URL:            "http://localhost:3000",
		Category:       "app",
	},
	"frontend": {
		ComposeService: "the0-frontend",
		FriendlyName:   "Frontend",
		Port:           3001,
		URL:            "http://localhost:3001",
		Category:       "app",
	},
	"docs": {
		ComposeService: "the0-docs",
		FriendlyName:   "Documentation",
		Port:           3002,
		URL:            "http://localhost:3002",
		Category:       "app",
	},
	"postgres": {
		ComposeService: "postgres",
		FriendlyName:   "PostgreSQL",
		Port:           5432,
		URL:            "postgresql://the0:the0_password@localhost:5432/the0_oss",
		Category:       "infra",
	},
	"mongo": {
		ComposeService: "mongo",
		FriendlyName:   "MongoDB",
		Port:           27017,
		URL:            "mongodb://root:the0_mongo_password@localhost:27017",
		Category:       "infra",
	},
	"nats": {
		ComposeService: "nats",
		FriendlyName:   "NATS",
		Port:           4222,
		URL:            "nats://localhost:4222",
		Category:       "infra",
	},
	"minio": {
		ComposeService: "minio",
		FriendlyName:   "MinIO Console",
		Port:           9001,
		URL:            "http://localhost:9001",
		Category:       "infra",
	},
	"runner": {
		ComposeService: "bot-runner",
		FriendlyName:   "Bot Runner",
		Port:           8080,
		URL:            "http://localhost:8080",
		Category:       "runtime",
	},
	"scheduler": {
		ComposeService: "bot-scheduler",
		FriendlyName:   "Bot Scheduler",
		Port:           8082,
		URL:            "http://localhost:8082",
		Category:       "runtime",
	},
	"query": {
		ComposeService: "query-server",
		FriendlyName:   "Query Server",
		Port:           9477,
		URL:            "http://localhost:9477",
		Category:       "runtime",
	},
}

// ResolveFriendlyName maps a user-provided name to the compose service name.
// Accepts both friendly names ("api") and compose service names ("the0-api").
func ResolveFriendlyName(name string) (string, error) {
	name = strings.ToLower(strings.TrimSpace(name))

	// Direct match on friendly name
	if svc, ok := ServiceRegistry[name]; ok {
		return svc.ComposeService, nil
	}

	// Match on compose service name
	for _, svc := range ServiceRegistry {
		if svc.ComposeService == name {
			return svc.ComposeService, nil
		}
	}

	// List available names
	available := make([]string, 0, len(ServiceRegistry))
	for k := range ServiceRegistry {
		available = append(available, k)
	}

	return "", fmt.Errorf("unknown service %q (available: %s)", name, strings.Join(available, ", "))
}

// PrintServiceURLs prints the access URLs after services start
func PrintServiceURLs() {
	logger.Newline()
	logger.Info("Services are starting up. Access them at:")
	logger.Newline()

	categories := []struct {
		Name     string
		Category string
	}{
		{"Applications", "app"},
		{"Infrastructure", "infra"},
		{"Runtime", "runtime"},
	}

	for _, cat := range categories {
		logger.Printf("  %s:\n", cat.Name)
		// Collect and sort services in this category for deterministic output
		var names []string
		for name, svc := range ServiceRegistry {
			if svc.Category == cat.Category {
				names = append(names, name)
			}
		}
		sort.Strings(names)
		for _, name := range names {
			svc := ServiceRegistry[name]
			logger.Printf("    %-20s %s\n", svc.FriendlyName, svc.URL)
		}
		logger.Newline()
	}
}
