// Package launch is the contract between the runtime's launchers (the Docker
// runner and the Kubernetes pod generator) and `runtime execute` in the bot's
// container. It owns every launch setting: its name, its default and what it
// means in each mode. A launcher sends a Settings as its Command and Env, and
// the runner reads it back with Read.
//
// The bot's own environment, which `runtime execute` builds from these
// settings for the SDKs, is a separate, public contract
// (docs/custom-bot-development/bot-contract.md).
package launch

import (
	"encoding/json"
	"fmt"
	"path/filepath"

	"github.com/spf13/pflag"

	"runtime/internal/constants"
)

// Binary is where the runtime image keeps the runtime.
const Binary = "/app/runtime"

// Mode is what a launched container runs.
type Mode string

const (
	// Run starts the bot entrypoint, once for a scheduled bot or for as long
	// as a realtime bot runs.
	Run Mode = "run"
	// Query answers one query with the query entrypoint, then exits.
	Query Mode = "query"
	// Server runs the query entrypoint as a realtime bot's query server, in a
	// container beside the bot's.
	Server Mode = "server"
)

// The buckets a container uses when its launch names none.
const (
	DefaultCodeBucket         = "custom-bots"
	DefaultStateBucket        = "bot-state"
	DefaultLogsBucket         = "bot-logs"
	DefaultQueryResultsBucket = "query-results"
)

// DefaultStateDir is the SDK's state directory in the default state path.
const DefaultStateDir = "/state/.the0-state"

const stateDirName = ".the0-state"

// AccessKeyEnv and SecretKeyEnv carry Storage's credentials.
const (
	AccessKeyEnv = "MINIO_ACCESS_KEY"
	SecretKeyEnv = "MINIO_SECRET_KEY"
)

// Storage is the MinIO the bot's code, state, logs and query results are in.
type Storage struct {
	Endpoint  string
	AccessKey string
	SecretKey string
	UseSSL    bool

	CodeBucket         string
	StateBucket        string
	LogsBucket         string
	QueryResultsBucket string
}

// WithDefaults is s with every bucket it leaves empty set to its default.
func (s Storage) WithDefaults() Storage {
	s.CodeBucket = orDefault(s.CodeBucket, DefaultCodeBucket)
	s.StateBucket = orDefault(s.StateBucket, DefaultStateBucket)
	s.LogsBucket = orDefault(s.LogsBucket, DefaultLogsBucket)
	s.QueryResultsBucket = orDefault(s.QueryResultsBucket, DefaultQueryResultsBucket)
	return s
}

// Settings is one launch of `runtime execute`.
type Settings struct {
	// Mode is named in the command, so a launch never leaves the runner to
	// guess it from the environment.
	Mode Mode
	// SkipSync leaves saving a run's state and logs to a sync sidecar.
	SkipSync bool
	// SkipQueryServer leaves a realtime run's query server to a sidecar.
	SkipQueryServer bool

	// BotID, Runtime and Entrypoint are required in every mode.
	BotID string
	// CodeFile is the code archive's object in the code bucket. Empty means
	// the code is already in CodePath.
	CodeFile string
	// Runtime is the bot's language runtime, such as python3.11.
	Runtime string
	// Entrypoint is the file a run starts. Query launches set it to the
	// query entrypoint.
	Entrypoint string
	// QueryEntrypoint is the file queries and the query server start.
	QueryEntrypoint string
	// BotConfig is the bot's configuration, as JSON text.
	BotConfig string
	// BotType is realtime or scheduled. A realtime run with a query
	// entrypoint starts the query server.
	BotType string

	// QueryPath is the query a query launch answers. The command carries it.
	QueryPath string
	// QueryParams are its params, as JSON object text.
	QueryParams string
	// QueryResultKey is where a query launch uploads its result, in the query
	// results bucket. Empty leaves the result in the container.
	QueryResultKey string

	CodePath  string
	StatePath string
	LogsPath  string
	// StateDir is the SDK's state directory, by default in StatePath.
	StateDir string

	Storage Storage
	// NATSURL is where the sync publishes log lines live. Empty publishes none.
	NATSURL string
}

// WithDefaults is s with every setting it leaves empty that has a default set
// to that default.
func (s Settings) WithDefaults() Settings {
	s.CodePath = orDefault(s.CodePath, constants.DefaultCodePath)
	s.StatePath = orDefault(s.StatePath, constants.DefaultStatePath)
	s.LogsPath = orDefault(s.LogsPath, constants.DefaultLogsPath)
	s.StateDir = orDefault(s.StateDir, filepath.Join(s.StatePath, stateDirName))
	s.Storage = s.Storage.WithDefaults()
	return s
}

// Command is the command a container runs for s.
func (s Settings) Command() []string {
	command := []string{Binary, "execute"}
	switch s.Mode {
	case Query:
		return append(command, "--query="+s.QueryPath)
	case Server:
		return append(command, "--query-server-only")
	}
	if s.SkipSync {
		command = append(command, "--skip-sync")
	}
	if s.SkipQueryServer {
		command = append(command, "--skip-query-server")
	}
	return command
}

// QueryParams is how a query launch sends params: as JSON object text, which
// is {} when there are none.
func QueryParams(params map[string]interface{}) (string, error) {
	if len(params) == 0 {
		return "{}", nil
	}
	data, err := json.Marshal(params)
	if err != nil {
		return "", fmt.Errorf("failed to marshal query params: %w", err)
	}
	return string(data), nil
}

// Var is one environment variable.
type Var struct {
	Name  string
	Value string
}

// Env is the environment a container needs for s.
func (s Settings) Env() []Var {
	return s.env(func(variable) bool { return true })
}

// SyncEnv is the part of Env that the sync process reads.
func (s Settings) SyncEnv() []Var {
	return s.env(func(v variable) bool { return v.sync })
}

func (s Settings) env(include func(variable) bool) []Var {
	var env []Var
	for _, v := range variables {
		if !include(v) || !v.in(s.Mode) {
			continue
		}
		if value, ok := v.write(&s); ok {
			env = append(env, Var{Name: v.name, Value: value})
		}
	}
	return env
}

// Flags are the flags of `runtime execute` that a launch's command sets.
type Flags struct {
	query           string
	queryServerOnly bool
	skipSync        bool
	skipQueryServer bool
}

// BindFlags defines the launch flags on the flag set of `runtime execute`.
func BindFlags(flagSet *pflag.FlagSet) *Flags {
	flags := &Flags{}
	flagSet.StringVar(&flags.query, "query", "", "Execute a query (ephemeral mode) with the given path")
	flagSet.BoolVar(&flags.queryServerOnly, "query-server-only", false, "Run only the query server (for K8s sidecar mode)")
	flagSet.BoolVar(&flags.skipSync, "skip-sync", false, "Skip starting sync subprocess (for K8s where sync is a sidecar)")
	flagSet.BoolVar(&flags.skipQueryServer, "skip-query-server", false, "Skip starting query server subprocess (for K8s where query is a sidecar)")
	return flags
}

// Read is the launch a container was started with: the flags its command
// set and the environment lookup finds.
func Read(flags *Flags, lookup func(string) (string, bool)) (Settings, error) {
	var s Settings
	switch {
	case flags.queryServerOnly:
		s = readEnv(lookup, Server)
	case flags.query != "":
		s = readEnv(lookup, Query)
		s.QueryPath = flags.query
	default:
		s = readEnv(lookup, Run)
		s.SkipSync = flags.skipSync
		s.SkipQueryServer = flags.skipQueryServer
	}
	s = s.WithDefaults()

	for _, required := range []struct{ name, value string }{
		{"BOT_ID", s.BotID},
		{"RUNTIME", s.Runtime},
		{"ENTRYPOINT", s.Entrypoint},
	} {
		if required.value == "" {
			return Settings{}, fmt.Errorf("%s environment variable is required", required.name)
		}
	}
	return s, nil
}

// ReadStorage is the storage in the environment lookup finds.
func ReadStorage(lookup func(string) (string, bool)) Storage {
	return readEnv(lookup, Run).Storage.WithDefaults()
}

// ReadNATSURL is the NATS URL in the environment lookup finds.
func ReadNATSURL(lookup func(string) (string, bool)) string {
	return readEnv(lookup, Run).NATSURL
}

func readEnv(lookup func(string) (string, bool), mode Mode) Settings {
	s := Settings{Mode: mode}
	for _, v := range variables {
		if !v.in(mode) {
			continue
		}
		if value, ok := lookup(v.name); ok {
			v.read(&s, value)
		}
	}
	return s
}

// variable is a setting carried in an environment variable.
type variable struct {
	name string
	// sync marks the settings the sync process reads too.
	sync bool
	// modes are the modes the setting is sent in; nil is every mode.
	modes []Mode
	write func(*Settings) (string, bool)
	read  func(*Settings, string)
}

func (v variable) in(mode Mode) bool {
	if v.modes == nil {
		return true
	}
	for _, m := range v.modes {
		if m == mode {
			return true
		}
	}
	return false
}

var variables = []variable{
	text("BOT_ID", func(s *Settings) *string { return &s.BotID }),
	text("CODE_FILE", func(s *Settings) *string { return &s.CodeFile }),
	text("RUNTIME", func(s *Settings) *string { return &s.Runtime }),
	text("ENTRYPOINT", func(s *Settings) *string { return &s.Entrypoint }),
	text("BOT_CONFIG", func(s *Settings) *string { return &s.BotConfig }),
	ofRuns(text("BOT_TYPE", func(s *Settings) *string { return &s.BotType })),
	text("QUERY_ENTRYPOINT", func(s *Settings) *string { return &s.QueryEntrypoint }),
	text("QUERY_PARAMS", func(s *Settings) *string { return &s.QueryParams }),
	text("QUERY_RESULT_KEY", func(s *Settings) *string { return &s.QueryResultKey }),
	text("CODE_PATH", func(s *Settings) *string { return &s.CodePath }),
	text("STATE_PATH", func(s *Settings) *string { return &s.StatePath }),
	text("LOGS_PATH", func(s *Settings) *string { return &s.LogsPath }),
	text("STATE_DIR", func(s *Settings) *string { return &s.StateDir }),
	synced(always("MINIO_ENDPOINT", func(s *Settings) *string { return &s.Storage.Endpoint })),
	synced(always(AccessKeyEnv, func(s *Settings) *string { return &s.Storage.AccessKey })),
	synced(always(SecretKeyEnv, func(s *Settings) *string { return &s.Storage.SecretKey })),
	synced(boolean("MINIO_USE_SSL", func(s *Settings) *bool { return &s.Storage.UseSSL })),
	synced(text("MINIO_CODE_BUCKET", func(s *Settings) *string { return &s.Storage.CodeBucket })),
	synced(text("MINIO_STATE_BUCKET", func(s *Settings) *string { return &s.Storage.StateBucket })),
	synced(text("MINIO_LOGS_BUCKET", func(s *Settings) *string { return &s.Storage.LogsBucket })),
	synced(text("MINIO_QUERY_RESULTS_BUCKET", func(s *Settings) *string { return &s.Storage.QueryResultsBucket })),
	ofRuns(synced(text("NATS_URL", func(s *Settings) *string { return &s.NATSURL }))),
}

// text is sent when it is set.
func text(name string, field func(*Settings) *string) variable {
	return variable{
		name:  name,
		write: func(s *Settings) (string, bool) { return *field(s), *field(s) != "" },
		read:  func(s *Settings, value string) { *field(s) = value },
	}
}

// always is sent even when it is empty.
func always(name string, field func(*Settings) *string) variable {
	v := text(name, field)
	v.write = func(s *Settings) (string, bool) { return *field(s), true }
	return v
}

// flag is sent, as true, only when it is set.
func flag(name string, field func(*Settings) *bool) variable {
	return variable{
		name:  name,
		write: func(s *Settings) (string, bool) { return "true", *field(s) },
		read:  func(s *Settings, value string) { *field(s) = value == "true" },
	}
}

// boolean is always sent, as true or false.
func boolean(name string, field func(*Settings) *bool) variable {
	v := flag(name, field)
	v.write = func(s *Settings) (string, bool) { return fmt.Sprintf("%t", *field(s)), true }
	return v
}

func synced(v variable) variable {
	v.sync = true
	return v
}

// ofRuns is a bot run's setting. The query server, which serves beside a
// realtime run, shares the run's settings; a query is no run and gets none.
func ofRuns(v variable) variable {
	v.modes = []Mode{Run, Server}
	return v
}

func orDefault(value, fallback string) string {
	if value == "" {
		return fallback
	}
	return value
}
