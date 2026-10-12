package launch_test

import (
	"testing"

	"github.com/spf13/pflag"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"runtime/internal/launch"
)

// realtimeRun is a realtime bot's run with every setting set, the way a
// Kubernetes bot pod starts one.
func realtimeRun() launch.Settings {
	return launch.Settings{
		Mode:            launch.Run,
		SkipSync:        true,
		SkipQueryServer: true,
		BotID:           "bot-1",
		CodeFile:        "acme/1.0.0/code.zip",
		Runtime:         "python3.11",
		Entrypoint:      "main.py",
		QueryEntrypoint: "query.py",
		BotConfig:       `{"symbol":"BTC/USD"}`,
		BotType:         "realtime",
		CodePath:        "/bot",
		StatePath:       "/state",
		LogsPath:        "/var/the0/logs",
		StateDir:        "/state/.the0-state",
		Storage: launch.Storage{
			Endpoint:           "minio:9000",
			AccessKey:          "access",
			SecretKey:          "secret",
			UseSSL:             true,
			CodeBucket:         "acme-code",
			StateBucket:        "acme-state",
			LogsBucket:         "acme-logs",
			QueryResultsBucket: "acme-results",
		},
		NATSURL: "nats://nats:4222",
	}
}

func scheduledRun() launch.Settings {
	s := realtimeRun()
	s.SkipSync = false
	s.BotType = "scheduled"
	s.IsScheduled = true
	return s
}

func query() launch.Settings {
	s := realtimeRun()
	s.Mode = launch.Query
	s.SkipSync = false
	s.SkipQueryServer = false
	s.Entrypoint = "query.py"
	s.BotType = ""
	s.QueryPath = "/portfolio"
	s.QueryParams = `{"limit":10}`
	s.QueryResultKey = "bot-1/1/result.json"
	return s
}

func queryServer() launch.Settings {
	s := realtimeRun()
	s.Mode = launch.Server
	s.SkipSync = false
	s.SkipQueryServer = false
	return s
}

// read is what `runtime execute` reads from a container started with command
// and env.
func read(t *testing.T, command []string, env []launch.Var) (launch.Settings, error) {
	t.Helper()
	require.GreaterOrEqual(t, len(command), 2, "command %q", command)
	require.Equal(t, []string{launch.Binary, "execute"}, command[:2], "command %q", command)
	flagSet := pflag.NewFlagSet("execute", pflag.ContinueOnError)
	flags := launch.BindFlags(flagSet)
	require.NoError(t, flagSet.Parse(command[2:]))
	return launch.Read(flags, lookupIn(env))
}

func lookupIn(env []launch.Var) func(string) (string, bool) {
	values := make(map[string]string, len(env))
	for _, variable := range env {
		values[variable.Name] = variable.Value
	}
	return func(name string) (string, bool) {
		value, ok := values[name]
		return value, ok
	}
}

func names(env []launch.Var) []string {
	var names []string
	for _, variable := range env {
		names = append(names, variable.Name)
	}
	return names
}

func TestRead_ReadsBackEachModesLaunch(t *testing.T) {
	launches := []struct {
		name     string
		settings launch.Settings
	}{
		{"realtime run", realtimeRun()},
		{"scheduled run", scheduledRun()},
		{"query", query()},
		{"query server", queryServer()},
	}
	for _, l := range launches {
		t.Run(l.name, func(t *testing.T) {
			got, err := read(t, l.settings.Command(), l.settings.Env())
			require.NoError(t, err)
			assert.Equal(t, l.settings, got)
		})
	}
}

func TestRead_DefaultsWhatALaunchLeavesOut(t *testing.T) {
	got, err := read(t, []string{launch.Binary, "execute"}, []launch.Var{
		{Name: "BOT_ID", Value: "bot-1"},
		{Name: "RUNTIME", Value: "python3.11"},
		{Name: "ENTRYPOINT", Value: "main.py"},
	})

	require.NoError(t, err)
	assert.Equal(t, launch.Settings{
		Mode:       launch.Run,
		BotID:      "bot-1",
		Runtime:    "python3.11",
		Entrypoint: "main.py",
		CodePath:   "/bot",
		StatePath:  "/state",
		LogsPath:   "/var/the0/logs",
		StateDir:   launch.DefaultStateDir,
		Storage: launch.Storage{
			CodeBucket:         launch.DefaultCodeBucket,
			StateBucket:        launch.DefaultStateBucket,
			LogsBucket:         launch.DefaultLogsBucket,
			QueryResultsBucket: launch.DefaultQueryResultsBucket,
		},
	}, got)
	assert.Equal(t, "/state/.the0-state", launch.DefaultStateDir)
}

func TestRead_PutsTheDefaultStateDirInTheStatePath(t *testing.T) {
	got, err := read(t, []string{launch.Binary, "execute"}, []launch.Var{
		{Name: "BOT_ID", Value: "bot-1"},
		{Name: "RUNTIME", Value: "python3.11"},
		{Name: "ENTRYPOINT", Value: "main.py"},
		{Name: "STATE_PATH", Value: "/custom/state"},
	})

	require.NoError(t, err)
	assert.Equal(t, "/custom/state/.the0-state", got.StateDir)
}

func TestRead_RefusesALaunchWithoutBotIDRuntimeOrEntrypoint(t *testing.T) {
	for _, missing := range []string{"BOT_ID", "RUNTIME", "ENTRYPOINT"} {
		t.Run(missing, func(t *testing.T) {
			var env []launch.Var
			for _, variable := range realtimeRun().Env() {
				if variable.Name != missing {
					env = append(env, variable)
				}
			}

			_, err := read(t, realtimeRun().Command(), env)

			require.Error(t, err)
			assert.Contains(t, err.Error(), missing)
		})
	}
}

func TestRead_TakesTheModeFromTheLaunch(t *testing.T) {
	required := []launch.Var{
		{Name: "BOT_ID", Value: "bot-1"},
		{Name: "RUNTIME", Value: "python3.11"},
		{Name: "ENTRYPOINT", Value: "main.py"},
	}
	withQueryPath := append(append([]launch.Var{}, required...), launch.Var{Name: "QUERY_PATH", Value: "/from-env"})

	cases := []struct {
		name      string
		flags     []string
		env       []launch.Var
		mode      launch.Mode
		queryPath string
	}{
		{name: "nothing runs the bot", env: required, mode: launch.Run},
		{name: "--query-server-only serves queries", flags: []string{"--query-server-only"}, env: required, mode: launch.Server},
		{name: "QUERY_PATH answers that query", env: withQueryPath, mode: launch.Query, queryPath: "/from-env"},
		{name: "--query answers its query over QUERY_PATH", flags: []string{"--query=/from-flag"}, env: withQueryPath, mode: launch.Query, queryPath: "/from-flag"},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			got, err := read(t, append([]string{launch.Binary, "execute"}, c.flags...), c.env)

			require.NoError(t, err)
			assert.Equal(t, c.mode, got.Mode)
			assert.Equal(t, c.queryPath, got.QueryPath)
		})
	}
}

func TestCommand_SkipsWhatRunsInASidecar(t *testing.T) {
	assert.Equal(t, []string{launch.Binary, "execute", "--skip-sync", "--skip-query-server"}, realtimeRun().Command())
	assert.Equal(t, []string{launch.Binary, "execute", "--skip-query-server"}, scheduledRun().Command())
	assert.Equal(t, []string{launch.Binary, "execute", "--query-server-only"}, queryServer().Command())
	assert.Equal(t, []string{launch.Binary, "execute"}, launch.Settings{Mode: launch.Run}.Command())
}

func TestEnv_SendsIsScheduledOnlyForAScheduledRun(t *testing.T) {
	assert.NotContains(t, names(realtimeRun().Env()), "IS_SCHEDULED")
	assert.Contains(t, scheduledRun().Env(), launch.Var{Name: "IS_SCHEDULED", Value: "true"})
}

func TestEnv_LeavesOutEmptySettingsButNotTheStorageConnection(t *testing.T) {
	env := launch.Settings{Mode: launch.Run, BotID: "bot-1"}.Env()

	assert.Equal(t, []launch.Var{
		{Name: "BOT_ID", Value: "bot-1"},
		{Name: "MINIO_ENDPOINT", Value: ""},
		{Name: "MINIO_ACCESS_KEY", Value: ""},
		{Name: "MINIO_SECRET_KEY", Value: ""},
		{Name: "MINIO_USE_SSL", Value: "false"},
	}, env)
}

func TestSyncEnv_IsTheStorageAndNATSTheSyncProcessReads(t *testing.T) {
	sent := realtimeRun()

	env := sent.SyncEnv()

	assert.ElementsMatch(t, []string{
		"MINIO_ENDPOINT", "MINIO_ACCESS_KEY", "MINIO_SECRET_KEY", "MINIO_USE_SSL",
		"MINIO_CODE_BUCKET", "MINIO_STATE_BUCKET", "MINIO_LOGS_BUCKET", "MINIO_QUERY_RESULTS_BUCKET",
		"NATS_URL",
	}, names(env))
	assert.Equal(t, sent.Storage, launch.ReadStorage(lookupIn(env)))
	assert.Equal(t, sent.NATSURL, launch.ReadNATSURL(lookupIn(env)))
}

func TestReadStorage_DefaultsTheBuckets(t *testing.T) {
	got := launch.ReadStorage(lookupIn([]launch.Var{
		{Name: "MINIO_ENDPOINT", Value: "minio:9000"},
		{Name: "MINIO_STATE_BUCKET", Value: ""},
	}))

	assert.Equal(t, launch.Storage{
		Endpoint:           "minio:9000",
		CodeBucket:         launch.DefaultCodeBucket,
		StateBucket:        launch.DefaultStateBucket,
		LogsBucket:         launch.DefaultLogsBucket,
		QueryResultsBucket: launch.DefaultQueryResultsBucket,
	}, got)
	assert.Equal(t, []string{"custom-bots", "bot-state", "bot-logs", "query-results"}, []string{
		launch.DefaultCodeBucket, launch.DefaultStateBucket, launch.DefaultLogsBucket, launch.DefaultQueryResultsBucket,
	})
}
