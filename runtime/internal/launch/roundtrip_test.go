package launch_test

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	corev1 "k8s.io/api/core/v1"

	"runtime/internal/docker"
	"runtime/internal/k8s/podgen"
	"runtime/internal/launch"
	"runtime/internal/model"
)

// The round trip: each backend launches the same bot in each mode, and
// `runtime execute` in the container must read back exactly the settings the
// launch meant to send.

var roundTripBot = model.Bot{
	ID:     "bot-1",
	Config: map[string]interface{}{"symbol": "BTC/USD"},
	CustomBotVersion: model.CustomBotVersion{
		Version:  "1.0.0",
		FilePath: "acme/1.0.0/code.zip",
		Config: model.APIBotConfig{
			Name:        "acme",
			Runtime:     "python3.11",
			Entrypoints: map[string]string{"bot": "main.py", "query": "query.py"},
		},
	},
}

const (
	roundTripQueryPath = "/portfolio"
	roundTripResultKey = "bot-1/1-abc/result.json"
	roundTripNATSURL   = "nats://nats:4222"
	roundTripImage     = "the0/runtime:test"
)

var roundTripQueryParams = map[string]interface{}{"limit": 10}

// Both backends are configured with the same MinIO and buckets.
var (
	dockerConfig = &docker.DockerRunnerConfig{
		MinIOEndpoint:           "localhost:9000",
		MinIOContainerEndpoint:  "minio:9000",
		MinIOAccessKeyID:        "access",
		MinIOSecretAccessKey:    "secret",
		MinIOUseSSL:             true,
		MinIOCodeBucket:         "acme-code",
		MinioStateBucket:        "acme-state",
		MinioLogsBucket:         "acme-logs",
		MinIOQueryResultsBucket: "acme-results",
		MaxStateSizeMB:          4096,
		MaxStateFileSizeMB:      20,
		NatsURL:                 roundTripNATSURL,
	}
	podConfig = podgen.PodGeneratorConfig{
		Namespace: "the0",
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
		NATSURL:      roundTripNATSURL,
		RuntimeImage: roundTripImage,
	}
)

// launchOf is the launch of roundTripBot in mode, whichever backend starts it.
func launchOf(mode launch.Mode, botType string) launch.Settings {
	s := launch.Settings{
		Mode:            mode,
		BotID:           "bot-1",
		CodeFile:        "acme/1.0.0/code.zip",
		Runtime:         "python3.11",
		Entrypoint:      "main.py",
		QueryEntrypoint: "query.py",
		BotConfig:       `{"symbol":"BTC/USD"}`,
		BotType:         botType,
		StateDir:        launch.DefaultStateDir,
		Storage: launch.Storage{
			Endpoint:  "minio:9000",
			AccessKey: "access",
			SecretKey: "secret",
			UseSSL:    true,
		},
		NATSURL: roundTripNATSURL,
	}
	if mode == launch.Query {
		s.Entrypoint = "query.py"
		s.QueryPath = roundTripQueryPath
		s.QueryParams = `{"limit":10}`
		s.QueryResultKey = roundTripResultKey
		// A query is neither a realtime nor a scheduled run, and saves nothing.
		s.BotType = ""
		s.NATSURL = ""
	}
	return s
}

// inDocker is a launch as the Docker runner sends it, with every bucket and
// state limit the runner is configured with.
func inDocker(s launch.Settings) launch.Settings {
	s.Storage.CodeBucket = "acme-code"
	s.Storage.StateBucket = "acme-state"
	s.Storage.LogsBucket = "acme-logs"
	s.Storage.QueryResultsBucket = "acme-results"
	s.Storage.MaxStateSizeMB = 4096
	s.Storage.MaxStateFileSizeMB = 20
	return s
}

// inPod is a launch as a pod sends it, which spells out the container paths
// and carries every bucket the controller is configured with.
func inPod(s launch.Settings, skipSync, skipQueryServer bool) launch.Settings {
	s.Storage.CodeBucket = "acme-code"
	s.Storage.StateBucket = "acme-state"
	s.Storage.LogsBucket = "acme-logs"
	s.Storage.QueryResultsBucket = "acme-results"
	s.CodePath = "/bot"
	s.StatePath = "/state"
	s.LogsPath = "/var/the0/logs"
	s.SkipSync = skipSync
	s.SkipQueryServer = skipQueryServer
	return s
}

// dockerExecutable is the executable BotService (realtime) and
// ScheduleService (scheduled) start roundTripBot with.
func dockerExecutable(isLongRunning bool) model.Executable {
	return model.Executable{
		ID:              roundTripBot.ID,
		Runtime:         roundTripBot.CustomBotVersion.Config.Runtime,
		Entrypoint:      "bot",
		EntrypointFiles: roundTripBot.CustomBotVersion.Config.Entrypoints,
		Config:          roundTripBot.Config,
		FilePath:        roundTripBot.CustomBotVersion.FilePath,
		IsLongRunning:   isLongRunning,
	}
}

// dockerQueryExecutable is the executable QueryHandler starts a query with.
func dockerQueryExecutable() model.Executable {
	e := dockerExecutable(false)
	e.Entrypoint = "query"
	e.QueryPath = roundTripQueryPath
	e.QueryParams = roundTripQueryParams
	e.QueryResultKey = roundTripResultKey
	return e
}

// sent is what a launch hands a container.
type sent struct {
	command []string
	env     []launch.Var
}

func dockerSent(t *testing.T, executable model.Executable) sent {
	t.Helper()
	config, _ := docker.ContainerConfig(dockerConfig, executable, roundTripImage)
	var env []launch.Var
	for _, entry := range config.Env {
		name, value, ok := strings.Cut(entry, "=")
		require.True(t, ok, "env entry %q", entry)
		env = append(env, launch.Var{Name: name, Value: value})
	}
	return sent{command: config.Cmd, env: env}
}

func podSent(c corev1.Container) sent {
	var env []launch.Var
	for _, variable := range c.Env {
		env = append(env, launch.Var{Name: variable.Name, Value: variable.Value})
	}
	return sent{command: c.Command, env: env}
}

func container(t *testing.T, containers []corev1.Container, name string) corev1.Container {
	t.Helper()
	for _, c := range containers {
		if c.Name == name {
			return c
		}
	}
	t.Fatalf("no %s container", name)
	return corev1.Container{}
}

// Each backend also sets these for the bot process itself. `runtime execute`
// reads none of them.
var (
	dockerProcessEnv = []string{"PYTHONDONTWRITEBYTECODE", "PYTHONUNBUFFERED"}
	podProcessEnv    = []string{"PYTHONUNBUFFERED"}
)

func TestLaunch_RunnerReadsExactlyWhatEachBackendSends(t *testing.T) {
	generator := podgen.NewPodGenerator(podConfig)
	realtimePod, err := generator.GeneratePod(roundTripBot)
	require.NoError(t, err)
	scheduledPod, err := generator.GenerateScheduledPodSpec(roundTripBot)
	require.NoError(t, err)
	queryPod, err := generator.GenerateQueryPod(roundTripBot, roundTripQueryPath, roundTripQueryParams, roundTripResultKey)
	require.NoError(t, err)

	podScheduled := inPod(launchOf(launch.Run, "scheduled"), false, true)
	// A scheduled bot's pod has no query entrypoint.
	podScheduled.QueryEntrypoint = ""

	cases := []struct {
		name       string
		sent       sent
		processEnv []string
		want       launch.Settings
	}{
		{
			name:       "docker realtime run",
			sent:       dockerSent(t, dockerExecutable(true)),
			processEnv: dockerProcessEnv,
			want:       inDocker(launchOf(launch.Run, "realtime")),
		},
		{
			name:       "docker scheduled run",
			sent:       dockerSent(t, dockerExecutable(false)),
			processEnv: dockerProcessEnv,
			want:       inDocker(launchOf(launch.Run, "scheduled")),
		},
		{
			name:       "docker query",
			sent:       dockerSent(t, dockerQueryExecutable()),
			processEnv: dockerProcessEnv,
			want:       inDocker(launchOf(launch.Query, "")),
		},
		{
			name:       "kubernetes realtime run",
			sent:       podSent(container(t, realtimePod.Spec.Containers, "bot")),
			processEnv: podProcessEnv,
			want:       inPod(launchOf(launch.Run, "realtime"), true, true),
		},
		{
			name:       "kubernetes query server",
			sent:       podSent(container(t, realtimePod.Spec.Containers, "query-server")),
			processEnv: podProcessEnv,
			want:       inPod(launchOf(launch.Server, "realtime"), false, false),
		},
		{
			name:       "kubernetes scheduled run",
			sent:       podSent(container(t, scheduledPod.Containers, "bot")),
			processEnv: podProcessEnv,
			want:       podScheduled,
		},
		{
			name:       "kubernetes query",
			sent:       podSent(container(t, queryPod.Spec.Containers, "bot")),
			processEnv: podProcessEnv,
			want:       inPod(launchOf(launch.Query, ""), false, false),
		},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			assert.Equal(t, c.want.Command(), c.sent.command, "command")
			assert.Equal(t, envMap(t, c.want.Env()), without(envMap(t, c.sent.env), c.processEnv), "environment")

			got, err := read(t, c.sent.command, c.sent.env)
			require.NoError(t, err)
			assert.Equal(t, c.want.WithDefaults(), got)
		})
	}

	t.Run("a query names its query in the command on both backends", func(t *testing.T) {
		for _, s := range []sent{
			dockerSent(t, dockerQueryExecutable()),
			podSent(container(t, queryPod.Spec.Containers, "bot")),
		} {
			assert.Equal(t, []string{launch.Binary, "execute", "--query=" + roundTripQueryPath}, s.command)
			assert.NotContains(t, envMap(t, s.env), "QUERY_PATH")
		}
	})

	t.Run("a query without params sends them as {} on both backends", func(t *testing.T) {
		executable := dockerQueryExecutable()
		executable.QueryParams = nil
		pod, err := generator.GenerateQueryPod(roundTripBot, roundTripQueryPath, nil, roundTripResultKey)
		require.NoError(t, err)

		for _, s := range []sent{dockerSent(t, executable), podSent(container(t, pod.Spec.Containers, "bot"))} {
			got, err := read(t, s.command, s.env)
			require.NoError(t, err)
			assert.Equal(t, "{}", got.QueryParams)
		}
	})

	t.Run("a bot without config sends BOT_CONFIG as {} on both backends", func(t *testing.T) {
		bot := roundTripBot
		bot.Config = nil
		realtimeExecutable := dockerExecutable(true)
		realtimeExecutable.Config = nil
		scheduledExecutable := dockerExecutable(false)
		scheduledExecutable.Config = nil
		pod, err := generator.GeneratePod(bot)
		require.NoError(t, err)
		scheduled, err := generator.GenerateScheduledPodSpec(bot)
		require.NoError(t, err)

		for name, s := range map[string]sent{
			"docker realtime run":      dockerSent(t, realtimeExecutable),
			"docker scheduled run":     dockerSent(t, scheduledExecutable),
			"kubernetes realtime run":  podSent(container(t, pod.Spec.Containers, "bot")),
			"kubernetes scheduled run": podSent(container(t, scheduled.Containers, "bot")),
		} {
			got, err := read(t, s.command, s.env)
			require.NoError(t, err, name)
			assert.Equal(t, "{}", got.BotConfig, name)
		}
	})

	t.Run("kubernetes sync sidecar", func(t *testing.T) {
		want := inPod(launchOf(launch.Run, "realtime"), true, true)
		syncEnv := podSent(container(t, realtimePod.Spec.InitContainers, "sync")).env

		assert.Equal(t, envMap(t, want.SyncEnv()), envMap(t, syncEnv))
		assert.Equal(t, want.Storage.WithDefaults(), launch.ReadStorage(lookupIn(syncEnv)))
		assert.Equal(t, want.NATSURL, launch.ReadNATSURL(lookupIn(syncEnv)))
	})
}

func envMap(t *testing.T, env []launch.Var) map[string]string {
	t.Helper()
	values := make(map[string]string, len(env))
	for _, variable := range env {
		if _, twice := values[variable.Name]; twice {
			t.Errorf("%s is sent twice", variable.Name)
		}
		values[variable.Name] = variable.Value
	}
	return values
}

func without(env map[string]string, names []string) map[string]string {
	for _, name := range names {
		delete(env, name)
	}
	return env
}
