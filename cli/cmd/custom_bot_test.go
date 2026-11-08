package cmd

import (
	"errors"
	"os"
	"testing"

	"the0/internal"
)

func TestCustomBotDeploy_RefusesAMissingMainBeforeVendoring(t *testing.T) {
	_, errOut := captureLogger(t)
	t.Chdir(t.TempDir())
	checked := ""
	refuse := errors.New("main.py has no main(bot_id, config) function at module level")
	// If the deploy ever got past the check, it must still go nowhere: no
	// saved login, an api address nothing listens on, and no Docker on PATH
	// for vendoring.
	internal.SetConfigDir(t.TempDir())
	t.Cleanup(func() { internal.SetConfigDir("") })
	t.Setenv("THE0_API_URL", "http://127.0.0.1:1")
	t.Setenv("PATH", "")
	files := map[string]string{
		"bot-config.yaml": "name: no-main\nversion: 1.0.0\ntype: scheduled\nruntime: python3.11\n" +
			"entrypoints:\n  bot: main.py\nschema:\n  bot: bot-schema.json\nreadme: README.md\n",
		"bot-schema.json": "{}\n",
		"README.md":       "# no-main\n",
		"main.py":         "def run(bot_id, config):\n    pass\n",
		// Vendoring would start on this, so getting past the check is visible.
		"requirements.txt": "requests\n",
	}
	for name, content := range files {
		if err := os.WriteFile(name, []byte(content), 0o644); err != nil {
			t.Fatal(err)
		}
	}

	err := deployBotWith(func(config *internal.BotConfig) error {
		checked = config.Entrypoints.Bot
		return refuse
	})

	if !errors.Is(err, refuse) || checked != "main.py" {
		t.Fatalf("error = %v after checking %q, want the check's refusal of main.py", err, checked)
	}
	if got, want := errOut.String(), "x Entrypoint check failed\n"; got != want {
		t.Errorf("printed %q, want %q", got, want)
	}
	if _, statErr := os.Stat("vendor"); !os.IsNotExist(statErr) {
		t.Errorf("vendor exists (stat error %v), want vendoring never started", statErr)
	}
}
