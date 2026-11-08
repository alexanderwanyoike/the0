package cmd

import (
	"os"
	"strings"
	"testing"

	"github.com/spf13/cobra"
)

func TestCustomBotDeploy_RefusesAMissingMainBeforeVendoring(t *testing.T) {
	_, errOut := captureLogger(t)
	t.Chdir(t.TempDir())
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

	err := deployBot(&cobra.Command{}, nil)

	if err == nil || !strings.HasPrefix(err.Error(), "main.py has no top-level main(bot_id, config) function") {
		t.Fatalf("error = %v, want the missing main reported", err)
	}
	if got, want := errOut.String(), "x Entrypoint check failed\n"; got != want {
		t.Errorf("printed %q, want %q", got, want)
	}
	if _, statErr := os.Stat("vendor"); !os.IsNotExist(statErr) {
		t.Errorf("vendor exists (stat error %v), want vendoring never started", statErr)
	}
}
