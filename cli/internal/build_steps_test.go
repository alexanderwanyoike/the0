package internal

import (
	"errors"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
)

// fakeRunner stands in for Docker: it records the steps a build asks for
// and lets a test play the container's part through onRun.
type fakeRunner struct {
	ensured []string
	steps   []BuildStep
	onRun   func(step BuildStep) error
	removed []string
}

func (f *fakeRunner) Ping() error { return nil }

func (f *fakeRunner) EnsureImage(image string) error {
	f.ensured = append(f.ensured, image)
	return nil
}

func (f *fakeRunner) Run(step BuildStep) (string, error) {
	f.steps = append(f.steps, step)
	if f.onRun != nil {
		return "ctr-1", f.onRun(step)
	}
	return "ctr-1", nil
}

func (f *fakeRunner) Remove(id string) { f.removed = append(f.removed, id) }

func (f *fakeRunner) Close() error { return nil }

func writeFile(t *testing.T, path, content string, mode os.FileMode) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte(content), mode); err != nil {
		t.Fatal(err)
	}
}

func TestRustBuild_RunsCargoInTheProject(t *testing.T) {
	t.Run("builds with cargo as the host user and finds the binary", func(t *testing.T) {
		project := t.TempDir()
		runner := &fakeRunner{onRun: func(BuildStep) error {
			writeFile(t, filepath.Join(project, "target", "release", "bot"), "binary", 0755)
			return nil
		}}

		if err := (&RustBuilder{}).Build(NewVendorManager(project, runner)); err != nil {
			t.Fatalf("Build() error = %v", err)
		}

		if !reflect.DeepEqual(runner.ensured, []string{rustImage}) {
			t.Errorf("ensured %v, want [%s]", runner.ensured, rustImage)
		}
		if len(runner.steps) != 1 {
			t.Fatalf("ran %d steps, want 1", len(runner.steps))
		}
		step := runner.steps[0]
		uid, gid := hostUserIDs()
		if step.Image != rustImage || step.WorkingDir != "/project" || step.User != uid+":"+gid {
			t.Errorf("step = %+v, want rust:latest in /project as %s:%s", step, uid, gid)
		}
		if !reflect.DeepEqual(step.Cmd, []string{"sh", "-c", "cargo build --release 2>&1"}) {
			t.Errorf("step.Cmd = %q", step.Cmd)
		}
		absProject, _ := filepath.Abs(project)
		if !reflect.DeepEqual(step.Binds, []string{absProject + ":/project"}) {
			t.Errorf("step.Binds = %q, want the project at /project", step.Binds)
		}
	})

	t.Run("fails when cargo leaves no binary", func(t *testing.T) {
		err := (&RustBuilder{}).Build(NewVendorManager(t.TempDir(), &fakeRunner{}))

		if err == nil || !strings.Contains(err.Error(), "did not produce a binary") {
			t.Errorf("Build() error = %v, want the missing binary reported", err)
		}
	})

	t.Run("reports a failed cargo run", func(t *testing.T) {
		runner := &fakeRunner{onRun: func(BuildStep) error {
			return errors.New("cargo build failed with exit code 101")
		}}

		err := (&RustBuilder{}).Build(NewVendorManager(t.TempDir(), runner))

		want := "failed to run Rust build container: cargo build failed with exit code 101"
		if err == nil || err.Error() != want {
			t.Errorf("Build() error = %v, want %q", err, want)
		}
	})
}

func TestPythonVendor_InstallsIntoVendor(t *testing.T) {
	t.Run("mounts vendor and requirements.txt and checks what was installed", func(t *testing.T) {
		project := t.TempDir()
		writeFile(t, filepath.Join(project, "requirements.txt"), "requests\n", 0644)
		runner := &fakeRunner{onRun: func(BuildStep) error {
			writeFile(t, filepath.Join(project, "vendor", "requests", "__init__.py"), "#", 0644)
			return nil
		}}

		if err := (&PythonVendor{}).Vendor(NewVendorManager(project, runner)); err != nil {
			t.Fatalf("Vendor() error = %v", err)
		}

		absProject, _ := filepath.Abs(project)
		want := []string{
			absProject + "/vendor:/vendor",
			absProject + "/requirements.txt:/requirements.txt:ro",
		}
		if len(runner.steps) != 1 || !reflect.DeepEqual(runner.steps[0].Binds, want) {
			t.Errorf("steps = %+v, want binds %q", runner.steps, want)
		}
		if runner.steps[0].Image != pythonImage {
			t.Errorf("image = %q, want %q", runner.steps[0].Image, pythonImage)
		}
	})

	t.Run("fails when pip installed nothing", func(t *testing.T) {
		project := t.TempDir()
		writeFile(t, filepath.Join(project, "requirements.txt"), "requests\n", 0644)

		err := (&PythonVendor{}).Vendor(NewVendorManager(project, &fakeRunner{}))

		if err == nil || !strings.Contains(err.Error(), "no packages were vendored") {
			t.Errorf("Vendor() error = %v, want an empty vendor directory reported", err)
		}
	})
}

func TestNodeVendor_RestoresNodeModulesWhenTheInstallFails(t *testing.T) {
	project := t.TempDir()
	writeFile(t, filepath.Join(project, "package.json"), `{"name":"bot"}`, 0644)
	writeFile(t, filepath.Join(project, "node_modules", "left-pad", "index.js"), "old", 0644)
	runner := &fakeRunner{onRun: func(BuildStep) error {
		return errors.New("npm install failed with exit code 1")
	}}

	vm := NewVendorManager(project, runner)
	vm.runtime = "nodejs22"
	err := (&NodeVendor{}).Vendor(vm)

	if err == nil || !strings.Contains(err.Error(), "npm install failed with exit code 1") {
		t.Fatalf("Vendor() error = %v, want the failed install", err)
	}
	if got, _ := os.ReadFile(filepath.Join(project, "node_modules", "left-pad", "index.js")); string(got) != "old" {
		t.Errorf("node_modules/left-pad/index.js = %q, want the backup restored", got)
	}
	if runner.steps[0].Image != nodeImageFor("nodejs22") {
		t.Errorf("image = %q, want the bot runtime's %q", runner.steps[0].Image, nodeImageFor("nodejs22"))
	}
}

func TestFrontendBuild_NeedsABundle(t *testing.T) {
	t.Run("builds in frontend and reports the bundle", func(t *testing.T) {
		project := t.TempDir()
		runner := &fakeRunner{onRun: func(BuildStep) error {
			writeFile(t, filepath.Join(project, "frontend", "dist", "bundle.js"), "export {}", 0644)
			return nil
		}}

		if err := buildFrontend(NewVendorManager(project, runner)); err != nil {
			t.Fatalf("buildFrontend() error = %v", err)
		}
		if runner.steps[0].WorkingDir != "/project/frontend" || runner.steps[0].Image != frontendNodeImage {
			t.Errorf("step = %+v, want %s in /project/frontend", runner.steps[0], frontendNodeImage)
		}
	})

	t.Run("fails when the build leaves no bundle", func(t *testing.T) {
		err := buildFrontend(NewVendorManager(t.TempDir(), &fakeRunner{}))

		if err == nil || !strings.Contains(err.Error(), "did not produce dist/bundle.js") {
			t.Errorf("buildFrontend() error = %v, want the missing bundle reported", err)
		}
	})
}
