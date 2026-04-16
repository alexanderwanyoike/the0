//go:build integration

// Integration tests for `the0 dev`. Each test creates a throwaway bot project
// in t.TempDir(), runs it through the real dispatcher + runner, and asserts
// that at least one metric event was captured. The bots emit raw SDK-format
// JSON on stdout (no SDK dependency) so only the language toolchain is needed.
//
// Run with:  go test -tags integration ./internal/dev/ -v -timeout 120s
//
// Each test skips gracefully if the toolchain is not on PATH.
//
// Uses the external test package (dev_test) to avoid an import cycle:
// dev/runtime imports dev, so a test in package dev cannot also import
// dev/runtime.
package dev_test

import (
	"context"
	"encoding/json"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	"the0/internal/dev"
	"the0/internal/dev/runtime"
)

type collectSink struct {
	mu     sync.Mutex
	events []dev.Event
}

func (c *collectSink) Emit(ev dev.Event) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.events = append(c.events, ev)
}

func (c *collectSink) get() []dev.Event {
	c.mu.Lock()
	defer c.mu.Unlock()
	out := make([]dev.Event, len(c.events))
	copy(out, c.events)
	return out
}

func requireTool(t *testing.T, name string) {
	t.Helper()
	if _, err := exec.LookPath(name); err != nil {
		t.Skipf("toolchain %q not on PATH, skipping", name)
	}
}

func writeFile(t *testing.T, dir, name, content string) {
	t.Helper()
	path := filepath.Join(dir, name)
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
		t.Fatal(err)
	}
}

func runBot(t *testing.T, rt dev.Runtime, dir string) []dev.Event {
	t.Helper()
	devRoot := filepath.Join(dir, ".the0", "dev", "test")
	codeDir := filepath.Join(devRoot, "code")
	stateDir := filepath.Join(devRoot, "state")
	for _, d := range []string{codeDir, stateDir} {
		if err := os.MkdirAll(d, 0o755); err != nil {
			t.Fatal(err)
		}
	}

	config, _ := json.Marshal(map[string]string{"name": "integration-test"})
	opts := runtime.Opts{
		Mode:      runtime.ModeNative,
		Cwd:       dir,
		Script:    rt.DefaultScript(),
		BotID:     "integration-test",
		BotConfig: config,
		CodeDir:   codeDir,
		StateDir:  stateDir,
	}

	var spec *dev.RunSpec
	var err error
	switch rt {
	case dev.RuntimePython:
		spec, err = runtime.Python(opts)
	case dev.RuntimeNode:
		spec, err = runtime.Node(opts)
	case dev.RuntimeRust:
		spec, err = runtime.Rust(opts)
	case dev.RuntimeDotnet:
		spec, err = runtime.Dotnet(opts)
	case dev.RuntimeCpp:
		spec, err = runtime.Cpp(opts)
	case dev.RuntimeScala:
		spec, err = runtime.Scala(opts)
	case dev.RuntimeHaskell:
		spec, err = runtime.Haskell(opts)
	default:
		t.Fatalf("unsupported runtime %q", rt)
	}
	if err != nil {
		t.Fatalf("dispatch: %v", err)
	}

	sink := &collectSink{}
	runner := dev.NewRunner(spec, sink)
	ctx, cancel := context.WithTimeout(context.Background(), 60*time.Second)
	defer cancel()

	exitCode, runErr := runner.Run(ctx)
	if runErr != nil {
		t.Fatalf("runner: %v", runErr)
	}
	if exitCode != 0 {
		events := sink.get()
		var stderr []string
		for _, ev := range events {
			if ev.Stream == dev.StreamStderr {
				stderr = append(stderr, ev.Raw)
			}
		}
		t.Fatalf("exit %d; stderr:\n%s", exitCode, strings.Join(stderr, "\n"))
	}
	return sink.get()
}

func assertHasMetric(t *testing.T, events []dev.Event, metricType string) {
	t.Helper()
	for _, ev := range events {
		if ev.Kind == dev.EventMetric && ev.MetricType == metricType {
			return
		}
	}
	t.Errorf("no metric %q found in %d events", metricType, len(events))
}

// --- Python ---

func TestIntegration_Python(t *testing.T) {
	requireTool(t, "python3")
	dir := t.TempDir()
	writeFile(t, dir, "main.py", `
import json, sys

def main(bot_id, config):
    print(json.dumps({"_metric": "heartbeat", "bot_id": bot_id, "alive": True}))
    print(json.dumps({"level": "INFO", "message": "python integration test"}), file=sys.stderr)

if __name__ == "__main__":
    import os
    bot_id = os.environ.get("BOT_ID", "unknown")
    main(bot_id, {})
`)
	writeFile(t, dir, "config.json", `{"name":"py-integration"}`)

	events := runBot(t, dev.RuntimePython, dir)
	assertHasMetric(t, events, "heartbeat")
}

// --- Node ---

func TestIntegration_Node(t *testing.T) {
	requireTool(t, "node")
	dir := t.TempDir()
	writeFile(t, dir, "main.js", `
const botId = process.env.BOT_ID || "unknown";
console.log(JSON.stringify({_metric: "heartbeat", bot_id: botId, alive: true}));
console.error(JSON.stringify({level: "INFO", message: "node integration test"}));
`)
	writeFile(t, dir, "config.json", `{"name":"node-integration"}`)

	events := runBot(t, dev.RuntimeNode, dir)
	assertHasMetric(t, events, "heartbeat")
}

// --- Rust ---

func TestIntegration_Rust(t *testing.T) {
	requireTool(t, "cargo")
	dir := t.TempDir()
	writeFile(t, dir, "Cargo.toml", `
[package]
name = "integration-test"
version = "0.1.0"
edition = "2021"
`)
	writeFile(t, dir, "src/main.rs", `
fn main() {
    let bot_id = std::env::var("BOT_ID").unwrap_or_else(|_| "unknown".to_string());
    println!(r#"{{"_metric":"heartbeat","bot_id":"{}","alive":true}}"#, bot_id);
    eprintln!(r#"{{"level":"INFO","message":"rust integration test"}}"#);
}
`)
	writeFile(t, dir, "config.json", `{"name":"rust-integration"}`)

	events := runBot(t, dev.RuntimeRust, dir)
	assertHasMetric(t, events, "heartbeat")
}

// --- .NET ---

func TestIntegration_Dotnet(t *testing.T) {
	requireTool(t, "dotnet")
	dir := t.TempDir()
	writeFile(t, dir, "integration-test.csproj", `
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net8.0</TargetFramework>
    <ImplicitUsings>enable</ImplicitUsings>
  </PropertyGroup>
</Project>
`)
	writeFile(t, dir, "Program.cs", `
var botId = Environment.GetEnvironmentVariable("BOT_ID") ?? "unknown";
Console.WriteLine($@"{{""_metric"":""heartbeat"",""bot_id"":""{botId}"",""alive"":true}}");
Console.Error.WriteLine(@"{""level"":""INFO"",""message"":""dotnet integration test""}");
`)
	writeFile(t, dir, "config.json", `{"name":"dotnet-integration"}`)

	events := runBot(t, dev.RuntimeDotnet, dir)
	assertHasMetric(t, events, "heartbeat")
}

// --- C++ ---

func TestIntegration_Cpp(t *testing.T) {
	requireTool(t, "make")
	requireTool(t, "g++")
	dir := t.TempDir()
	writeFile(t, dir, "main.cpp", `
#include <cstdlib>
#include <iostream>
#include <string>
int main() {
    std::string bot_id = "unknown";
    if (auto* e = std::getenv("BOT_ID")) bot_id = e;
    std::cout << R"({"_metric":"heartbeat","bot_id":")" << bot_id << R"(","alive":true})" << std::endl;
    std::cerr << R"({"level":"INFO","message":"cpp integration test"})" << std::endl;
    return 0;
}
`)
	writeFile(t, dir, "Makefile", "run:\n\tg++ -o bot main.cpp && ./bot\n")
	writeFile(t, dir, "config.json", `{"name":"cpp-integration"}`)

	events := runBot(t, dev.RuntimeCpp, dir)
	assertHasMetric(t, events, "heartbeat")
}

// --- Scala ---

func TestIntegration_Scala(t *testing.T) {
	requireTool(t, "sbt")
	// Scala first-run is 10-40s for JVM + dep resolution. Give it headroom.
	if testing.Short() {
		t.Skip("scala integration is slow; skipping in -short mode")
	}
	dir := t.TempDir()
	writeFile(t, dir, "build.sbt", `
name := "integration-test"
version := "0.1.0"
scalaVersion := "3.3.1"
`)
	writeFile(t, dir, "src/main/scala/Main.scala", `
@main def run(): Unit =
  val botId = sys.env.getOrElse("BOT_ID", "unknown")
  println(s"""{"_metric":"heartbeat","bot_id":"$botId","alive":true}""")
  System.err.println("""{"level":"INFO","message":"scala integration test"}""")
`)
	writeFile(t, dir, "config.json", `{"name":"scala-integration"}`)

	events := runBot(t, dev.RuntimeScala, dir)
	assertHasMetric(t, events, "heartbeat")
}

// --- Haskell ---

func TestIntegration_Haskell(t *testing.T) {
	requireTool(t, "cabal")
	dir := t.TempDir()
	writeFile(t, dir, "integration-test.cabal", `
cabal-version: 2.4
name:          integration-test
version:       0.1.0
executable integration-test
  main-is: Main.hs
  build-depends: base
  default-language: Haskell2010
`)
	writeFile(t, dir, "Main.hs", `
module Main where
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
main :: IO ()
main = do
  botId <- maybe "unknown" id <$> lookupEnv "BOT_ID"
  putStrLn $ "{\"_metric\":\"heartbeat\",\"bot_id\":\"" ++ botId ++ "\",\"alive\":true}"
  hPutStrLn stderr "{\"level\":\"INFO\",\"message\":\"haskell integration test\"}"
`)
	writeFile(t, dir, "config.json", `{"name":"haskell-integration"}`)

	events := runBot(t, dev.RuntimeHaskell, dir)
	assertHasMetric(t, events, "heartbeat")
}
