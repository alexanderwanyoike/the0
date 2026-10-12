/**
 * Runs every bot contract case in sdk/contract/cases against this SDK.
 *
 * The case format and the matching rules are in sdk/contract/README.md.
 */

import { spawn, spawnSync } from "child_process";
import * as fs from "fs";
import * as http from "http";
import * as net from "net";
import * as os from "os";
import * as path from "path";
import ts from "typescript";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

const SRC_DIR = __dirname;
const CASES_DIR = path.join(SRC_DIR, "..", "..", "contract", "cases");

const RUN_TIMEOUT_MS = 60_000;
const SERVER_START_TIMEOUT_MS = 30_000;
// sdk/contract/README.md, step 5, says why a server case may need a fresh port.
const PORT_ATTEMPTS = 3;

// Cases this SDK does not pass yet, each with the reason. A listed case that
// starts passing fails the suite, so the list cannot go stale.
const KNOWN_FAILURES: Record<string, string> = {};

const TIMESTAMP = /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(\.\d+)?Z$/;

type Json = unknown;
type JsonObject = Record<string, Json>;

interface ContractRequest {
  method: string;
  target: string;
  body?: Json;
  raw_body?: string;
  content_type?: string;
  expect: { status: number; headers?: JsonObject; body: Json };
}

interface ContractCase {
  entrypoint: "bot" | "query";
  env: Record<string, string>;
  state_before?: JsonObject;
  requests?: ContractRequest[];
  expect?: {
    exit_code?: Json;
    stdout?: Json[];
    stderr?: Json[];
    result?: Json;
    query_result?: Json;
    state_after?: JsonObject;
  };
}

interface Output {
  exitCode: number | null;
  stdout: string;
  stderr: string;
}

interface Run {
  out: Output;
  // Mismatches found while the process ran, such as HTTP responses.
  problems: string[];
  exitedBeforeServing: boolean;
}

function casePaths(): string[] {
  return fs
    .readdirSync(CASES_DIR)
    .filter((name) => name.endsWith(".json"))
    .sort()
    .map((name) => path.join(CASES_DIR, name));
}

function caseName(file: string): string {
  return path.basename(file, ".json");
}

function isObject(value: Json): value is JsonObject {
  return value !== null && typeof value === "object" && !Array.isArray(value);
}

function show(value: Json): string {
  return value === undefined ? "nothing" : JSON.stringify(value);
}

/** Every way actual fails to match expected, as readable lines. */
function mismatches(expected: Json, actual: Json, where: string): string[] {
  if (isObject(expected) && "$match" in expected) {
    return matcherMismatches(expected, actual, where);
  }
  if (isObject(expected)) {
    if (!isObject(actual)) {
      return [`${where}: expected an object, got ${show(actual)}`];
    }
    const problems: string[] = [];
    for (const key of Object.keys(expected)) {
      if (!(key in actual)) {
        problems.push(`${where}.${key}: missing`);
      } else {
        problems.push(...mismatches(expected[key], actual[key], `${where}.${key}`));
      }
    }
    for (const key of Object.keys(actual)) {
      if (!(key in expected)) {
        problems.push(`${where}.${key}: unexpected, got ${show(actual[key])}`);
      }
    }
    return problems;
  }
  if (Array.isArray(expected)) {
    if (!Array.isArray(actual) || actual.length !== expected.length) {
      return [`${where}: expected ${show(expected)}, got ${show(actual)}`];
    }
    return expected.flatMap((item, index) => mismatches(item, actual[index], `${where}[${index}]`));
  }
  if (expected === actual) {
    return [];
  }
  return [`${where}: expected ${show(expected)}, got ${show(actual)}`];
}

function matcherMismatches(expected: JsonObject, actual: Json, where: string): string[] {
  let ok: boolean;
  switch (expected.$match) {
    case "timestamp":
      ok = typeof actual === "string" && TIMESTAMP.test(actual);
      break;
    case "string":
      ok = typeof actual === "string";
      break;
    case "contains":
      ok = typeof actual === "string" && actual.includes(expected.value as string);
      break;
    case "nonzero":
      ok = typeof actual === "number" && actual !== 0;
      break;
    case "unordered": {
      const items = expected.items as Json[];
      ok = Array.isArray(actual) && actual.length === items.length;
      const remaining = ok ? [...(actual as Json[])] : [];
      for (const item of ok ? items : []) {
        const found = remaining.findIndex((got) => mismatches(item, got, where).length === 0);
        if (found === -1) {
          ok = false;
          break;
        }
        remaining.splice(found, 1);
      }
      break;
    }
    default:
      return [`${where}: unknown matcher ${show(expected.$match)}`];
  }
  return ok ? [] : [`${where}: expected ${show(expected)}, got ${show(actual)}`];
}

function lineMismatches(expected: Json[], text: string, stream: string): string[] {
  const lines = text.split("\n").filter((line) => line.trim() !== "");
  if (lines.length !== expected.length) {
    return [`${stream}: expected ${expected.length} lines, got ${lines.length}`];
  }
  return lines.flatMap((line, index) => {
    let parsed: Json;
    try {
      parsed = JSON.parse(line);
    } catch {
      return [`${stream}[${index}]: not JSON: ${line}`];
    }
    return mismatches(expected[index], parsed, `${stream}[${index}]`);
  });
}

function fileMismatches(expected: Json, file: string, name: string): string[] {
  if (expected === null) {
    return fs.existsSync(file) ? [`${name}: expected no file, found one`] : [];
  }
  if (!fs.existsSync(file)) {
    return [`${name}: no file written`];
  }
  try {
    return mismatches(expected, JSON.parse(fs.readFileSync(file, "utf8")), name);
  } catch (e) {
    return [`${name}: not JSON: ${e}`];
  }
}

function readState(stateDir: string): JsonObject {
  if (!fs.existsSync(stateDir)) {
    return {};
  }
  const state: JsonObject = {};
  for (const name of fs.readdirSync(stateDir).filter((file) => file.endsWith(".json"))) {
    state[path.basename(name, ".json")] = JSON.parse(fs.readFileSync(path.join(stateDir, name), "utf8"));
  }
  return state;
}

function freePort(): Promise<number> {
  return new Promise((resolve, reject) => {
    const server = net.createServer();
    server.once("error", reject);
    server.listen(0, "127.0.0.1", () => {
      const { port } = server.address() as net.AddressInfo;
      server.close(() => resolve(port));
    });
  });
}

function canConnect(port: number): Promise<boolean> {
  return new Promise((resolve) => {
    const socket = net.connect(port, "127.0.0.1");
    socket.once("connect", () => {
      socket.destroy();
      resolve(true);
    });
    socket.once("error", () => resolve(false));
  });
}

interface Response {
  status: number;
  headers: http.IncomingHttpHeaders;
  body: string;
}

function send(port: number, request: ContractRequest): Promise<Response> {
  let body: string | undefined;
  const headers: Record<string, string> = {};
  if (request.body !== undefined) {
    body = JSON.stringify(request.body);
    headers["Content-Type"] = "application/json";
  } else if (request.raw_body !== undefined) {
    body = request.raw_body;
    headers["Content-Type"] = request.content_type ?? "text/plain";
  }
  if (body !== undefined) {
    headers["Content-Length"] = String(Buffer.byteLength(body));
  }
  return new Promise((resolve, reject) => {
    const req = http.request(
      { host: "127.0.0.1", port, method: request.method, path: request.target, headers, agent: false },
      (res) => {
        let text = "";
        res.setEncoding("utf8");
        res.on("data", (chunk) => (text += chunk));
        res.on("end", () => resolve({ status: res.statusCode ?? 0, headers: res.headers, body: text }));
      }
    );
    req.setTimeout(RUN_TIMEOUT_MS, () => req.destroy(new Error("request timed out")));
    req.on("error", reject);
    req.end(body);
  });
}

/** The directories the runtime gives a bot, rooted in a temporary directory. */
class Sandbox {
  readonly root = fs.mkdtempSync(path.join(os.tmpdir(), "the0-contract-"));
  readonly codeDir = path.join(this.root, "bot");
  readonly stateDir = path.join(this.root, "state", ".the0-state");
  readonly queryResult = path.join(this.root, "query", "result.json");

  constructor(contractCase: ContractCase, readonly port: number | null) {
    fs.mkdirSync(this.codeDir);
    fs.mkdirSync(path.dirname(this.queryResult));
    if (contractCase.state_before !== undefined) {
      fs.mkdirSync(this.stateDir, { recursive: true });
      for (const [key, value] of Object.entries(contractCase.state_before)) {
        fs.writeFileSync(path.join(this.stateDir, `${key}.json`), JSON.stringify(value));
      }
    }
  }

  env(contractCase: ContractCase): NodeJS.ProcessEnv {
    const placeholders: Record<string, string> = {
      "{{code_mount_dir}}": this.codeDir.replace(/^\//, ""),
      "{{state_dir}}": this.stateDir,
      "{{port}}": String(this.port),
    };
    const env: NodeJS.ProcessEnv = { PATH: process.env.PATH ?? "" };
    for (const [key, value] of Object.entries(contractCase.env)) {
      env[key] = Object.entries(placeholders).reduce(
        (text, [placeholder, actual]) => text.split(placeholder).join(actual),
        value
      );
    }
    return env;
  }

  remove(): void {
    fs.rmSync(this.root, { recursive: true, force: true });
  }
}

let buildDir: string;

/** Compiles the SDK sources and the contract bot to plain JavaScript, so tests always run the current source. */
function buildContractBot(): string {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "the0-contract-bot-"));
  const sources = fs
    .readdirSync(SRC_DIR)
    .filter((name) => name.endsWith(".ts") && !name.endsWith(".test.ts"));
  for (const source of sources) {
    const output = ts.transpileModule(fs.readFileSync(path.join(SRC_DIR, source), "utf8"), {
      compilerOptions: {
        module: ts.ModuleKind.CommonJS,
        target: ts.ScriptTarget.ES2022,
        esModuleInterop: true,
      },
      fileName: source,
    });
    fs.writeFileSync(path.join(dir, source.replace(/\.ts$/, ".js")), output.outputText);
  }
  return dir;
}

async function runCase(file: string): Promise<{ problems: string[]; output: string }> {
  const contractCase = JSON.parse(fs.readFileSync(file, "utf8")) as ContractCase;
  const servesHttp = contractCase.requests !== undefined;
  for (let attempt = 1; ; attempt++) {
    const sandbox = new Sandbox(contractCase, servesHttp ? await freePort() : null);
    try {
      const run = await runContractBot(contractCase, file, sandbox, servesHttp);
      if (run.exitedBeforeServing && attempt < PORT_ATTEMPTS) {
        continue;
      }
      const { out } = run;
      return {
        problems: [...run.problems, ...compare(contractCase, sandbox, out)],
        output: `exit code: ${out.exitCode}\n--- stdout\n${out.stdout}--- stderr\n${out.stderr}`,
      };
    } finally {
      sandbox.remove();
    }
  }
}

async function runContractBot(
  contractCase: ContractCase,
  file: string,
  sandbox: Sandbox,
  servesHttp: boolean
): Promise<Run> {
  const command = [path.join(buildDir, "contract-bot.js"), file, sandbox.queryResult];
  if (servesHttp) {
    return runServer(contractCase, sandbox, command);
  }
  const run = spawnSync(process.execPath, command, {
    env: sandbox.env(contractCase),
    encoding: "utf8",
    timeout: RUN_TIMEOUT_MS,
  });
  return {
    out: { exitCode: run.status, stdout: run.stdout, stderr: run.stderr },
    problems: [],
    exitedBeforeServing: false,
  };
}

function compare(contractCase: ContractCase, sandbox: Sandbox, out: Output): string[] {
  const problems: string[] = [];
  const expected = contractCase.expect ?? {};
  if ("exit_code" in expected) {
    problems.push(...mismatches(expected.exit_code, out.exitCode, "exit_code"));
  }
  if (expected.stdout !== undefined) {
    problems.push(...lineMismatches(expected.stdout, out.stdout, "stdout"));
  }
  if (expected.stderr !== undefined) {
    problems.push(...lineMismatches(expected.stderr, out.stderr, "stderr"));
  }
  if ("result" in expected) {
    problems.push(...fileMismatches(expected.result, path.join(sandbox.codeDir, "result.json"), "result"));
  }
  if ("query_result" in expected) {
    problems.push(...fileMismatches(expected.query_result, sandbox.queryResult, "query_result"));
  }
  if (expected.state_after !== undefined) {
    problems.push(...mismatches(expected.state_after, readState(sandbox.stateDir), "state_after"));
  }
  return problems;
}

async function runServer(
  contractCase: ContractCase,
  sandbox: Sandbox,
  command: string[]
): Promise<Run> {
  const child = spawn(process.execPath, command, { env: sandbox.env(contractCase) });
  const out: Output = { exitCode: null, stdout: "", stderr: "" };
  child.stdout.setEncoding("utf8").on("data", (chunk) => (out.stdout += chunk));
  child.stderr.setEncoding("utf8").on("data", (chunk) => (out.stderr += chunk));
  const exited = new Promise<void>((resolve) =>
    child.on("exit", (code) => {
      out.exitCode = code;
      resolve();
    })
  );
  const port = sandbox.port as number;

  const problems: string[] = [];
  let exitedBeforeServing = false;
  try {
    const deadline = Date.now() + SERVER_START_TIMEOUT_MS;
    let ready = false;
    while (!ready && Date.now() < deadline && out.exitCode === null) {
      ready = await canConnect(port);
      if (!ready) {
        await new Promise((resolve) => setTimeout(resolve, 50));
      }
    }
    if (!ready) {
      exitedBeforeServing = out.exitCode !== null;
      problems.push(
        exitedBeforeServing
          ? `query server exited with code ${out.exitCode} before accepting connections`
          : `query server did not accept connections within ${SERVER_START_TIMEOUT_MS}ms`
      );
    } else {
      for (const [index, request] of (contractCase.requests ?? []).entries()) {
        const where = `requests[${index}] ${request.method} ${request.target}`;
        const response = await send(port, request);
        problems.push(...mismatches(request.expect.status, response.status, `${where} status`));
        for (const [name, value] of Object.entries(request.expect.headers ?? {})) {
          const actual = response.headers[name.toLowerCase()];
          if (actual === undefined) {
            problems.push(`${where} header ${name}: missing`);
          } else {
            problems.push(...mismatches(value, actual, `${where} header ${name}`));
          }
        }
        try {
          problems.push(...mismatches(request.expect.body, JSON.parse(response.body), `${where} body`));
        } catch {
          problems.push(`${where} body: not JSON: ${response.body}`);
        }
      }
    }
  } finally {
    child.kill("SIGTERM");
    await exited;
  }
  return { out, problems, exitedBeforeServing };
}

describe("bot contract cases", () => {
  beforeAll(() => {
    buildDir = buildContractBot();
  });

  afterAll(() => {
    fs.rmSync(buildDir, { recursive: true, force: true });
  });

  it("finds the cases", () => {
    expect(casePaths().length).toBeGreaterThan(0);
  });

  it("lists only real cases as known failures", () => {
    const names = new Set(casePaths().map(caseName));
    expect(Object.keys(KNOWN_FAILURES).filter((name) => !names.has(name))).toEqual([]);
  });

  for (const file of casePaths()) {
    const name = caseName(file);
    const test = name in KNOWN_FAILURES ? it.fails : it;
    test(
      name,
      async () => {
        const { problems, output } = await runCase(file);
        if (problems.length > 0) {
          expect.fail(`${problems.join("\n")}\n${output}`);
        }
      },
      RUN_TIMEOUT_MS
    );
  }
});
