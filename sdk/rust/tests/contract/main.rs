//! Runs every bot contract case in sdk/contract/cases against this SDK.
//!
//! The case format and the matching rules are in sdk/contract/README.md.
//!
//! This test binary is also the contract bot: the harness starts it again with
//! `contract-bot` as its first argument, and it then plays one case. Being one
//! binary means the bot needs no build step of its own, and the harness owns
//! `main`, so the test runner prints nothing on the bot's stdout.

mod contract_bot;

use libtest_mimic::{Arguments, Failed, Trial};
use serde_json::{Map, Value};
use std::collections::HashMap;
use std::fs;
use std::io::{Read, Write};
use std::net::{Ipv4Addr, SocketAddr, TcpListener, TcpStream};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, ExitStatus, Stdio};
use std::thread::{self, JoinHandle};
use std::time::{Duration, Instant};
use tempfile::TempDir;

const CONTRACT_BOT_ARG: &str = "contract-bot";

const RUN_TIMEOUT: Duration = Duration::from_secs(60);
const SERVER_START_TIMEOUT: Duration = Duration::from_secs(30);
const STOP_TIMEOUT: Duration = Duration::from_secs(10);
// sdk/contract/README.md, step 5, says why a server case may need a fresh port.
const PORT_ATTEMPTS: usize = 3;

// Cases this SDK does not pass yet, each with the reason; sdk/contract/README.md
// says how the list is kept.
const KNOWN_FAILURES: &[(&str, &str)] = &[
    (
        "query-server-params-json-body",
        "the server reads params from the query string only",
    ),
    (
        "query-server-params-query-string",
        "the server does not URL-decode the query string",
    ),
];

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.get(1).map(String::as_str) == Some(CONTRACT_BOT_ARG) {
        contract_bot::main(&args[2], &args[3]);
        return;
    }
    libtest_mimic::run(&Arguments::from_args(), trials()).exit();
}

fn cases_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../contract/cases")
}

fn case_paths() -> Vec<PathBuf> {
    let mut paths: Vec<PathBuf> = fs::read_dir(cases_dir())
        .map(|entries| {
            entries
                .filter_map(|entry| entry.ok().map(|entry| entry.path()))
                .filter(|path| path.extension().is_some_and(|ext| ext == "json"))
                .collect()
        })
        .unwrap_or_default();
    paths.sort();
    paths
}

fn file_stem(path: &Path) -> String {
    path.file_stem()
        .map(|stem| stem.to_string_lossy().into_owned())
        .unwrap_or_default()
}

fn trials() -> Vec<Trial> {
    let paths = case_paths();
    let names: Vec<String> = paths.iter().map(|path| file_stem(path)).collect();

    let found = !paths.is_empty();
    let mut trials = vec![
        Trial::test("cases_are_found", move || {
            if found {
                Ok(())
            } else {
                Err(format!("no contract cases in {}", cases_dir().display()).into())
            }
        }),
        Trial::test("known_failures_name_real_cases", move || {
            let unknown: Vec<&str> = KNOWN_FAILURES
                .iter()
                .map(|(name, _)| *name)
                .filter(|name| !names.iter().any(|case| case == name))
                .collect();
            if unknown.is_empty() {
                Ok(())
            } else {
                Err(format!("KNOWN_FAILURES names no such case: {unknown:?}").into())
            }
        }),
    ];

    for path in paths {
        let name = file_stem(&path);
        let known_failure = KNOWN_FAILURES
            .iter()
            .find(|(listed, _)| *listed == name)
            .map(|(_, reason)| *reason);
        trials.push(Trial::test(name, move || check_case(&path, known_failure)).with_kind("case"));
    }
    trials
}

fn check_case(path: &Path, known_failure: Option<&str>) -> Result<(), Failed> {
    let (problems, output) = run_case(path);
    match (problems.is_empty(), known_failure) {
        (true, None) | (false, Some(_)) => Ok(()),
        (true, Some(reason)) => Err(format!(
            "listed in KNOWN_FAILURES ({reason}) but passes: take it off the list"
        )
        .into()),
        (false, None) => Err(format!("{}\n{}", problems.join("\n"), output).into()),
    }
}

fn run_case(path: &Path) -> (Vec<String>, String) {
    let text = fs::read_to_string(path).expect("cannot read the case");
    let case: Value = serde_json::from_str(&text).expect("the case is not JSON");
    let serves_http = case.get("requests").is_some();
    let mut attempt = 1;
    loop {
        let sandbox = Sandbox::new(&case, serves_http.then(free_port));
        let run = run_contract_bot(&case, path, &sandbox);
        if run.exited_before_serving && attempt < PORT_ATTEMPTS {
            attempt += 1;
            continue;
        }
        return (compare(&case, &sandbox, &run), run.output());
    }
}

/// The directories the runtime gives a bot, rooted in a temporary directory
/// that is removed on drop.
struct Sandbox {
    root: TempDir,
    port: Option<u16>,
}

impl Sandbox {
    fn new(case: &Value, port: Option<u16>) -> Self {
        let sandbox = Sandbox {
            root: tempfile::Builder::new()
                .prefix("the0-contract-")
                .tempdir()
                .expect("cannot make a sandbox"),
            port,
        };
        fs::create_dir_all(sandbox.code_dir()).unwrap();
        fs::create_dir_all(sandbox.query_result().parent().unwrap()).unwrap();
        if let Some(state) = case.get("state_before").and_then(Value::as_object) {
            fs::create_dir_all(sandbox.state_dir()).unwrap();
            for (key, value) in state {
                let file = sandbox.state_dir().join(format!("{key}.json"));
                fs::write(file, value.to_string()).unwrap();
            }
        }
        sandbox
    }

    fn code_dir(&self) -> PathBuf {
        self.root.path().join("bot")
    }

    fn state_dir(&self) -> PathBuf {
        self.root.path().join("state").join(".the0-state")
    }

    fn query_result(&self) -> PathBuf {
        self.root.path().join("query").join("result.json")
    }

    fn env(&self, case: &Value) -> Vec<(String, String)> {
        let code_dir = self.code_dir();
        let placeholders = [
            (
                "{{code_mount_dir}}",
                code_dir
                    .to_string_lossy()
                    .trim_start_matches('/')
                    .to_string(),
            ),
            (
                "{{state_dir}}",
                self.state_dir().to_string_lossy().into_owned(),
            ),
            (
                "{{port}}",
                self.port.map(|port| port.to_string()).unwrap_or_default(),
            ),
        ];
        let env = case["env"].as_object().expect("a case needs env");
        env.iter()
            .map(|(key, value)| {
                let mut value = value.as_str().expect("env values are strings").to_string();
                for (placeholder, actual) in &placeholders {
                    value = value.replace(placeholder, actual);
                }
                (key.clone(), value)
            })
            .collect()
    }
}

struct Run {
    exit_code: Value,
    stdout: String,
    stderr: String,
    // Mismatches found while the process ran, such as HTTP responses.
    problems: Vec<String>,
    exited_before_serving: bool,
}

impl Run {
    fn output(&self) -> String {
        format!(
            "exit code: {}\n--- stdout\n{}--- stderr\n{}",
            self.exit_code, self.stdout, self.stderr
        )
    }
}

fn run_contract_bot(case: &Value, case_path: &Path, sandbox: &Sandbox) -> Run {
    let mut child = Command::new(std::env::current_exe().expect("cannot find this test binary"))
        .arg(CONTRACT_BOT_ARG)
        .arg(case_path)
        .arg(sandbox.query_result())
        .env_clear()
        .envs(sandbox.env(case))
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("cannot start the contract bot");
    let stdout = capture(child.stdout.take().unwrap());
    let stderr = capture(child.stderr.take().unwrap());

    let mut problems = Vec::new();
    let mut exited_before_serving = false;
    let status = match sandbox.port {
        Some(port) => {
            match wait_for_port(port, &mut child) {
                Ok(()) => {
                    for (index, request) in case["requests"].as_array().unwrap().iter().enumerate()
                    {
                        problems.extend(request_mismatches(port, index, request));
                    }
                }
                Err(not_ready) => {
                    exited_before_serving = matches!(child.try_wait(), Ok(Some(_)));
                    problems.push(not_ready);
                }
            }
            stop(&mut child)
        }
        None => wait_until(&mut child, Instant::now() + RUN_TIMEOUT).or_else(|| {
            problems.push(format!("did not exit within {}s", RUN_TIMEOUT.as_secs()));
            kill(&mut child)
        }),
    };

    Run {
        exit_code: status.map(exit_code).unwrap_or(Value::Null),
        stdout: stdout.join().unwrap(),
        stderr: stderr.join().unwrap(),
        problems,
        exited_before_serving,
    }
}

fn capture(mut pipe: impl Read + Send + 'static) -> JoinHandle<String> {
    thread::spawn(move || {
        let mut bytes = Vec::new();
        let _ = pipe.read_to_end(&mut bytes);
        String::from_utf8_lossy(&bytes).into_owned()
    })
}

fn wait_until(child: &mut Child, deadline: Instant) -> Option<ExitStatus> {
    loop {
        if let Ok(Some(status)) = child.try_wait() {
            return Some(status);
        }
        if Instant::now() >= deadline {
            return None;
        }
        thread::sleep(Duration::from_millis(10));
    }
}

fn kill(child: &mut Child) -> Option<ExitStatus> {
    let _ = child.kill();
    child.wait().ok()
}

/// Stops a query server the way the runtime does, with SIGTERM.
fn stop(child: &mut Child) -> Option<ExitStatus> {
    if let Ok(Some(status)) = child.try_wait() {
        return Some(status);
    }
    unsafe {
        libc::kill(child.id() as libc::pid_t, libc::SIGTERM);
    }
    wait_until(child, Instant::now() + STOP_TIMEOUT).or_else(|| kill(child))
}

/// The exit code as JSON. A process ended by a signal gets the negated signal
/// number, as Python reports it, so `nonzero` still matches.
fn exit_code(status: ExitStatus) -> Value {
    use std::os::unix::process::ExitStatusExt;
    match status.code() {
        Some(code) => Value::from(code),
        None => Value::from(-status.signal().unwrap_or(0)),
    }
}

fn free_port() -> u16 {
    TcpListener::bind((Ipv4Addr::LOCALHOST, 0))
        .and_then(|listener| listener.local_addr())
        .map(|addr| addr.port())
        .expect("cannot find a free port")
}

fn wait_for_port(port: u16, child: &mut Child) -> Result<(), String> {
    let address = SocketAddr::from((Ipv4Addr::LOCALHOST, port));
    let deadline = Instant::now() + SERVER_START_TIMEOUT;
    while Instant::now() < deadline {
        if let Ok(Some(status)) = child.try_wait() {
            return Err(format!(
                "query server exited with {} before accepting connections",
                exit_code(status)
            ));
        }
        if TcpStream::connect_timeout(&address, Duration::from_secs(1)).is_ok() {
            return Ok(());
        }
        thread::sleep(Duration::from_millis(50));
    }
    Err(format!(
        "query server did not accept connections within {}s",
        SERVER_START_TIMEOUT.as_secs()
    ))
}

struct Response {
    status: u16,
    headers: HashMap<String, String>,
    body: String,
}

fn request_mismatches(port: u16, index: usize, request: &Value) -> Vec<String> {
    let method = request["method"]
        .as_str()
        .expect("a request needs a method");
    let target = request["target"]
        .as_str()
        .expect("a request needs a target");
    let at = format!("requests[{index}] {method} {target}");
    let expect = &request["expect"];

    let response = match send(port, method, target, request) {
        Ok(response) => response,
        Err(e) => return vec![format!("{at}: {e}")],
    };
    let mut problems = mismatches(
        &expect["status"],
        &Value::from(response.status),
        &format!("{at} status"),
    );
    if let Some(headers) = expect.get("headers").and_then(Value::as_object) {
        for (name, want) in headers {
            match response.headers.get(&name.to_lowercase()) {
                None => problems.push(format!("{at} header {name}: missing")),
                Some(got) => problems.extend(mismatches(
                    want,
                    &Value::from(got.as_str()),
                    &format!("{at} header {name}"),
                )),
            }
        }
    }
    match serde_json::from_str::<Value>(&response.body) {
        Ok(body) => problems.extend(mismatches(&expect["body"], &body, &format!("{at} body"))),
        Err(_) => problems.push(format!("{at} body: not JSON: {}", response.body)),
    }
    problems
}

/// One request on a new connection, then the whole response.
fn send(port: u16, method: &str, target: &str, request: &Value) -> Result<Response, String> {
    let (body, content_type) = if let Some(body) = request.get("body") {
        (body.to_string().into_bytes(), Some("application/json"))
    } else if let Some(raw) = request.get("raw_body").and_then(Value::as_str) {
        let content_type = request
            .get("content_type")
            .and_then(Value::as_str)
            .unwrap_or("text/plain");
        (raw.as_bytes().to_vec(), Some(content_type))
    } else {
        (Vec::new(), None)
    };

    let mut head =
        format!("{method} {target} HTTP/1.1\r\nHost: 127.0.0.1:{port}\r\nConnection: close\r\n");
    if let Some(content_type) = content_type {
        head.push_str(&format!("Content-Type: {content_type}\r\n"));
    }
    if !body.is_empty() || method == "POST" {
        head.push_str(&format!("Content-Length: {}\r\n", body.len()));
    }
    head.push_str("\r\n");

    let mut stream = TcpStream::connect((Ipv4Addr::LOCALHOST, port))
        .map_err(|e| format!("cannot connect: {e}"))?;
    stream.set_read_timeout(Some(RUN_TIMEOUT)).unwrap();
    stream
        .write_all(&[head.as_bytes(), &body].concat())
        .map_err(|e| format!("cannot send: {e}"))?;

    let mut raw = Vec::new();
    if let Err(e) = stream.read_to_end(&mut raw) {
        // A server that closes without reading the whole request may reset
        // the connection after it has sent its response.
        if raw.is_empty() {
            return Err(format!("no response: {e}"));
        }
    }
    parse_response(&raw)
}

fn parse_response(raw: &[u8]) -> Result<Response, String> {
    let text = String::from_utf8_lossy(raw);
    let (head, body) = text
        .split_once("\r\n\r\n")
        .ok_or_else(|| format!("not an HTTP response: {text}"))?;
    let mut lines = head.split("\r\n");
    let status = lines
        .next()
        .and_then(|line| line.split_whitespace().nth(1))
        .and_then(|code| code.parse().ok())
        .ok_or_else(|| format!("no status line: {head}"))?;
    let headers: HashMap<String, String> = lines
        .filter_map(|line| line.split_once(':'))
        .map(|(name, value)| (name.trim().to_lowercase(), value.trim().to_string()))
        .collect();
    let body = match headers.get("content-length").and_then(|n| n.parse().ok()) {
        Some(length) if length <= body.len() => body[..length].to_string(),
        _ => body.to_string(),
    };
    Ok(Response {
        status,
        headers,
        body,
    })
}

fn compare(case: &Value, sandbox: &Sandbox, run: &Run) -> Vec<String> {
    let mut problems = run.problems.clone();
    let Some(expect) = case.get("expect") else {
        return problems;
    };
    if let Some(want) = expect.get("exit_code") {
        problems.extend(mismatches(want, &run.exit_code, "exit_code"));
    }
    for (stream, text) in [("stdout", &run.stdout), ("stderr", &run.stderr)] {
        if let Some(want) = expect.get(stream) {
            problems.extend(line_mismatches(want, text, stream));
        }
    }
    if let Some(want) = expect.get("result") {
        let result = sandbox.code_dir().join("result.json");
        problems.extend(file_mismatches(want, &result, "result"));
    }
    if let Some(want) = expect.get("query_result") {
        problems.extend(file_mismatches(
            want,
            &sandbox.query_result(),
            "query_result",
        ));
    }
    if let Some(want) = expect.get("state_after") {
        problems.extend(mismatches(
            want,
            &read_state(&sandbox.state_dir()),
            "state_after",
        ));
    }
    problems
}

fn line_mismatches(expected: &Value, text: &str, stream: &str) -> Vec<String> {
    let expected = expected.as_array().expect("expected lines are an array");
    let lines: Vec<&str> = text
        .lines()
        .filter(|line| !line.trim().is_empty())
        .collect();
    if lines.len() != expected.len() {
        return vec![format!(
            "{stream}: expected {} lines, got {}",
            expected.len(),
            lines.len()
        )];
    }
    let mut problems = Vec::new();
    for (index, (want, line)) in expected.iter().zip(lines).enumerate() {
        match serde_json::from_str::<Value>(line) {
            Ok(got) => problems.extend(mismatches(want, &got, &format!("{stream}[{index}]"))),
            Err(_) => problems.push(format!("{stream}[{index}]: not JSON: {line}")),
        }
    }
    problems
}

fn file_mismatches(expected: &Value, path: &Path, name: &str) -> Vec<String> {
    if expected.is_null() {
        return if path.exists() {
            vec![format!("{name}: expected no file, found one")]
        } else {
            Vec::new()
        };
    }
    let Ok(text) = fs::read_to_string(path) else {
        return vec![format!("{name}: no file written")];
    };
    match serde_json::from_str::<Value>(&text) {
        Ok(actual) => mismatches(expected, &actual, name),
        Err(e) => vec![format!("{name}: not JSON: {e}")],
    }
}

fn read_state(state_dir: &Path) -> Value {
    let mut state = Map::new();
    for entry in fs::read_dir(state_dir).into_iter().flatten().flatten() {
        let path = entry.path();
        if path.extension().is_some_and(|ext| ext == "json") {
            let text = fs::read_to_string(&path).unwrap_or_default();
            let value = serde_json::from_str(&text).unwrap_or(Value::String(text));
            state.insert(file_stem(&path), value);
        }
    }
    Value::Object(state)
}

/// Every way `actual` fails to match `expected`, as readable lines.
fn mismatches(expected: &Value, actual: &Value, at: &str) -> Vec<String> {
    let differ = || vec![format!("{at}: expected {expected}, got {actual}")];
    match expected {
        Value::Object(want) if want.contains_key("$match") => match matches(want, actual) {
            Ok(true) => Vec::new(),
            Ok(false) => differ(),
            Err(unknown) => vec![format!("{at}: {unknown}")],
        },
        Value::Object(want) => {
            let Value::Object(got) = actual else {
                return vec![format!("{at}: expected an object, got {actual}")];
            };
            let mut problems = Vec::new();
            for (key, want_value) in want {
                match got.get(key) {
                    None => problems.push(format!("{at}.{key}: missing")),
                    Some(got_value) => {
                        problems.extend(mismatches(want_value, got_value, &format!("{at}.{key}")))
                    }
                }
            }
            for (key, got_value) in got {
                if !want.contains_key(key) {
                    problems.push(format!("{at}.{key}: unexpected, got {got_value}"));
                }
            }
            problems
        }
        Value::Array(want) => match actual {
            Value::Array(got) if got.len() == want.len() => want
                .iter()
                .zip(got)
                .enumerate()
                .flat_map(|(index, (w, g))| mismatches(w, g, &format!("{at}[{index}]")))
                .collect(),
            _ => differ(),
        },
        Value::Number(want) if actual.is_number() && actual.as_f64() == want.as_f64() => Vec::new(),
        Value::Number(_) => differ(),
        _ if expected == actual => Vec::new(),
        _ => differ(),
    }
}

/// Whether `actual` matches a `{"$match": ...}` matcher.
fn matches(matcher: &Map<String, Value>, actual: &Value) -> Result<bool, String> {
    let kind = matcher["$match"].as_str().unwrap_or_default();
    Ok(match kind {
        "timestamp" => actual.as_str().is_some_and(is_timestamp),
        "string" => actual.is_string(),
        "contains" => {
            let part = matcher["value"].as_str().unwrap_or_default();
            actual.as_str().is_some_and(|text| text.contains(part))
        }
        "nonzero" => actual.as_f64().is_some_and(|n| n != 0.0),
        "unordered" => unordered_matches(matcher["items"].as_array().unwrap(), actual),
        _ => return Err(format!("unknown matcher {kind:?}")),
    })
}

fn unordered_matches(items: &[Value], actual: &Value) -> bool {
    let Some(actual) = actual.as_array() else {
        return false;
    };
    if actual.len() != items.len() {
        return false;
    }
    let mut remaining: Vec<&Value> = actual.iter().collect();
    for item in items {
        match remaining
            .iter()
            .position(|got| mismatches(item, got, "").is_empty())
        {
            Some(index) => {
                remaining.remove(index);
            }
            None => return false,
        }
    }
    true
}

/// `^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(\.\d+)?Z$`
fn is_timestamp(text: &str) -> bool {
    let bytes = text.as_bytes();
    let shape = b"dddd-dd-ddTdd:dd:dd";
    if bytes.len() < shape.len() + 1 {
        return false;
    }
    let date_time_ok = shape.iter().zip(bytes).all(|(want, got)| match want {
        b'd' => got.is_ascii_digit(),
        _ => want == got,
    });
    date_time_ok
        && match &bytes[shape.len()..] {
            [b'Z'] => true,
            [b'.', fraction @ .., b'Z'] => {
                !fraction.is_empty() && fraction.iter().all(u8::is_ascii_digit)
            }
            _ => false,
        }
}
