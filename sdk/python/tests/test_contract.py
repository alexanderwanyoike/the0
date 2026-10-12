"""Runs every bot contract case in sdk/contract/cases against this SDK.

The case format and the matching rules are in sdk/contract/README.md.
"""

import glob
import http.client
import json
import os
import re
import shutil
import socket
import subprocess
import sys
import tempfile
import time
import unittest

TESTS_DIR = os.path.dirname(os.path.abspath(__file__))
SDK_ROOT = os.path.dirname(TESTS_DIR)
CASES_DIR = os.path.join(SDK_ROOT, "..", "contract", "cases")
CONTRACT_BOT = os.path.join(TESTS_DIR, "contract_bot.py")

RUN_TIMEOUT_SECONDS = 60
SERVER_START_TIMEOUT_SECONDS = 30

# Cases this SDK does not pass yet, each with the reason. A listed case that
# starts passing fails the suite, so the list cannot go stale.
KNOWN_FAILURES = {}

TIMESTAMP = re.compile(r"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(\.\d+)?Z$")


def case_paths():
    return sorted(glob.glob(os.path.join(CASES_DIR, "*.json")))


def case_name(path):
    return os.path.splitext(os.path.basename(path))[0]


def is_number(value):
    return isinstance(value, (int, float)) and not isinstance(value, bool)


def mismatches(expected, actual, where):
    """Every way actual fails to match expected, as readable lines."""
    if isinstance(expected, dict) and "$match" in expected:
        return _match(expected, actual, where)
    if isinstance(expected, dict):
        if not isinstance(actual, dict):
            return [f"{where}: expected an object, got {json.dumps(actual)}"]
        problems = []
        for key in expected:
            if key not in actual:
                problems.append(f"{where}.{key}: missing")
            else:
                problems += mismatches(expected[key], actual[key], f"{where}.{key}")
        for key in actual:
            if key not in expected:
                problems.append(f"{where}.{key}: unexpected, got {json.dumps(actual[key])}")
        return problems
    if isinstance(expected, list):
        if not isinstance(actual, list) or len(actual) != len(expected):
            return [f"{where}: expected {json.dumps(expected)}, got {json.dumps(actual)}"]
        problems = []
        for index, (want, got) in enumerate(zip(expected, actual)):
            problems += mismatches(want, got, f"{where}[{index}]")
        return problems
    if is_number(expected):
        if is_number(actual) and actual == expected:
            return []
    elif type(expected) is type(actual) and expected == actual:
        return []
    return [f"{where}: expected {json.dumps(expected)}, got {json.dumps(actual)}"]


def _match(expected, actual, where):
    kind = expected["$match"]
    if kind == "timestamp":
        ok = isinstance(actual, str) and TIMESTAMP.match(actual) is not None
    elif kind == "string":
        ok = isinstance(actual, str)
    elif kind == "contains":
        ok = isinstance(actual, str) and expected["value"] in actual
    elif kind == "nonzero":
        ok = is_number(actual) and actual != 0
    elif kind == "unordered":
        ok = isinstance(actual, list) and len(actual) == len(expected["items"])
        remaining = list(actual) if ok else []
        for item in expected["items"] if ok else []:
            found = next((i for i, got in enumerate(remaining) if not mismatches(item, got, where)), None)
            if found is None:
                ok = False
                break
            remaining.pop(found)
    else:
        return [f"{where}: unknown matcher {kind!r}"]
    if ok:
        return []
    return [f"{where}: expected {json.dumps(expected)}, got {json.dumps(actual)}"]


def line_mismatches(expected_lines, text, stream):
    lines = [line for line in text.splitlines() if line.strip()]
    if len(lines) != len(expected_lines):
        return [f"{stream}: expected {len(expected_lines)} lines, got {len(lines)}"]
    problems = []
    for index, (want, line) in enumerate(zip(expected_lines, lines)):
        try:
            got = json.loads(line)
        except ValueError:
            problems.append(f"{stream}[{index}]: not JSON: {line}")
            continue
        problems += mismatches(want, got, f"{stream}[{index}]")
    return problems


def file_mismatches(expected, path, name):
    if expected is None:
        return [] if not os.path.exists(path) else [f"{name}: expected no file, found one"]
    try:
        with open(path) as f:
            actual = json.load(f)
    except FileNotFoundError:
        return [f"{name}: no file written"]
    except ValueError as e:
        return [f"{name}: not JSON: {e}"]
    return mismatches(expected, actual, name)


def read_state(state_dir):
    files = glob.glob(os.path.join(state_dir, "*.json"))
    state = {}
    for path in files:
        with open(path) as f:
            state[case_name(path)] = json.load(f)
    return state


def free_port():
    with socket.socket() as s:
        s.bind(("127.0.0.1", 0))
        return s.getsockname()[1]


def wait_for_port(port, process):
    deadline = time.monotonic() + SERVER_START_TIMEOUT_SECONDS
    while time.monotonic() < deadline:
        if process.poll() is not None:
            return f"query server exited with code {process.returncode} before accepting connections"
        try:
            socket.create_connection(("127.0.0.1", port), timeout=1).close()
            return None
        except OSError:
            time.sleep(0.05)
    return f"query server did not accept connections within {SERVER_START_TIMEOUT_SECONDS}s"


def send(port, request):
    headers = {}
    body = None
    if "body" in request:
        body = json.dumps(request["body"]).encode()
        headers["Content-Type"] = "application/json"
    elif "raw_body" in request:
        body = request["raw_body"].encode()
        headers["Content-Type"] = request.get("content_type", "text/plain")
    connection = http.client.HTTPConnection("127.0.0.1", port, timeout=RUN_TIMEOUT_SECONDS)
    try:
        connection.request(request["method"], request["target"], body=body, headers=headers)
        response = connection.getresponse()
        return response.status, response.read().decode()
    finally:
        connection.close()


class Sandbox:
    """The directories the runtime gives a bot, rooted in a temporary directory."""

    def __init__(self, case):
        self.root = tempfile.mkdtemp(prefix="the0-contract-")
        self.code_dir = os.path.join(self.root, "bot")
        self.state_dir = os.path.join(self.root, "state", ".the0-state")
        self.query_result = os.path.join(self.root, "query", "result.json")
        os.makedirs(self.code_dir)
        os.makedirs(os.path.dirname(self.query_result))
        if "state_before" in case:
            os.makedirs(self.state_dir)
            for key, value in case["state_before"].items():
                with open(os.path.join(self.state_dir, key + ".json"), "w") as f:
                    json.dump(value, f)
        self.port = free_port() if "requests" in case else None

    def env(self, case):
        placeholders = {
            "{{code_mount_dir}}": self.code_dir.lstrip("/"),
            "{{state_dir}}": self.state_dir,
            "{{port}}": str(self.port),
        }
        env = {}
        for key, value in case["env"].items():
            for placeholder, actual in placeholders.items():
                value = value.replace(placeholder, actual)
            env[key] = value
        env["PATH"] = os.environ.get("PATH", "")
        env["PYTHONPATH"] = SDK_ROOT
        return env

    def remove(self):
        shutil.rmtree(self.root, ignore_errors=True)


def run_case(path):
    """Runs one case and returns its mismatches plus the process output."""
    with open(path) as f:
        case = json.load(f)
    sandbox = Sandbox(case)
    try:
        command = [sys.executable, CONTRACT_BOT, path, sandbox.query_result]
        if "requests" in case:
            problems, exit_code, stdout, stderr = run_server(case, sandbox, command)
        else:
            process = subprocess.run(
                command,
                env=sandbox.env(case),
                capture_output=True,
                text=True,
                timeout=RUN_TIMEOUT_SECONDS,
            )
            problems, exit_code, stdout, stderr = [], process.returncode, process.stdout, process.stderr

        expect = case.get("expect", {})
        if "exit_code" in expect:
            problems += mismatches(expect["exit_code"], exit_code, "exit_code")
        for stream, text in (("stdout", stdout), ("stderr", stderr)):
            if stream in expect:
                problems += line_mismatches(expect[stream], text, stream)
        if "result" in expect:
            result_path = os.path.join(sandbox.code_dir, "result.json")
            problems += file_mismatches(expect["result"], result_path, "result")
        if "query_result" in expect:
            problems += file_mismatches(expect["query_result"], sandbox.query_result, "query_result")
        if "state_after" in expect:
            problems += mismatches(expect["state_after"], read_state(sandbox.state_dir), "state_after")
        return problems, f"exit code: {exit_code}\n--- stdout\n{stdout}--- stderr\n{stderr}"
    finally:
        sandbox.remove()


def run_server(case, sandbox, command):
    process = subprocess.Popen(
        command,
        env=sandbox.env(case),
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    problems = []
    try:
        not_ready = wait_for_port(sandbox.port, process)
        if not_ready:
            problems.append(not_ready)
        else:
            for index, request in enumerate(case["requests"]):
                where = f"requests[{index}] {request['method']} {request['target']}"
                status, body = send(sandbox.port, request)
                problems += mismatches(request["expect"]["status"], status, f"{where} status")
                try:
                    problems += mismatches(request["expect"]["body"], json.loads(body), f"{where} body")
                except ValueError:
                    problems.append(f"{where} body: not JSON: {body}")
    finally:
        process.terminate()
        try:
            stdout, stderr = process.communicate(timeout=10)
        except subprocess.TimeoutExpired:
            process.kill()
            stdout, stderr = process.communicate()
    return problems, process.returncode, stdout, stderr


class TestContractCases(unittest.TestCase):
    """One test per case file; the methods are added below."""

    def test_cases_are_found(self):
        self.assertTrue(case_paths(), f"no contract cases in {CASES_DIR}")

    def test_known_failures_name_real_cases(self):
        names = {case_name(path) for path in case_paths()}
        self.assertEqual(set(KNOWN_FAILURES) - names, set())


def _case_test(path):
    def test(self):
        problems, output = run_case(path)
        if problems:
            self.fail("\n".join(problems) + "\n" + output)

    return test


for _path in case_paths():
    _test = _case_test(_path)
    _test.__doc__ = f"contract case {case_name(_path)}"
    if case_name(_path) in KNOWN_FAILURES:
        _test = unittest.expectedFailure(_test)
    setattr(TestContractCases, "test_" + case_name(_path).replace("-", "_"), _test)


if __name__ == "__main__":
    unittest.main()
