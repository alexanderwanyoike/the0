using System.Diagnostics;
using System.Globalization;
using System.Net.Http.Headers;
using System.Net.Sockets;
using System.Runtime.InteropServices;
using System.Text.Json;
using System.Text.Json.Nodes;
using System.Text.RegularExpressions;
using Xunit;

namespace The0.Tests;

/// <summary>
/// Runs every bot contract case in sdk/contract/cases against this SDK.
/// The case format and the matching rules are in sdk/contract/README.md.
/// </summary>
public class ContractTests
{
    // Cases this SDK does not pass yet, each with the reason; sdk/contract/README.md
    // says how the list is kept.
    private static readonly Dictionary<string, string> KnownFailures = new()
    {
        ["bot-error"] = "Error() does not print its message on stderr",
        ["bot-error-with-data"] = "Error() ignores its data and does not print its message on stderr",
        ["bot-success-with-data"] = "Success() ignores its data",
        ["output-log"] = "log timestamps are epoch milliseconds",
        ["output-metric"] = "metric timestamps are epoch milliseconds",
        ["query-ephemeral-params"] = "a non-string value in QUERY_PARAMS drops every param, and the typed accessors are stubs",
        ["query-ephemeral-read-only-before-key-check"] = "State throws The0.ReadOnlyStateError, not the Query.ReadOnlyStateError the query module documents",
        ["query-ephemeral-state-read-only"] = "State throws The0.ReadOnlyStateError, not the Query.ReadOnlyStateError the query module documents",
        ["query-server-params-json-body"] = "the query server reads only the query string, and the typed accessors are stubs",
        ["query-server-params-query-string"] = "the typed accessors are stubs",
        ["query-server-start-line"] = "the start line is {\"_log\": \"info\"} with no timestamp",
        ["query-server-unknown-path"] = "the 404 does not list the available paths",
    };

    private static readonly TimeSpan RunTimeout = TimeSpan.FromSeconds(60);
    private static readonly TimeSpan ServerStartTimeout = TimeSpan.FromSeconds(30);
    private static readonly TimeSpan StopTimeout = TimeSpan.FromSeconds(10);
    // sdk/contract/README.md, step 5, says why a server case may need a fresh port.
    private const int PortAttempts = 3;
    private const int SIGTERM = 15;

    private static readonly Regex Timestamp =
        new(@"^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\.[0-9]+)?Z$");

    private static readonly string CasesDir = FindCasesDir();
    private static readonly string ContractBot = Path.Combine(AppContext.BaseDirectory, "The0.ContractBot.dll");

    public static IEnumerable<object[]> Cases() =>
        CasePaths().Select(path => new object[] { Path.GetFileNameWithoutExtension(path) });

    [Fact]
    public void CasesAreFound()
    {
        Assert.NotEmpty(CasePaths());
    }

    [Fact]
    public void KnownFailuresNameRealCases()
    {
        var names = CasePaths().Select(path => Path.GetFileNameWithoutExtension(path)).ToHashSet();
        Assert.Subset(names, KnownFailures.Keys.ToHashSet());
    }

    [Theory]
    [MemberData(nameof(Cases))]
    public async Task Case(string name)
    {
        var (problems, output) = await RunCase(Path.Combine(CasesDir, name + ".json"));
        if (KnownFailures.TryGetValue(name, out var reason))
        {
            Assert.True(problems.Count > 0, $"{name} passes now, so take it off KnownFailures (listed because: {reason})");
            return;
        }
        Assert.True(problems.Count == 0, string.Join("\n", problems) + "\n" + output);
    }

    private static string FindCasesDir()
    {
        for (var dir = new DirectoryInfo(AppContext.BaseDirectory); dir != null; dir = dir.Parent)
        {
            var cases = Path.Combine(dir.FullName, "contract", "cases");
            if (Directory.Exists(cases))
            {
                return cases;
            }
        }
        throw new DirectoryNotFoundException($"no sdk/contract/cases above {AppContext.BaseDirectory}");
    }

    private static string[] CasePaths() =>
        Directory.GetFiles(CasesDir, "*.json").OrderBy(path => path, StringComparer.Ordinal).ToArray();

    private sealed record Run(int? ExitCode, string Stdout, string Stderr, List<string> Problems, bool ExitedBeforeServing = false)
    {
        public string Output() => $"exit code: {ExitCode}\n--- stdout\n{Stdout}--- stderr\n{Stderr}";
    }

    /// <summary>The directories the runtime gives a bot, rooted in a temporary directory.</summary>
    private sealed class Sandbox : IDisposable
    {
        public string Root { get; }
        public string CodeDir { get; }
        public string StateDir { get; }
        public string QueryResult { get; }
        public int? Port { get; }

        public Sandbox(JsonObject testCase, int? port)
        {
            Port = port;
            Root = Directory.CreateTempSubdirectory("the0-contract-").FullName;
            CodeDir = Path.Combine(Root, "bot");
            StateDir = Path.Combine(Root, "state", ".the0-state");
            QueryResult = Path.Combine(Root, "query", "result.json");
            Directory.CreateDirectory(CodeDir);
            Directory.CreateDirectory(Path.GetDirectoryName(QueryResult)!);
            if (testCase["state_before"] is JsonObject stateBefore)
            {
                Directory.CreateDirectory(StateDir);
                foreach (var (key, value) in stateBefore)
                {
                    File.WriteAllText(Path.Combine(StateDir, key + ".json"), Json(value));
                }
            }
        }

        public void ApplyEnvironment(JsonObject testCase, ProcessStartInfo startInfo)
        {
            var placeholders = new Dictionary<string, string>
            {
                ["{{code_mount_dir}}"] = CodeDir.TrimStart('/'),
                ["{{state_dir}}"] = StateDir,
                ["{{port}}"] = Port?.ToString(CultureInfo.InvariantCulture) ?? "",
            };
            var inherited = new[] { "PATH", "HOME", "DOTNET_ROOT" }
                .Select(name => (name, value: Environment.GetEnvironmentVariable(name)))
                .Where(pair => pair.value != null)
                .ToList();

            startInfo.Environment.Clear();
            foreach (var (name, value) in inherited)
            {
                startInfo.Environment[name] = value;
            }
            foreach (var (key, value) in testCase["env"]!.AsObject())
            {
                var text = (string)value!;
                foreach (var (placeholder, actual) in placeholders)
                {
                    text = text.Replace(placeholder, actual);
                }
                startInfo.Environment[key] = text;
            }
        }

        public void Dispose()
        {
            try
            {
                Directory.Delete(Root, recursive: true);
            }
            catch (IOException)
            {
                // A leftover temporary directory does not change the result.
            }
        }
    }

    private static async Task<(List<string> Problems, string Output)> RunCase(string path)
    {
        var testCase = JsonNode.Parse(File.ReadAllText(path))!.AsObject();
        var servesHttp = testCase.ContainsKey("requests");
        for (var attempt = 1; ; attempt++)
        {
            using var sandbox = new Sandbox(testCase, servesHttp ? FreePort() : null);
            var startInfo = ContractBotStartInfo(path, sandbox);
            sandbox.ApplyEnvironment(testCase, startInfo);
            var run = servesHttp
                ? await RunServer(testCase, sandbox, startInfo)
                : await RunToExit(startInfo);
            if (run.ExitedBeforeServing && attempt < PortAttempts)
            {
                continue;
            }
            return (Compare(testCase, sandbox, run), run.Output());
        }
    }

    private static ProcessStartInfo ContractBotStartInfo(string casePath, Sandbox sandbox)
    {
        var startInfo = new ProcessStartInfo(DotnetHost())
        {
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
        };
        startInfo.ArgumentList.Add(ContractBot);
        startInfo.ArgumentList.Add(casePath);
        startInfo.ArgumentList.Add(sandbox.QueryResult);
        return startInfo;
    }

    // The test host runs under the same dotnet that must start the contract
    // bot; PATH may not lead to it, or may lead to a different install.
    private static string DotnetHost()
    {
        var hostPath = Environment.GetEnvironmentVariable("DOTNET_HOST_PATH");
        if (!string.IsNullOrEmpty(hostPath) && File.Exists(hostPath))
        {
            return hostPath;
        }
        var processPath = Environment.ProcessPath;
        if (processPath != null && Path.GetFileNameWithoutExtension(processPath) == "dotnet")
        {
            return processPath;
        }
        return "dotnet";
    }

    private static async Task<Run> RunToExit(ProcessStartInfo startInfo)
    {
        using var process = Process.Start(startInfo)!;
        var stdout = process.StandardOutput.ReadToEndAsync();
        var stderr = process.StandardError.ReadToEndAsync();
        var problems = new List<string>();
        using var timeout = new CancellationTokenSource(RunTimeout);
        try
        {
            await process.WaitForExitAsync(timeout.Token);
        }
        catch (OperationCanceledException)
        {
            process.Kill(entireProcessTree: true);
            await process.WaitForExitAsync();
            problems.Add($"the contract bot did not exit within {RunTimeout.TotalSeconds}s");
        }
        return new Run(process.ExitCode, await stdout, await stderr, problems);
    }

    private static async Task<Run> RunServer(JsonObject testCase, Sandbox sandbox, ProcessStartInfo startInfo)
    {
        using var process = Process.Start(startInfo)!;
        var stdout = process.StandardOutput.ReadToEndAsync();
        var stderr = process.StandardError.ReadToEndAsync();
        var problems = new List<string>();
        var exitedBeforeServing = false;
        try
        {
            var notReady = await WaitForPort(sandbox.Port!.Value, process);
            if (notReady != null)
            {
                problems.Add(notReady);
                exitedBeforeServing = process.HasExited;
            }
            else
            {
                var index = 0;
                foreach (var request in testCase["requests"]!.AsArray())
                {
                    problems.AddRange(await CheckRequest(sandbox.Port!.Value, request!.AsObject(), index++));
                }
            }
        }
        finally
        {
            await Stop(process);
        }
        return new Run(process.ExitCode, await stdout, await stderr, problems, exitedBeforeServing);
    }

    [DllImport("libc", SetLastError = true)]
    private static extern int kill(int pid, int signal);

    private static async Task Stop(Process process)
    {
        if (!process.HasExited)
        {
            kill(process.Id, SIGTERM);
        }
        using var timeout = new CancellationTokenSource(StopTimeout);
        try
        {
            await process.WaitForExitAsync(timeout.Token);
        }
        catch (OperationCanceledException)
        {
            process.Kill(entireProcessTree: true);
            await process.WaitForExitAsync();
        }
    }

    private static int FreePort()
    {
        var listener = new TcpListener(System.Net.IPAddress.Loopback, 0);
        listener.Start();
        var port = ((System.Net.IPEndPoint)listener.LocalEndpoint).Port;
        listener.Stop();
        return port;
    }

    private static async Task<string?> WaitForPort(int port, Process process)
    {
        var deadline = DateTime.UtcNow + ServerStartTimeout;
        while (DateTime.UtcNow < deadline)
        {
            if (process.HasExited)
            {
                return $"query server exited with code {process.ExitCode} before accepting connections";
            }
            try
            {
                using var client = new TcpClient();
                using var attempt = new CancellationTokenSource(TimeSpan.FromSeconds(1));
                await client.ConnectAsync("127.0.0.1", port, attempt.Token);
                return null;
            }
            catch (Exception e) when (e is SocketException or OperationCanceledException)
            {
                await Task.Delay(50);
            }
        }
        return $"query server did not accept connections within {ServerStartTimeout.TotalSeconds}s";
    }

    private static async Task<List<string>> CheckRequest(int port, JsonObject request, int index)
    {
        var method = (string)request["method"]!;
        var target = (string)request["target"]!;
        var where = $"requests[{index}] {method} {target}";
        var expect = request["expect"]!.AsObject();

        using var message = new HttpRequestMessage(new HttpMethod(method), $"http://127.0.0.1:{port}{target}");
        if (request.ContainsKey("body"))
        {
            message.Content = new StringContent(Json(request["body"]));
            message.Content.Headers.ContentType = new MediaTypeHeaderValue("application/json");
        }
        else if (request["raw_body"] is JsonNode rawBody)
        {
            message.Content = new StringContent((string)rawBody!);
            message.Content.Headers.ContentType =
                MediaTypeHeaderValue.Parse((string?)request["content_type"] ?? "text/plain");
        }

        // A fresh client per request, so each request goes on a new connection.
        using var client = new HttpClient { Timeout = RunTimeout };
        using var response = await client.SendAsync(message);
        var headers = response.Headers.Concat(response.Content.Headers)
            .ToDictionary(header => header.Key, header => string.Join(", ", header.Value), StringComparer.OrdinalIgnoreCase);
        var body = await response.Content.ReadAsStringAsync();

        var problems = Mismatches(expect["status"], (int)response.StatusCode, $"{where} status");
        if (expect["headers"] is JsonObject expectedHeaders)
        {
            foreach (var (name, value) in expectedHeaders)
            {
                if (!headers.TryGetValue(name, out var actual))
                    problems.Add($"{where} header {name}: missing");
                else
                    problems.AddRange(Mismatches(value, actual, $"{where} header {name}"));
            }
        }
        try
        {
            problems.AddRange(Mismatches(expect["body"], JsonNode.Parse(body), $"{where} body"));
        }
        catch (JsonException)
        {
            problems.Add($"{where} body: not JSON: {body}");
        }
        return problems;
    }

    private static List<string> Compare(JsonObject testCase, Sandbox sandbox, Run run)
    {
        var problems = new List<string>(run.Problems);
        var expect = testCase["expect"] as JsonObject ?? new JsonObject();
        if (expect.ContainsKey("exit_code"))
        {
            problems.AddRange(Mismatches(expect["exit_code"], run.ExitCode, "exit_code"));
        }
        if (expect["stdout"] is JsonArray expectedStdout)
        {
            problems.AddRange(LineMismatches(expectedStdout, run.Stdout, "stdout"));
        }
        if (expect["stderr"] is JsonArray expectedStderr)
        {
            problems.AddRange(LineMismatches(expectedStderr, run.Stderr, "stderr"));
        }
        if (expect.ContainsKey("result"))
        {
            problems.AddRange(FileMismatches(expect["result"], Path.Combine(sandbox.CodeDir, "result.json"), "result"));
        }
        if (expect.ContainsKey("query_result"))
        {
            problems.AddRange(FileMismatches(expect["query_result"], sandbox.QueryResult, "query_result"));
        }
        if (expect.ContainsKey("state_after"))
        {
            problems.AddRange(Mismatches(expect["state_after"], ReadState(sandbox.StateDir), "state_after"));
        }
        return problems;
    }

    private static List<string> LineMismatches(JsonArray expected, string text, string stream)
    {
        var lines = text.Split('\n').Where(line => !string.IsNullOrWhiteSpace(line)).ToList();
        if (lines.Count != expected.Count)
        {
            return new List<string> { $"{stream}: expected {expected.Count} lines, got {lines.Count}" };
        }
        var problems = new List<string>();
        for (var index = 0; index < lines.Count; index++)
        {
            try
            {
                problems.AddRange(Mismatches(expected[index], JsonNode.Parse(lines[index]), $"{stream}[{index}]"));
            }
            catch (JsonException)
            {
                problems.Add($"{stream}[{index}]: not JSON: {lines[index]}");
            }
        }
        return problems;
    }

    private static List<string> FileMismatches(JsonNode? expected, string path, string name)
    {
        if (expected == null)
        {
            return File.Exists(path) ? new List<string> { $"{name}: expected no file, found one" } : new List<string>();
        }
        if (!File.Exists(path))
        {
            return new List<string> { $"{name}: no file written" };
        }
        try
        {
            return Mismatches(expected, JsonNode.Parse(File.ReadAllText(path)), name);
        }
        catch (JsonException e)
        {
            return new List<string> { $"{name}: not JSON: {e.Message}" };
        }
    }

    private static JsonObject ReadState(string stateDir)
    {
        var state = new JsonObject();
        if (Directory.Exists(stateDir))
        {
            foreach (var file in Directory.GetFiles(stateDir, "*.json"))
            {
                state[Path.GetFileNameWithoutExtension(file)] = JsonNode.Parse(File.ReadAllText(file));
            }
        }
        return state;
    }

    /// <summary>Every way actual fails to match expected, as readable lines.</summary>
    private static List<string> Mismatches(JsonNode? expected, JsonNode? actual, string where)
    {
        if (expected is JsonObject matcher && matcher.ContainsKey("$match"))
        {
            return Match(matcher, actual, where);
        }
        if (expected is JsonObject expectedObject)
        {
            if (actual is not JsonObject actualObject)
            {
                return new List<string> { $"{where}: expected an object, got {Json(actual)}" };
            }
            var problems = new List<string>();
            foreach (var (key, value) in expectedObject)
            {
                if (!actualObject.ContainsKey(key))
                    problems.Add($"{where}.{key}: missing");
                else
                    problems.AddRange(Mismatches(value, actualObject[key], $"{where}.{key}"));
            }
            foreach (var (key, value) in actualObject)
            {
                if (!expectedObject.ContainsKey(key))
                {
                    problems.Add($"{where}.{key}: unexpected, got {Json(value)}");
                }
            }
            return problems;
        }
        if (expected is JsonArray expectedArray)
        {
            if (actual is not JsonArray actualArray || actualArray.Count != expectedArray.Count)
            {
                return new List<string> { $"{where}: expected {Json(expected)}, got {Json(actual)}" };
            }
            var problems = new List<string>();
            for (var index = 0; index < expectedArray.Count; index++)
            {
                problems.AddRange(Mismatches(expectedArray[index], actualArray[index], $"{where}[{index}]"));
            }
            return problems;
        }
        var same = Kind(expected) == Kind(actual) && Kind(expected) switch
        {
            JsonValueKind.Number => Number(expected) == Number(actual),
            JsonValueKind.String => expected!.GetValue<string>() == actual!.GetValue<string>(),
            _ => true,
        };
        return same ? new List<string>() : new List<string> { $"{where}: expected {Json(expected)}, got {Json(actual)}" };
    }

    private static List<string> Match(JsonObject expected, JsonNode? actual, string where)
    {
        var kind = (string)expected["$match"]!;
        var text = Kind(actual) == JsonValueKind.String ? actual!.GetValue<string>() : null;
        bool ok;
        switch (kind)
        {
            case "timestamp":
                ok = text != null && Timestamp.IsMatch(text);
                break;
            case "string":
                ok = text != null;
                break;
            case "contains":
                ok = text != null && text.Contains((string)expected["value"]!, StringComparison.Ordinal);
                break;
            case "nonzero":
                ok = Kind(actual) == JsonValueKind.Number && Number(actual) != 0;
                break;
            case "unordered":
                var items = expected["items"]!.AsArray();
                ok = actual is JsonArray actualArray && actualArray.Count == items.Count;
                var remaining = ok ? ((JsonArray)actual!).ToList() : new List<JsonNode?>();
                foreach (var item in ok ? items : new JsonArray())
                {
                    var found = remaining.FindIndex(got => Mismatches(item, got, where).Count == 0);
                    if (found < 0)
                    {
                        ok = false;
                        break;
                    }
                    remaining.RemoveAt(found);
                }
                break;
            default:
                return new List<string> { $"{where}: unknown matcher '{kind}'" };
        }
        return ok ? new List<string>() : new List<string> { $"{where}: expected {Json(expected)}, got {Json(actual)}" };
    }

    private static JsonValueKind Kind(JsonNode? node) => node?.GetValueKind() ?? JsonValueKind.Null;

    private static double Number(JsonNode? node) =>
        double.Parse(Json(node), NumberStyles.Float, CultureInfo.InvariantCulture);

    private static string Json(JsonNode? node) => node?.ToJsonString() ?? "null";
}
