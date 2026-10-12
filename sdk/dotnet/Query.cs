using System.IO;
using System.Net;
using System.Text.Json;
using System.Text.Json.Nodes;
using System.Web;

namespace The0;

/// <summary>
/// the0 Query Module - Express-like handler interface for bot queries.
///
/// Separate namespace from State and Input. Users explicitly use:
/// <code>
/// using The0;
///
/// Query.Handler("/portfolio", req => {
///     var positions = State.Get&lt;List&lt;Position&gt;&gt;("positions") ?? new();
///     return new { positions, count = positions.Count };
/// });
///
/// Query.Run();
/// </code>
/// </summary>
public static class Query
{
    private static readonly Dictionary<string, Func<QueryRequest, object>> _handlers = new();
    private static Dictionary<string, string> _currentParams = new();
    private static JsonNode? _config;

    /// <summary>
    /// Request object passed to handlers (Express-like).
    /// </summary>
    public class QueryRequest
    {
        /// <summary>The query path being requested (e.g., "/portfolio")</summary>
        public string Path { get; }

        /// <summary>
        /// Every parameter as text: a string as itself, any other JSON value as
        /// its JSON text. A null parameter is left out, so defaults apply.
        /// </summary>
        public Dictionary<string, string> Params { get; }

        /// <summary>Every parameter as its JSON value.</summary>
        public JsonObject JsonParams { get; }

        /// <summary>
        /// Create a request whose parameters are all strings.
        /// </summary>
        /// <param name="path">The query path</param>
        /// <param name="params">The parameters</param>
        public QueryRequest(string path, Dictionary<string, string> @params)
        {
            Path = path;
            Params = @params;
            JsonParams = new JsonObject(@params.Select(param =>
                KeyValuePair.Create(param.Key, (JsonNode?)JsonValue.Create(param.Value))));
        }

        /// <summary>
        /// Create a request whose parameters are JSON values.
        /// </summary>
        /// <param name="path">The query path</param>
        /// <param name="params">The parameters as a JSON object</param>
        public QueryRequest(string path, JsonObject @params)
        {
            Path = path;
            JsonParams = @params;
            Params = @params
                .Where(param => param.Value is not null)
                .ToDictionary(param => param.Key, param => AsText(param.Value!));
        }

        private static string AsText(JsonNode value) =>
            value.GetValueKind() == JsonValueKind.String ? value.GetValue<string>() : value.ToJsonString();

        /// <summary>
        /// Get a parameter as a <typeparamref name="T"/>, deserialized from its JSON value.
        /// </summary>
        /// <typeparam name="T">The type to deserialize to</typeparam>
        /// <param name="key">The parameter key</param>
        /// <param name="defaultValue">The value to return when the parameter is missing</param>
        /// <returns>The parameter's value, or the default</returns>
        /// <exception cref="JsonException">The parameter's value is not a <typeparamref name="T"/></exception>
        public T? Get<T>(string key, T? defaultValue = default) =>
            JsonParams.TryGetPropertyValue(key, out var value) ? value.Deserialize<T>() : defaultValue;

        /// <summary>
        /// Get a parameter as text: a string as itself, any other JSON value as its JSON text.
        /// </summary>
        /// <param name="key">The parameter key</param>
        /// <returns>The parameter value or null if not found</returns>
        public string? Get(string key) =>
            Params.TryGetValue(key, out var value) ? value : null;

        /// <summary>
        /// Get a parameter as text, with a default: a string as itself, any other JSON value as its JSON text.
        /// </summary>
        /// <param name="key">The parameter key</param>
        /// <param name="defaultValue">The default value if not found</param>
        /// <returns>The parameter value or the default</returns>
        public string Get(string key, string defaultValue) =>
            Params.TryGetValue(key, out var value) ? value : defaultValue;
    }

    /// <summary>
    /// Register a query handler (Express-like).
    /// </summary>
    /// <param name="path">The query path to handle (e.g., "/portfolio", "/signals")</param>
    /// <param name="handler">
    /// Handler function that receives request and returns response object. A handler that
    /// throws answers {"status": "error", "error": message}.
    /// </param>
    /// <example>
    /// <code>
    /// Query.Handler("/portfolio", req => {
    ///     var symbol = req.Get("symbol", "BTC/USD");
    ///     return new { symbol, positions = new List&lt;string&gt;() };
    /// });
    /// </code>
    /// </example>
    public static void Handler(string path, Func<QueryRequest, object> handler)
    {
        _handlers[path] = handler;
    }

    /// <summary>
    /// Get current query parameters (alternative to request object).
    /// </summary>
    /// <returns>A copy of the current query parameters</returns>
    public static Dictionary<string, string> GetParams() =>
        new(_currentParams);

    /// <summary>
    /// Get the bot configuration.
    /// </summary>
    /// <returns>The bot configuration as JsonNode</returns>
    public static JsonNode? GetConfig() => _config;

    /// <summary>
    /// Check if currently running in query mode (read-only).
    /// Used by State module to enforce read-only behavior.
    /// </summary>
    /// <returns>True if in query mode</returns>
    public static bool IsQueryMode() =>
        !string.IsNullOrEmpty(Environment.GetEnvironmentVariable("QUERY_PATH"));

    /// <summary>
    /// Run the query system with automatic mode detection.
    ///
    /// Modes:
    /// - QUERY_PATH env set: Ephemeral mode (execute once, output JSON, exit)
    /// - BOT_TYPE=realtime: Server mode (HTTP server on port 9476)
    /// - Neither: Info mode (print available handlers)
    /// </summary>
    public static void Run()
    {
        // Load bot config from environment
        var configStr = Environment.GetEnvironmentVariable("BOT_CONFIG") ?? "{}";
        try
        {
            _config = JsonNode.Parse(configStr);
        }
        catch
        {
            _config = JsonNode.Parse("{}");
        }

        // Register built-in handlers
        if (!_handlers.ContainsKey("/health"))
        {
            _handlers["/health"] = _ => new { status = "ok" };
        }
        if (!_handlers.ContainsKey("/info"))
        {
            _handlers["/info"] = _ => new { available_queries = _handlers.Keys.ToList() };
        }

        var queryPath = Environment.GetEnvironmentVariable("QUERY_PATH");
        var botType = Environment.GetEnvironmentVariable("BOT_TYPE");

        if (!string.IsNullOrEmpty(queryPath))
        {
            RunEphemeral(queryPath);
        }
        else if (botType == "realtime")
        {
            RunServer();
        }
        else
        {
            RunEphemeral("/info");
        }
    }

    /// <summary>
    /// Where an ephemeral query writes its result. Settable so the contract
    /// tests can point it into a sandbox; the runtime always reads
    /// /query/result.json.
    /// </summary>
    internal static string ResultPath { get; set; } = "/query/result.json";

    /// <summary>
    /// Write query result to /query/result.json file (matches Python SDK behavior).
    /// This avoids stdout pollution from runtime logs mixing with query results.
    /// </summary>
    private static void WriteResult(object result)
    {
        try
        {
            Directory.CreateDirectory(Path.GetDirectoryName(ResultPath)!);
            File.WriteAllText(ResultPath, JsonSerializer.Serialize(result));
        }
        catch (Exception e)
        {
            Console.Error.WriteLine($"RESULT_ERROR: Failed to write result file: {e.Message}");
        }
    }

    /// <summary>
    /// The params in a JSON text, or none when it is missing, is not JSON or is not an object.
    /// </summary>
    private static JsonObject ParamsObject(string? json)
    {
        try
        {
            return JsonNode.Parse(json ?? "{}") as JsonObject ?? new JsonObject();
        }
        catch (JsonException)
        {
            return new JsonObject();
        }
    }

    /// <summary>
    /// Execute single query and write result to /query/result.json.
    /// </summary>
    private static void RunEphemeral(string queryPath)
    {
        var request = new QueryRequest(queryPath, ParamsObject(Environment.GetEnvironmentVariable("QUERY_PARAMS")));
        _currentParams = request.Params;

        // Find and execute handler
        if (!_handlers.TryGetValue(queryPath, out var handler))
        {
            var result = new
            {
                status = "error",
                error = $"No handler for path: {queryPath}",
                available = _handlers.Keys.ToList()
            };
            WriteResult(result);
            Environment.Exit(1);
            return;
        }

        try
        {
            var data = handler(request);
            WriteResult(new { status = "ok", data });
        }
        catch (Exception e)
        {
            WriteResult(new { status = "error", error = e.Message });
            Environment.Exit(1);
        }
    }

    /// <summary>
    /// Start HTTP server on port 9476 for realtime bots.
    /// </summary>
    private static void RunServer()
    {
        var port = Environment.GetEnvironmentVariable("THE0_QUERY_PORT") ?? "9476";
        var prefix = $"http://*:{port}/";

        var listener = new HttpListener();
        listener.Prefixes.Add(prefix);
        listener.Start();

        Input.Log($"Query server started on port {port}");

        while (true)
        {
            try
            {
                var context = listener.GetContext();
                HandleRequest(context);
            }
            catch (Exception e)
            {
                Console.Error.WriteLine(JsonSerializer.Serialize(new
                {
                    _log = "error",
                    message = $"Error handling request: {e.Message}"
                }));
            }
        }
    }

    /// <summary>
    /// The params in a request body that is a JSON object, or null when there is no such body.
    /// </summary>
    private static JsonObject? BodyParams(HttpListenerRequest request)
    {
        if (!request.HasEntityBody)
        {
            return null;
        }
        using var reader = new StreamReader(request.InputStream, System.Text.Encoding.UTF8);
        try
        {
            return JsonNode.Parse(reader.ReadToEnd()) as JsonObject;
        }
        catch (JsonException)
        {
            return null;
        }
    }

    /// <summary>
    /// The params in a URL query string, URL-decoded, every value a string.
    /// </summary>
    private static Dictionary<string, string> QueryStringParams(string queryString)
    {
        var queryParams = new Dictionary<string, string>();
        if (!string.IsNullOrEmpty(queryString) && queryString.StartsWith("?"))
        {
            var parsed = HttpUtility.ParseQueryString(queryString);
            foreach (string? key in parsed.AllKeys)
            {
                if (key != null)
                {
                    queryParams[key] = parsed[key] ?? "";
                }
            }
        }
        return queryParams;
    }

    /// <summary>
    /// Handle an HTTP request.
    /// </summary>
    private static void HandleRequest(HttpListenerContext context)
    {
        var httpRequest = context.Request;
        var response = context.Response;

        var path = httpRequest.Url?.AbsolutePath ?? "/";
        var queryRequest = BodyParams(httpRequest) is JsonObject body
            ? new QueryRequest(path, body)
            : new QueryRequest(path, QueryStringParams(httpRequest.Url?.Query ?? ""));
        _currentParams = queryRequest.Params;

        string responseBody;
        int statusCode;

        if (!_handlers.TryGetValue(path, out var handler))
        {
            statusCode = 404;
            responseBody = JsonSerializer.Serialize(new
            {
                status = "error",
                error = $"No handler for path: {path}",
                available = _handlers.Keys.ToList()
            });
        }
        else
        {
            try
            {
                var data = handler(queryRequest);
                statusCode = 200;
                responseBody = JsonSerializer.Serialize(new { status = "ok", data });
            }
            catch (Exception e)
            {
                statusCode = 500;
                responseBody = JsonSerializer.Serialize(new { status = "error", error = e.Message });
            }
        }

        response.StatusCode = statusCode;
        response.ContentType = "application/json";
        var buffer = System.Text.Encoding.UTF8.GetBytes(responseBody);
        response.ContentLength64 = buffer.Length;
        response.OutputStream.Write(buffer, 0, buffer.Length);
        response.OutputStream.Close();
    }
}
