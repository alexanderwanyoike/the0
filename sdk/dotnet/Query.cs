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

        /// <summary>Dictionary of query parameters</summary>
        public Dictionary<string, string> Params { get; }

        public QueryRequest(string path, Dictionary<string, string> @params)
        {
            Path = path;
            Params = @params;
        }

        /// <summary>
        /// Get a parameter value.
        /// </summary>
        /// <param name="key">The parameter key</param>
        /// <returns>The parameter value or null if not found</returns>
        public string? Get(string key) =>
            Params.TryGetValue(key, out var value) ? value : null;

        /// <summary>
        /// Get a parameter value with a default.
        /// </summary>
        /// <param name="key">The parameter key</param>
        /// <param name="defaultValue">The default value if not found</param>
        /// <returns>The parameter value or the default</returns>
        public string Get(string key, string defaultValue) =>
            Params.TryGetValue(key, out var value) ? value : defaultValue;
    }

    /// <summary>
    /// Error thrown when attempting to modify state during query execution.
    /// </summary>
    public class ReadOnlyStateError : Exception
    {
        public ReadOnlyStateError(string message) : base(message) { }
    }

    /// <summary>
    /// Register a query handler (Express-like).
    /// </summary>
    /// <param name="path">The query path to handle (e.g., "/portfolio", "/signals")</param>
    /// <param name="handler">Handler function that receives request and returns response object</param>
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
    /// Execute single query and output JSON to stdout.
    /// </summary>
    private static void RunEphemeral(string queryPath)
    {
        // Parse parameters from environment
        var paramsStr = Environment.GetEnvironmentVariable("QUERY_PARAMS") ?? "{}";
        try
        {
            _currentParams = JsonSerializer.Deserialize<Dictionary<string, string>>(paramsStr) ?? new();
        }
        catch
        {
            _currentParams = new();
        }

        // Find and execute handler
        if (!_handlers.TryGetValue(queryPath, out var handler))
        {
            var result = new
            {
                status = "error",
                error = $"No handler for path: {queryPath}",
                available = _handlers.Keys.ToList()
            };
            Console.WriteLine(JsonSerializer.Serialize(result));
            Environment.Exit(1);
            return;
        }

        try
        {
            var request = new QueryRequest(queryPath, _currentParams);
            var data = handler(request);
            Console.WriteLine(JsonSerializer.Serialize(new { status = "ok", data }));
        }
        catch (Exception e)
        {
            Console.WriteLine(JsonSerializer.Serialize(new { status = "error", error = e.Message }));
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

        Console.Error.WriteLine(JsonSerializer.Serialize(new
        {
            _log = "info",
            message = $"Query server started on port {port}"
        }));

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
    /// Handle an HTTP request.
    /// </summary>
    private static void HandleRequest(HttpListenerContext context)
    {
        var request = context.Request;
        var response = context.Response;

        var path = request.Url?.AbsolutePath ?? "/";
        var queryString = request.Url?.Query ?? "";

        // Parse query parameters
        _currentParams = new Dictionary<string, string>();
        if (!string.IsNullOrEmpty(queryString) && queryString.StartsWith("?"))
        {
            var parsed = HttpUtility.ParseQueryString(queryString);
            foreach (string? key in parsed.AllKeys)
            {
                if (key != null)
                {
                    _currentParams[key] = parsed[key] ?? "";
                }
            }
        }

        string responseBody;
        int statusCode;

        if (!_handlers.TryGetValue(path, out var handler))
        {
            statusCode = 404;
            responseBody = JsonSerializer.Serialize(new
            {
                status = "error",
                error = $"No handler for path: {path}"
            });
        }
        else
        {
            try
            {
                var req = new QueryRequest(path, _currentParams);
                var data = handler(req);
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
