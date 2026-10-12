using System.Globalization;
using System.Text.Json;
using System.Text.Json.Nodes;

namespace The0;

/// <summary>
/// Input parsing and output formatting utilities for the0 bots.
/// </summary>
public static class Input
{
    /// <summary>
    /// Get the path to the result file.
    /// </summary>
    private static string ResultFilePath
    {
        get
        {
            var mountDir = Environment.GetEnvironmentVariable("CODE_MOUNT_DIR") ?? "bot";
            return $"/{mountDir}/result.json";
        }
    }

    /// <summary>
    /// The current time as a UTC ISO 8601 string, the form metric and log lines carry.
    /// </summary>
    private static string Timestamp() =>
        DateTime.UtcNow.ToString("yyyy-MM-dd'T'HH:mm:ss.fff'Z'", CultureInfo.InvariantCulture);

    /// <summary>
    /// Write result to the result file.
    /// </summary>
    private static void WriteResult(string content)
    {
        try
        {
            File.WriteAllText(ResultFilePath, content);
        }
        catch (Exception e)
        {
            Console.Error.WriteLine($"RESULT_ERROR: Failed to write result file: {e.Message}");
        }
    }

    /// <summary>
    /// Parse bot configuration from environment variables.
    /// Returns a tuple of (BotId, Config) where Config is a JsonNode.
    /// </summary>
    /// <returns>Tuple containing bot ID and configuration as JsonNode</returns>
    /// <exception cref="InvalidOperationException">Thrown when required environment variables are not set</exception>
    public static (string BotId, JsonNode? Config) Parse()
    {
        var id = Environment.GetEnvironmentVariable("BOT_ID")
            ?? throw new InvalidOperationException("BOT_ID environment variable not set");
        var configStr = Environment.GetEnvironmentVariable("BOT_CONFIG")
            ?? throw new InvalidOperationException("BOT_CONFIG environment variable not set");
        var config = JsonNode.Parse(configStr);
        return (id, config);
    }

    /// <summary>
    /// Parse bot configuration as a Dictionary for easier access.
    /// Returns a tuple of (BotId, Config) where Config is a Dictionary.
    /// </summary>
    /// <returns>Tuple containing bot ID and configuration as Dictionary</returns>
    /// <exception cref="InvalidOperationException">Thrown when required environment variables are not set</exception>
    public static (string BotId, Dictionary<string, JsonNode?> Config) ParseAsDict()
    {
        var id = Environment.GetEnvironmentVariable("BOT_ID")
            ?? throw new InvalidOperationException("BOT_ID environment variable not set");
        var configStr = Environment.GetEnvironmentVariable("BOT_CONFIG")
            ?? throw new InvalidOperationException("BOT_CONFIG environment variable not set");
        var config = JsonSerializer.Deserialize<Dictionary<string, JsonNode?>>(configStr)
            ?? new Dictionary<string, JsonNode?>();
        return (id, config);
    }

    /// <summary>
    /// Output a success result to the result file.
    /// Writes a JSON object with status "success" and the provided message.
    /// </summary>
    /// <param name="message">The success message to include in the output</param>
    public static void Success(string message) => Success(message, null);

    /// <summary>
    /// Output a success result with data to the result file.
    /// Writes a JSON object with status "success", the message and, unless
    /// <paramref name="data"/> is null, the data under "data".
    /// </summary>
    /// <param name="message">The success message to include in the output</param>
    /// <param name="data">Any JSON-serializable value, or null for none</param>
    public static void Success(string message, object? data)
    {
        WriteResult(StatusResult("success", message, data));
    }

    private static string StatusResult(string status, string message, object? data)
    {
        var result = new JsonObject { ["status"] = status, ["message"] = message };
        if (data != null)
        {
            result["data"] = JsonSerializer.SerializeToNode(data);
        }
        return result.ToJsonString();
    }

    /// <summary>
    /// Output an error result to the result file, log the message and exit with code 1.
    /// Writes a JSON object with status "error" and the provided message,
    /// prints the message to stderr as an error log line,
    /// then terminates the process with exit code 1.
    /// </summary>
    /// <param name="message">The error message to include in the output</param>
    public static void Error(string message)
    {
        WriteResult(JsonSerializer.Serialize(new { status = "error", message }));
        Log(message, level: LogLevel.Error);
        Environment.Exit(1);
    }

    /// <summary>
    /// Output an error result with data to the result file and exit with code 1.
    /// Writes a JSON object with status "error", the message and, unless
    /// <paramref name="data"/> is null, the data under "data".
    /// </summary>
    /// <param name="message">The error message to include in the output</param>
    /// <param name="data">Any JSON-serializable value, or null for none</param>
    public static void Error(string message, object? data) => Error(message);

    /// <summary>
    /// Output a custom JSON result to the result file.
    /// Serializes the provided object as JSON and writes it.
    /// </summary>
    /// <param name="data">The object to serialize and output</param>
    public static void Result(object data)
    {
        WriteResult(JsonSerializer.Serialize(data));
    }

    /// <summary>
    /// Emit a metric to stdout with timestamp.
    /// </summary>
    /// <param name="metricType">The type of metric (e.g., "price", "signal")</param>
    /// <param name="data">The metric data object</param>
    public static void Metric(string metricType, object data)
    {
        var jsonObj = JsonSerializer.SerializeToNode(data)?.AsObject() ?? new JsonObject();
        jsonObj["_metric"] = metricType;
        jsonObj["timestamp"] = Timestamp();
        Console.WriteLine(jsonObj.ToJsonString());
    }

    /// <summary>
    /// Log levels supported by the platform.
    /// </summary>
    public enum LogLevel
    {
        Info,
        Warn,
        Error
    }

    /// <summary>
    /// Log a structured message to stderr.
    /// Outputs a JSON object with level, message, timestamp, and any additional fields.
    /// </summary>
    /// <param name="message">The message to log</param>
    /// <param name="data">Optional object with additional fields (null for none)</param>
    /// <param name="level">Optional log level (defaults to Info)</param>
    /// <example>
    /// // Simple log (defaults to info level)
    /// Input.Log("Starting trade execution");
    ///
    /// // Log with level
    /// Input.Log("Connection lost", level: LogLevel.Warn);
    ///
    /// // Log with structured data
    /// Input.Log("Order placed", new { order_id = "12345", symbol = "BTC/USD" });
    ///
    /// // Log with data and level
    /// Input.Log("Order failed", new { order_id = "12345" }, LogLevel.Error);
    /// </example>
    public static void Log(string message, object? data = null, LogLevel level = LogLevel.Info)
    {
        var jsonObj = data != null
            ? JsonSerializer.SerializeToNode(data)?.AsObject() ?? new JsonObject()
            : new JsonObject();

        jsonObj["level"] = level.ToString().ToLower();
        jsonObj["message"] = message;
        jsonObj["timestamp"] = Timestamp();

        Console.Error.WriteLine(jsonObj.ToJsonString());
    }
}
