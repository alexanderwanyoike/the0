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
    public static void Success(string message)
    {
        var escaped = message.Replace("\\", "\\\\").Replace("\"", "\\\"");
        WriteResult($"{{\"status\":\"success\",\"message\":\"{escaped}\"}}");
    }

    /// <summary>
    /// Output an error result to the result file and exit with code 1.
    /// Writes a JSON object with status "error" and the provided message,
    /// then terminates the process with exit code 1.
    /// </summary>
    /// <param name="message">The error message to include in the output</param>
    public static void Error(string message)
    {
        var escaped = message.Replace("\\", "\\\\").Replace("\"", "\\\"");
        WriteResult($"{{\"status\":\"error\",\"message\":\"{escaped}\"}}");
        Environment.Exit(1);
    }

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
        jsonObj["timestamp"] = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds().ToString() + "Z";
        Console.WriteLine(jsonObj.ToJsonString());
    }

    /// <summary>
    /// Log a message to stdout.
    /// </summary>
    /// <param name="message">The message to log</param>
    public static void Log(string message)
    {
        var escaped = message.Replace("\\", "\\\\").Replace("\"", "\\\"");
        Console.WriteLine($"{{\"message\":\"{escaped}\"}}");
    }
}
