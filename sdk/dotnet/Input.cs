using System.Text.Json;
using System.Text.Json.Nodes;

namespace The0;

/// <summary>
/// Input parsing and output formatting utilities for the0 bots.
/// </summary>
public static class Input
{
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
    /// Output a success result to stdout.
    /// Prints a JSON object with status "success" and the provided message.
    /// </summary>
    /// <param name="message">The success message to include in the output</param>
    public static void Success(string message)
    {
        var escaped = message.Replace("\\", "\\\\").Replace("\"", "\\\"");
        Console.WriteLine($"{{\"status\":\"success\",\"message\":\"{escaped}\"}}");
    }

    /// <summary>
    /// Output an error result to stdout and exit with code 1.
    /// Prints a JSON object with status "error" and the provided message,
    /// then terminates the process with exit code 1.
    /// </summary>
    /// <param name="message">The error message to include in the output</param>
    public static void Error(string message)
    {
        var escaped = message.Replace("\\", "\\\\").Replace("\"", "\\\"");
        Console.WriteLine($"{{\"status\":\"error\",\"message\":\"{escaped}\"}}");
        Environment.Exit(1);
    }

    /// <summary>
    /// Output a custom JSON result to stdout.
    /// Serializes the provided object as JSON and prints it.
    /// </summary>
    /// <param name="data">The object to serialize and output</param>
    public static void Result(object data)
    {
        Console.WriteLine(JsonSerializer.Serialize(data));
    }
}
