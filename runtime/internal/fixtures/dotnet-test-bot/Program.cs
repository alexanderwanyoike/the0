using System.Text.Json;
using System.Text.Json.Nodes;

// Get BOT_ID and BOT_CONFIG from environment
var botId = Environment.GetEnvironmentVariable("BOT_ID") ?? "unknown";
var configStr = Environment.GetEnvironmentVariable("BOT_CONFIG") ?? "{}";

// Parse config
JsonNode? config = null;
try
{
    config = JsonNode.Parse(configStr);
}
catch
{
    config = JsonNode.Parse("{}");
}

// Print inputs to stderr for test verification
Console.Error.WriteLine($"BOT_ID: {botId}");
Console.Error.WriteLine($"BOT_CONFIG: {config}");

// Output success JSON to stdout
Console.WriteLine($"{{\"status\":\"success\",\"message\":\"C# bot executed\",\"bot_id\":\"{botId}\"}}");
