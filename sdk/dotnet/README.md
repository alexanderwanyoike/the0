# the0 C#/.NET SDK

Optional SDK for building trading bots on the0 platform using C#/.NET 8.

**Note:** This SDK is optional. The the0 runtime automatically wraps your bot's output:
- Exit code 0 → `{"status":"success","message":"Bot executed successfully"}`
- Exit code non-zero → `{"status":"error","message":"Bot execution failed"}`
- If you output JSON with a `"status"` field, it's passed through as-is

## Minimal Example (No SDK)

```csharp
// No SDK needed - just run your code and exit normally
var botId = Environment.GetEnvironmentVariable("BOT_ID");
var configJson = Environment.GetEnvironmentVariable("BOT_CONFIG");

Console.Error.WriteLine($"Bot {botId} running...");

// Your trading logic here

// Exit normally - runtime outputs {"status":"success",...}
```

## With SDK (Optional)

The SDK provides convenience methods for parsing config and outputting custom results.

### Installation

Install from NuGet:

```bash
dotnet add package The0.Sdk
```

Or add to your `.csproj`:

```xml
<PackageReference Include="The0.Sdk" Version="0.1.2" />
```

### Usage

```csharp
using The0;

// Parse bot configuration
var (botId, config) = Input.Parse();

Console.Error.WriteLine($"Bot {botId} starting...");

// Access configuration values
var symbol = config?["symbol"]?.ToString() ?? "BTC/USDT";
var amount = config?["amount"]?.GetValue<decimal>() ?? 100m;

// Your trading logic here

// Optionally output custom result (otherwise runtime generates it)
Input.Success("Trade executed for " + symbol);
```

## API Reference

### `Input.Parse()`

Parse bot configuration from environment variables.

```csharp
var (botId, config) = Input.Parse();
// botId: string - unique bot instance ID
// config: JsonNode? - configuration as JSON
```

### `Input.ParseAsDict()`

Parse configuration as a Dictionary for easier access.

```csharp
var (botId, config) = Input.ParseAsDict();
// config: Dictionary<string, JsonNode?>
if (config.TryGetValue("symbol", out var symbol))
{
    Console.Error.WriteLine($"Symbol: {symbol}");
}
```

### `Input.Success(message)`

Output a success result.

```csharp
Input.Success("Trade executed successfully");
// Output: {"status":"success","message":"Trade executed successfully"}
```

### `Input.Error(message)`

Output an error result and exit with code 1.

```csharp
Input.Error("Failed to connect to exchange");
// Output: {"status":"error","message":"Failed to connect to exchange"}
// Process exits with code 1
```

### `Input.Result(data)`

Output a custom JSON result.

```csharp
Input.Result(new {
    status = "success",
    trade_id = "12345",
    filled_amount = 0.5m
});
```

### `Input.Metric(type, data)`

Emit a metric for the platform to collect.

```csharp
Input.Metric("price", new { symbol = "BTC/USD", value = 45000.0 });
// Output to stdout: {"_metric":"price","symbol":"BTC/USD","value":45000,"timestamp":"..."}
```

### `Input.Log(message, data?, level?)`

Log a structured message to stderr.

```csharp
// Simple log (defaults to info level)
Input.Log("Starting bot");

// Log with level
Input.Log("Connection lost", null, LogLevel.Warn);

// Log with structured data
Input.Log("Order placed", new { orderId = "12345", symbol = "BTC" });

// Log with data and level
Input.Log("Order failed", new { orderId = "12345" }, LogLevel.Error);
```

## Environment Variables

The SDK reads from these environment variables (set automatically by the0 runtime):

- `BOT_ID` - Unique identifier for the bot instance
- `BOT_CONFIG` - JSON-encoded configuration object

## Logging

Use `Console.Error` for logs (stdout is reserved for results):

```csharp
Console.Error.WriteLine("DEBUG: Processing trade...");  // Logs to stderr
Console.WriteLine("...");  // Reserved for JSON result output
```

## Publishing (Maintainers)

This package is published to NuGet.org.

```bash
export NUGET_API_KEY="your_key_here"
make publish
```

Get your API key from: https://www.nuget.org/account/apikeys

## License

Apache-2.0
