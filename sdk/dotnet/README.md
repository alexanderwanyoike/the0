# the0 C#/.NET SDK

SDK for building trading bots on the0 platform using C#/.NET 8.

## Installation

Add to your project:

```bash
dotnet add package The0
```

Or add to your `.csproj`:

```xml
<PackageReference Include="The0" Version="0.1.0" />
```

## Usage

```csharp
using The0;

// Parse bot configuration
var (botId, config) = Input.Parse();

Console.Error.WriteLine($"Bot {botId} starting...");

// Access configuration values
var symbol = config?["symbol"]?.ToString() ?? "BTC/USDT";
var amount = config?["amount"]?.GetValue<decimal>() ?? 100m;

// Your trading logic here

// Signal success when done
Input.Success("Bot executed successfully");
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

## License

Apache-2.0
