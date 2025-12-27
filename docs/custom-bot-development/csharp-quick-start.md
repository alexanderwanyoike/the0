---
title: "C# Quick Start"
description: "Build your first C# trading bot with the0"
tags: ["custom-bots", "csharp", "dotnet", "quick-start"]
order: 12
---

# C# Quick Start Guide

Build trading bots in C# with the0's .NET 8 runtime. C# offers excellent productivity with strong typing, modern async patterns, and a rich ecosystem of financial libraries.

---

## Why C# for Trading Bots?

C# and .NET provide a compelling platform for trading applications:

- **Strong Typing**: Catch configuration errors at compile time
- **Async/Await**: First-class support for concurrent API calls
- **Rich Ecosystem**: NuGet has packages for every broker and exchange
- **Cross-Platform**: .NET 8 runs natively on Linux, macOS, and Windows
- **Enterprise Ready**: Battle-tested in production financial systems
- **Excellent Tooling**: Visual Studio, Rider, and VS Code support

**When to Choose C#:**
- Building on existing .NET infrastructure
- Need strong typing with good IDE support
- Integrating with Windows-based trading systems
- Teams familiar with C# or Java

**Popular NuGet Packages for Trading:**
- `Alpaca.Markets` - Official Alpaca SDK for stocks/crypto
- `ExchangeSharpLib` - Multi-exchange trading library
- `TA-Lib.NETCore` - Technical analysis indicators
- `Newtonsoft.Json` / `System.Text.Json` - JSON handling
- `Polly` - Resilience and retry policies

---

## Prerequisites

- .NET 8 SDK installed ([download](https://dotnet.microsoft.com/download))
- the0 CLI installed
- Valid the0 API key
- Basic understanding of C# and async programming

---

## Project Structure

```
my-csharp-bot/
├── MyCsharpBot.csproj    # Project file with dependencies
├── Program.cs            # Your bot entry point
├── bot-config.yaml       # Bot configuration
├── bot-schema.json       # Parameter schema
├── config.json           # Example configuration
└── README.md             # Documentation
```

---

## Step 1: Create Your Project

```bash
# Create a new .NET console project
dotnet new console -n MyCsharpBot
cd MyCsharpBot
```

This creates a minimal project with `Program.cs` and `.csproj` files.

---

## Step 2: Add the SDK

Install the the0 SDK from NuGet:

```bash
dotnet add package The0.Sdk
```

Or add it directly to your `.csproj`:

```xml
<!-- MyCsharpBot.csproj -->
<Project Sdk="Microsoft.NET.Sdk">

  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net8.0</TargetFramework>
    <ImplicitUsings>enable</ImplicitUsings>
    <Nullable>enable</Nullable>
  </PropertyGroup>

  <ItemGroup>
    <PackageReference Include="The0.Sdk" Version="0.1.2" />
  </ItemGroup>

</Project>
```

---

## Step 3: Write Your Bot

Create `Program.cs`:

```csharp
using The0;

/// <summary>
/// Main entry point for the trading bot.
///
/// The the0 SDK handles:
/// - Parsing BOT_ID and BOT_CONFIG from environment
/// - JSON configuration with System.Text.Json
/// - Writing results to the correct output file
/// </summary>

// Parse bot configuration from environment
var (botId, config) = Input.Parse();

Console.Error.WriteLine($"Bot {botId} starting...");

// Access configuration with null-safe operators
// config is a JsonNode? so we use ?. and ?? for safety
var symbol = config?["symbol"]?.ToString() ?? "BTC/USDT";
var amount = config?["amount"]?.GetValue<double>() ?? 100.0;

Console.Error.WriteLine($"Trading {symbol} with amount {amount}");

// ===========================================
// YOUR TRADING LOGIC GOES HERE
// ===========================================

try
{
    // Example: Validate configuration
    if (amount <= 0)
    {
        Input.Error("Amount must be positive");
        return; // Won't reach here - Error exits
    }

    // Example: Fetch price (implement with HttpClient)
    // var price = await FetchPrice(symbol);

    // Example: Execute trade
    // var orderId = await PlaceOrder(symbol, amount);

    // ===========================================
    // END OF TRADING LOGIC
    // ===========================================

    // Signal success with additional data
    Input.Result(new
    {
        status = "success",
        message = "Trade executed",
        data = new
        {
            symbol,
            amount,
            timestamp = DateTime.UtcNow.ToString("O")
        }
    });
}
catch (Exception ex)
{
    Input.Error($"Trade failed: {ex.Message}");
}
```

---

## Step 4: Create Bot Configuration

Create `bot-config.yaml`:

```yaml
name: my-csharp-bot
description: "A C# trading bot with async support"
version: "1.0.0"
author: "Your Name"
type: scheduled
runtime: dotnet8

# The entrypoint is the source file
entrypoints:
  bot: Program.cs

schema:
  bot: bot-schema.json

readme: README.md

metadata:
  categories: [trading]
  instruments: [crypto, stocks]
  tags: [csharp, dotnet, async]
```

**Note:** The `runtime: dotnet8` tells the platform to build with .NET 8. You don't need to compile locally.

---

## Step 5: Define Parameter Schema

Create `bot-schema.json`:

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "C# Bot Configuration",
  "description": "Configuration for the C# trading bot",
  "properties": {
    "symbol": {
      "type": "string",
      "title": "Trading Symbol",
      "description": "The trading pair (e.g., BTC/USDT, AAPL)",
      "default": "BTC/USDT"
    },
    "amount": {
      "type": "number",
      "title": "Trade Amount",
      "description": "Amount in USD to trade",
      "default": 100,
      "minimum": 1
    },
    "api_key": {
      "type": "string",
      "title": "API Key",
      "description": "Your exchange API key"
    },
    "api_secret": {
      "type": "string",
      "title": "API Secret",
      "description": "Your exchange API secret"
    },
    "paper": {
      "type": "boolean",
      "title": "Paper Trading",
      "description": "Use paper trading mode (no real money)",
      "default": true
    }
  },
  "required": ["symbol", "api_key"]
}
```

---

## Step 6: Test Locally

```bash
# Set environment variables for testing
export BOT_ID="test-bot-123"
export BOT_CONFIG='{"symbol":"BTC/USDT","amount":100,"paper":true}'
export CODE_MOUNT_DIR="/tmp"

# Run the bot
dotnet run
```

---

## Step 7: Deploy

```bash
the0 custom-bot deploy
```

The platform will:
1. Build your .NET project with `dotnet publish`
2. Package the output
3. Deploy to the runtime environment

No need to compile locally - it all happens in the cloud!

---

## SDK API Reference

The `The0` namespace provides these methods in the `Input` class:

### `Input.Parse() -> (string BotId, JsonNode? Config)`

Parse bot configuration from environment:

```csharp
var (botId, config) = Input.Parse();

// Access values with null-safe operators
var symbol = config?["symbol"]?.ToString() ?? "BTC/USDT";
var amount = config?["amount"]?.GetValue<double>() ?? 100.0;
var enabled = config?["enabled"]?.GetValue<bool>() ?? true;

// Check nested values
var apiKey = config?["credentials"]?["api_key"]?.ToString();
```

### `Input.ParseAsDict() -> (string BotId, Dictionary<string, JsonNode?> Config)`

Parse configuration as a Dictionary for iteration:

```csharp
var (botId, config) = Input.ParseAsDict();

// Iterate over all config values
foreach (var (key, value) in config)
{
    Console.Error.WriteLine($"{key}: {value}");
}

// Use TryGetValue for safe access
if (config.TryGetValue("symbol", out var symbolNode))
{
    var symbol = symbolNode?.ToString();
}
```

### `Input.Success(string message)`

Output a success result:

```csharp
Input.Success("Trade executed successfully");
// Outputs: {"status":"success","message":"Trade executed successfully"}
```

### `Input.Error(string message)`

Output an error result and exit with code 1:

```csharp
if (amount <= 0)
{
    Input.Error("Amount must be positive");
    // Program exits here
}
// This line never reached if amount <= 0
```

### `Input.Result(object data)`

Output a custom JSON result:

```csharp
Input.Result(new
{
    status = "success",
    trade_id = "abc123",
    filled_amount = 0.5,
    average_price = 45123.50m
});
```

### `Input.Metric(string type, object data)`

Emit a metric for the platform to collect (written to stdout):

```csharp
// Emit a price metric
Input.Metric("price", new { symbol = "BTC/USD", value = 45000.0 });
// Output: {"_metric":"price","symbol":"BTC/USD","value":45000,"timestamp":"..."}

// Emit a trade metric
Input.Metric("trade", new { symbol = "AAPL", quantity = 10, price = 150.50 });
```

### `Input.Log(string message, object? data = null, LogLevel? level = null)`

Log a structured message to stderr:

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

Available log levels: `LogLevel.Info`, `LogLevel.Warn`, `LogLevel.Error`

---

## Example: Alpaca Trading Bot

Here's a complete example using the Alpaca API:

```csharp
using The0;
using System.Net.Http.Json;

var (botId, config) = Input.Parse();

var symbol = config?["symbol"]?.ToString() ?? "AAPL";
var amount = config?["amount"]?.GetValue<decimal>() ?? 100m;
var apiKey = config?["api_key"]?.ToString();
var apiSecret = config?["api_secret"]?.ToString();
var paper = config?["paper"]?.GetValue<bool>() ?? true;

if (string.IsNullOrEmpty(apiKey) || string.IsNullOrEmpty(apiSecret))
{
    Input.Error("API key and secret are required");
    return;
}

Console.Error.WriteLine($"Bot {botId} starting...");
Console.Error.WriteLine($"Trading {symbol} with ${amount} (paper: {paper})");

try
{
    // Create HTTP client for Alpaca
    using var client = new HttpClient();
    var baseUrl = paper
        ? "https://paper-api.alpaca.markets"
        : "https://api.alpaca.markets";

    client.DefaultRequestHeaders.Add("APCA-API-KEY-ID", apiKey);
    client.DefaultRequestHeaders.Add("APCA-API-SECRET-KEY", apiSecret);

    // Get current price
    var quoteUrl = $"https://data.alpaca.markets/v2/stocks/{symbol}/quotes/latest";
    var quoteResponse = await client.GetFromJsonAsync<AlpacaQuote>(quoteUrl);
    var price = quoteResponse?.Quote?.AskPrice ?? 0m;

    Console.Error.WriteLine($"Current price of {symbol}: ${price}");

    // Calculate quantity
    var quantity = Math.Floor(amount / price);
    if (quantity <= 0)
    {
        Input.Error($"Amount ${amount} not enough to buy 1 share at ${price}");
        return;
    }

    // Place market order
    var orderRequest = new
    {
        symbol = symbol,
        qty = quantity.ToString(),
        side = "buy",
        type = "market",
        time_in_force = "day"
    };

    var orderResponse = await client.PostAsJsonAsync(
        $"{baseUrl}/v2/orders",
        orderRequest
    );

    if (orderResponse.IsSuccessStatusCode)
    {
        var order = await orderResponse.Content.ReadFromJsonAsync<AlpacaOrder>();
        Console.Error.WriteLine($"Order placed: {order?.Id}");

        Input.Result(new
        {
            status = "success",
            message = $"Bought {quantity} shares of {symbol}",
            data = new
            {
                order_id = order?.Id,
                symbol,
                quantity,
                estimated_cost = quantity * price
            }
        });
    }
    else
    {
        var error = await orderResponse.Content.ReadAsStringAsync();
        Input.Error($"Order failed: {error}");
    }
}
catch (Exception ex)
{
    Input.Error($"Trade failed: {ex.Message}");
}

// Response models
record AlpacaQuote(QuoteData Quote);
record QuoteData(decimal AskPrice, decimal BidPrice);
record AlpacaOrder(string Id, string Status);
```

---

## Best Practices

### 1. Async Programming

Use async/await for non-blocking I/O:

```csharp
using The0;

var (botId, config) = Input.Parse();

try
{
    // Run async operations
    var result = await ExecuteTradeAsync(config);
    Input.Result(result);
}
catch (Exception ex)
{
    Input.Error(ex.Message);
}

async Task<object> ExecuteTradeAsync(System.Text.Json.Nodes.JsonNode? config)
{
    using var client = new HttpClient();

    // Parallel API calls
    var priceTask = client.GetStringAsync("https://api.example.com/price");
    var balanceTask = client.GetStringAsync("https://api.example.com/balance");

    await Task.WhenAll(priceTask, balanceTask);

    return new { price = priceTask.Result, balance = balanceTask.Result };
}
```

### 2. Error Handling

Use try-catch with specific exception types:

```csharp
using The0;

var (botId, config) = Input.Parse();

try
{
    // Your trading logic
    await ProcessTradeAsync();
    Input.Success("Trade completed");
}
catch (HttpRequestException ex)
{
    Input.Error($"Network error: {ex.Message}");
}
catch (JsonException ex)
{
    Input.Error($"Invalid response format: {ex.Message}");
}
catch (Exception ex)
{
    Input.Error($"Unexpected error: {ex.Message}");
}
```

### 3. Configuration Validation

Validate early and fail fast:

```csharp
using The0;

var (botId, config) = Input.Parse();

// Validate required fields
var apiKey = config?["api_key"]?.ToString();
if (string.IsNullOrEmpty(apiKey))
{
    Input.Error("Missing required field: api_key");
    return;
}

var amount = config?["amount"]?.GetValue<decimal>() ?? 0m;
if (amount <= 0)
{
    Input.Error("Amount must be positive");
    return;
}

// Continue with validated values
Console.Error.WriteLine($"Configuration validated: amount={amount}");
```

### 4. Logging

Use `Input.Log` for structured logging or `Console.Error` for simple messages:

```csharp
// Structured logging (recommended)
Input.Log("Starting trade", new { symbol, amount });
Input.Log("Price fetched", new { price = 45000.0 }, LogLevel.Info);
Input.Log("API rate limited", null, LogLevel.Warn);

// Simple logging
Console.Error.WriteLine($"DEBUG: price = {price}");
```

### 5. Decimal for Money

Always use `decimal` for financial calculations:

```csharp
// Good - precise decimal arithmetic
decimal price = 45123.50m;
decimal quantity = 0.5m;
decimal total = price * quantity;  // Exact: 22561.75

// Bad - floating point errors
double priceD = 45123.50;
double quantityD = 0.5;
double totalD = priceD * quantityD;  // May have rounding errors
```

---

## Adding Dependencies

Add NuGet packages to your `.csproj`:

```xml
<ItemGroup>
  <!-- JSON handling (built-in, but Newtonsoft is also popular) -->
  <PackageReference Include="Newtonsoft.Json" Version="13.0.3" />

  <!-- HTTP client enhancements -->
  <PackageReference Include="RestSharp" Version="110.2.0" />

  <!-- Alpaca trading SDK -->
  <PackageReference Include="Alpaca.Markets" Version="7.0.0" />

  <!-- Resilience and retry policies -->
  <PackageReference Include="Polly" Version="8.2.0" />

  <!-- Technical analysis -->
  <PackageReference Include="TALib.NETCore" Version="0.6.0" />
</ItemGroup>
```

---

## Related Documentation

- [Configuration Reference](/custom-bot-development/configuration)
- [Bot Types](/custom-bot-development/bot-types)
- [Custom Frontends](/custom-bot-development/custom-frontends)
- [Deployment Guide](/custom-bot-development/deployment)
