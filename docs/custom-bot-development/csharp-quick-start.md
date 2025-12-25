---
title: "C# Quick Start"
description: "Build your first C# trading bot with the0"
tags: ["custom-bots", "csharp", "dotnet", "quick-start"]
order: 12
---

# C# Quick Start Guide

Build a trading bot in C# with the0's .NET 8 SDK.

---

## Prerequisites

- .NET 8 SDK installed
- the0 CLI installed
- Valid the0 API key

---

## Project Structure

```
my-csharp-bot/
├── MyCsharpBot.csproj    # Project file with dependencies
├── Program.cs            # Your bot entry point
├── bot-config.yaml       # Bot configuration
├── bot-schema.json       # Parameter schema
└── README.md             # Documentation
```

---

## Step 1: Create Your Project

```bash
# Create a new .NET console project
dotnet new console -n MyCsharpBot
cd MyCsharpBot
```

---

## Step 2: Configure the Project

Update `MyCsharpBot.csproj`:

```xml
<Project Sdk="Microsoft.NET.Sdk">

  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net8.0</TargetFramework>
    <ImplicitUsings>enable</ImplicitUsings>
    <Nullable>enable</Nullable>
  </PropertyGroup>

  <ItemGroup>
    <PackageReference Include="The0" Version="0.1.0" />
  </ItemGroup>

</Project>
```

Or add the SDK from source:

```xml
<ItemGroup>
  <Compile Include="path/to/sdk/dotnet/Input.cs" />
  <PackageReference Include="System.Text.Json" Version="8.0.0" />
</ItemGroup>
```

---

## Step 3: Write Your Bot

Create `Program.cs`:

```csharp
using The0;

// Parse bot configuration
var (botId, config) = Input.Parse();

Console.Error.WriteLine($"Bot {botId} starting...");

// Access configuration values
var symbol = config?["symbol"]?.ToString() ?? "BTC/USDT";
var amount = config?["amount"]?.GetValue<double>() ?? 100.0;

Console.Error.WriteLine($"Trading {symbol} with amount {amount}");

// Your trading logic here
// Example: Check price, execute trade, log results

// Signal success when done
Input.Success("Bot executed successfully");
```

---

## Step 4: Create Bot Configuration

Create `bot-config.yaml`:

```yaml
name: my-csharp-bot
description: "A C# trading bot"
version: "1.0.0"
author: "Your Name"
type: scheduled
runtime: dotnet8

entrypoints:
  bot: Program.cs

schema:
  bot: bot-schema.json

readme: README.md
```

---

## Step 5: Define Parameter Schema

Create `bot-schema.json`:

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "Bot Configuration",
  "description": "Configuration for the C# trading bot",
  "properties": {
    "symbol": {
      "type": "string",
      "title": "Trading Symbol",
      "description": "The trading pair symbol",
      "default": "BTC/USDT"
    },
    "amount": {
      "type": "number",
      "title": "Trade Amount",
      "description": "Amount to trade per execution",
      "default": 100
    }
  },
  "required": ["symbol"]
}
```

---

## Step 6: Deploy

```bash
# Deploy your bot
the0 custom-bot deploy
```

The build happens automatically in the cloud - no need to compile locally!

---

## SDK API Reference

The `The0` namespace provides these methods in the `Input` class:

### `Input.Parse() -> (string BotId, JsonNode? Config)`

Parse bot configuration from environment. Returns a tuple of `(BotId, Config)`.

```csharp
var (botId, config) = Input.Parse();
var symbol = config?["symbol"]?.ToString();
```

### `Input.ParseAsDict() -> (string BotId, Dictionary<string, JsonNode?> Config)`

Parse configuration as a Dictionary for easier access.

```csharp
var (botId, config) = Input.ParseAsDict();
if (config.TryGetValue("symbol", out var symbolNode))
{
    Console.WriteLine($"Symbol: {symbolNode}");
}
```

### `Input.Success(string message)`

Output a success result.

```csharp
Input.Success("Trade executed successfully");
```

### `Input.Error(string message)`

Output an error result and exit with code 1.

```csharp
Input.Error("Failed to connect to exchange");
```

### `Input.Result(object data)`

Output a custom JSON result.

```csharp
Input.Result(new {
    status = "success",
    trade_id = "12345",
    filled_amount = 0.5
});
```

---

## Adding Dependencies

Add any NuGet package to your `.csproj`:

```xml
<ItemGroup>
  <PackageReference Include="The0" Version="0.1.0" />
  <PackageReference Include="Newtonsoft.Json" Version="13.0.3" />
  <PackageReference Include="RestSharp" Version="110.2.0" />
</ItemGroup>
```

The CLI builds your bot before deployment - no need to compile locally!

---

## Example: HTTP Request Bot

```csharp
using System.Net.Http.Json;
using The0;

var (botId, config) = Input.Parse();

// Get API endpoint from config
var endpoint = config?["api_endpoint"]?.ToString()
    ?? "https://api.example.com/price";

Console.Error.WriteLine($"Bot {botId} fetching from {endpoint}");

try
{
    using var httpClient = new HttpClient();
    var response = await httpClient.GetAsync(endpoint);

    if (response.IsSuccessStatusCode)
    {
        var data = await response.Content.ReadAsStringAsync();
        Console.Error.WriteLine($"Received: {data}");
        Input.Success("Data fetched successfully");
    }
    else
    {
        Input.Error($"Request failed with status: {response.StatusCode}");
    }
}
catch (Exception ex)
{
    Input.Error($"Request failed: {ex.Message}");
}
```

---

## Best Practices

### Error Handling

Use try-catch for robust error handling:

```csharp
using The0;

try
{
    var (botId, config) = Input.Parse();

    // Your trading logic
    var tradeId = ExecuteTrade("BTC/USDT", 100.0);

    Console.Error.WriteLine($"Trade executed: {tradeId}");
    Input.Success($"Trade {tradeId} completed");
}
catch (Exception ex)
{
    Input.Error($"Trade failed: {ex.Message}");
}

string ExecuteTrade(string symbol, double amount)
{
    // Your trade logic here
    return "trade_id_123";
}
```

### Configuration Validation

Validate configuration early:

```csharp
using The0;

var (botId, config) = Input.Parse();

// Validate required fields
var symbol = config?["symbol"]?.ToString();
if (string.IsNullOrEmpty(symbol))
{
    Input.Error("Missing required field: symbol");
}

var amount = config?["amount"]?.GetValue<double>() ?? 100.0;

Console.Error.WriteLine($"Trading {symbol} with amount {amount}");
Input.Success("Validation passed");
```

### Logging

You can use stdout or stderr for logging - the SDK writes results to a file:

```csharp
Console.WriteLine("Starting trade...");       // Logs to stdout
Console.Error.WriteLine("DEBUG: Details...");  // Logs to stderr
// Both appear in your bot's logs
```

### Async Programming

For async operations, use top-level statements with async:

```csharp
using The0;

var (botId, config) = Input.Parse();

await ProcessAsync();

Input.Success("Async processing complete");

async Task ProcessAsync()
{
    using var httpClient = new HttpClient();
    var result = await httpClient.GetStringAsync("https://api.example.com");
    Console.Error.WriteLine($"Got: {result}");
}
```

---

## Related Documentation

- [Configuration Reference](/custom-bot-development/configuration)
- [Bot Types](/custom-bot-development/bot-types)
- [Deployment Guide](/custom-bot-development/deployment)
