---
title: "C# Quick Start"
description: "Build your first C# trading bot with the0"
tags: ["custom-bots", "csharp", "dotnet", "quick-start"]
order: 14
---

# C# Quick Start

C# combines strong typing with excellent async support, making it well-suited for trading bots that need to manage multiple concurrent API calls. The .NET ecosystem provides battle-tested financial libraries, and the language's type system catches configuration errors at compile time. This guide walks through building an SMA crossover bot that monitors stock prices and emits trading signals.

By the end of this guide, you'll have a working realtime bot that calculates Simple Moving Averages and detects crossover signals using live market data.

## Prerequisites

Before starting, ensure you have the CLI installed and authenticated:

```bash
# Clone the repository and build the CLI
git clone https://github.com/alexanderwanyoike/the0.git
cd the0/cli
make install

# Authenticate
the0 auth login
```

You'll also need .NET 8 SDK installed locally for building. Download from [dotnet.microsoft.com](https://dotnet.microsoft.com/download).

## Project Structure

Create a new .NET console project:

```bash
dotnet new console -n SmaBot
cd SmaBot
```

A C# bot requires these files:

```
SmaBot/
├── SmaBot.csproj            # Project configuration
├── Program.cs               # Bot entry point
├── bot-config.yaml          # Bot metadata and runtime settings
├── bot-schema.json          # Configuration schema for users
└── bin/Release/net8.0/
    └── publish/
        └── SmaBot.dll       # Published output (after dotnet publish)
```

The entry point in `bot-config.yaml` must point to the published DLL. You compile and publish locally before deploying.

## Defining Bot Metadata

Create `bot-config.yaml`:

```yaml
name: sma-crossover
description: "SMA crossover strategy bot with Yahoo Finance data"
version: 1.0.0
author: "your-name"
type: realtime
runtime: dotnet8

entrypoints:
  bot: bin/Release/net8.0/publish/SmaBot.dll

schema:
  bot: bot-schema.json

readme: README.md

metadata:
  categories: [trading, technical-analysis]
  tags: [sma, crossover, csharp, dotnet]
  complexity: beginner
```

The `entrypoints.bot` field points to the published DLL. The path must match your publish output location.

## Defining Configuration Schema

Create `bot-schema.json`:

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "SMA Crossover Configuration",
  "description": "Configuration for the SMA crossover trading strategy bot",
  "properties": {
    "symbol": {
      "type": "string",
      "description": "Stock symbol to monitor (e.g., AAPL, MSFT, GOOGL)",
      "default": "AAPL"
    },
    "short_period": {
      "type": "integer",
      "description": "Number of periods for short SMA (fast moving average)",
      "default": 5,
      "minimum": 2,
      "maximum": 50
    },
    "long_period": {
      "type": "integer",
      "description": "Number of periods for long SMA (slow moving average)",
      "default": 20,
      "minimum": 5,
      "maximum": 200
    },
    "update_interval_ms": {
      "type": "integer",
      "description": "Milliseconds between price updates",
      "default": 60000,
      "minimum": 30000,
      "maximum": 3600000
    }
  },
  "additionalProperties": false
}
```

## Configuring the Project

Update `SmaBot.csproj`:

```xml
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

The SDK is published on NuGet as `The0.Sdk`. Install it with:

```bash
dotnet add package The0.Sdk
```

## Writing the Bot Logic

Replace `Program.cs`:

```csharp
using The0;

// Bot state persists across iterations
double? prevShortSma = null;
double? prevLongSma = null;

// Parse configuration from environment
var (botId, config) = Input.Parse();

var symbol = config?["symbol"]?.ToString() ?? "AAPL";
var shortPeriod = config?["short_period"]?.GetValue<int>() ?? 5;
var longPeriod = config?["long_period"]?.GetValue<int>() ?? 20;
var updateIntervalMs = config?["update_interval_ms"]?.GetValue<int>() ?? 60000;

Input.Log($"Bot {botId} started - {symbol} SMA({shortPeriod}/{longPeriod})");

using var client = new HttpClient();
client.DefaultRequestHeaders.Add("User-Agent", "the0-sma-bot/1.0");

// Main loop - runs until process is terminated
while (true)
{
    try
    {
        var prices = await FetchYahooFinance(client, symbol);

        if (prices.Count < longPeriod)
        {
            Input.Log($"Insufficient data: need {longPeriod} prices, have {prices.Count}");
            await Task.Delay(updateIntervalMs);
            continue;
        }

        // Calculate current price and change
        var currentPrice = prices[^1];
        var previousPrice = prices[^2];
        var changePct = (currentPrice - previousPrice) / previousPrice * 100;

        // Emit price metric
        Input.Metric("price", new
        {
            symbol,
            value = Math.Round(currentPrice, 2),
            change_pct = Math.Round(changePct, 3)
        });

        // Calculate SMAs
        var shortSma = CalculateSma(prices, shortPeriod);
        var longSma = CalculateSma(prices, longPeriod);

        // Emit SMA metric
        Input.Metric("sma", new
        {
            symbol,
            short_sma = Math.Round(shortSma, 2),
            long_sma = Math.Round(longSma, 2),
            short_period = shortPeriod,
            long_period = longPeriod
        });

        // Check for crossover signal
        if (prevShortSma.HasValue && prevLongSma.HasValue)
        {
            var signal = CheckCrossover(prevShortSma.Value, prevLongSma.Value, shortSma, longSma);
            if (signal != null)
            {
                var confidence = Math.Min(Math.Abs(shortSma - longSma) / longSma * 100, 0.95);
                var direction = signal == "BUY" ? "above" : "below";

                Input.Metric("signal", new
                {
                    type = signal,
                    symbol,
                    price = Math.Round(currentPrice, 2),
                    confidence = Math.Round(confidence, 2),
                    reason = $"SMA{shortPeriod} crossed {direction} SMA{longPeriod}"
                });
            }
        }

        // Update state for next iteration
        prevShortSma = shortSma;
        prevLongSma = longSma;
    }
    catch (Exception ex)
    {
        Input.Log($"Error: {ex.Message}", null, LogLevel.Error);
    }

    await Task.Delay(updateIntervalMs);
}

async Task<List<double>> FetchYahooFinance(HttpClient client, string symbol)
{
    var url = $"https://query1.finance.yahoo.com/v8/finance/chart/{symbol}?interval=1d&range=1mo";
    var response = await client.GetStringAsync(url);
    var json = System.Text.Json.JsonDocument.Parse(response);

    var prices = new List<double>();
    var closeData = json.RootElement
        .GetProperty("chart")
        .GetProperty("result")[0]
        .GetProperty("indicators")
        .GetProperty("quote")[0]
        .GetProperty("close");

    foreach (var price in closeData.EnumerateArray())
    {
        if (price.ValueKind != System.Text.Json.JsonValueKind.Null)
        {
            prices.Add(price.GetDouble());
        }
    }

    return prices;
}

double CalculateSma(List<double> prices, int period)
{
    if (prices.Count < period) return 0;
    return prices.TakeLast(period).Average();
}

string? CheckCrossover(double prevShort, double prevLong, double currShort, double currLong)
{
    // Golden cross: short SMA crosses above long SMA
    if (prevShort <= prevLong && currShort > currLong)
        return "BUY";

    // Death cross: short SMA crosses below long SMA
    if (prevShort >= prevLong && currShort < currLong)
        return "SELL";

    return null;
}
```

## SDK Functions

The C# SDK provides these methods in the `Input` class:

### Input.Parse()

Reads `BOT_ID` and `BOT_CONFIG` from environment variables. Returns a tuple of the bot ID and configuration:

```csharp
var (botId, config) = Input.Parse();

var symbol = config?["symbol"]?.ToString() ?? "AAPL";
var amount = config?["amount"]?.GetValue<double>() ?? 100.0;
```

### Input.Metric(type, data)

Emits a metric to the platform dashboard:

```csharp
Input.Metric("price", new { symbol = "AAPL", value = 150.25, change_pct = 1.5 });
Input.Metric("signal", new { type = "BUY", confidence = 0.85 });
```

### Input.Log(message, data, level)

Writes a structured log message:

```csharp
Input.Log("Processing symbol", new { symbol });
Input.Log("API error", new { error = ex.Message }, LogLevel.Error);
```

### Input.Success(message)

Reports successful execution for scheduled bots:

```csharp
Input.Success("Analysis complete");
```

### Input.Error(message)

Reports failure and terminates with exit code 1:

```csharp
if (string.IsNullOrEmpty(apiKey))
{
    Input.Error("API key is required");
}
```

### Input.Result(data)

Outputs a custom JSON result:

```csharp
Input.Result(new
{
    status = "success",
    trade_id = "abc123",
    filled_amount = 0.5
});
```

## Building

Publish in release mode:

```bash
dotnet publish -c Release
```

The DLL appears at `bin/Release/net8.0/publish/SmaBot.dll`, matching the entry point in `bot-config.yaml`.

## Testing Locally

Test by setting environment variables:

```bash
export BOT_ID="test-bot"
export BOT_CONFIG='{"symbol":"AAPL","short_period":5,"long_period":20,"update_interval_ms":5000}'
export CODE_MOUNT_DIR="/tmp"

dotnet run
```

## Deploying

Deploy your published bot to the platform:

```bash
the0 custom-bot deploy
```

The CLI packages the published DLL along with configuration files and uploads everything. You must publish locally before deploying.

## Creating Bot Instances

Once deployed, create instances that run your bot:

```json
{
  "name": "aapl-sma",
  "type": "realtime/sma-crossover",
  "version": "1.0.0",
  "config": {
    "symbol": "AAPL",
    "short_period": 5,
    "long_period": 20,
    "update_interval_ms": 60000
  }
}
```

Deploy the instance:

```bash
the0 bot deploy instance-config.json
```

## Monitoring

Monitor running instances:

```bash
# List running instances
the0 bot list

# View logs (use bot ID from deploy output or bot list)
the0 bot logs <bot_id>

# Stream logs in real-time
the0 bot logs <bot_id> -w

# Stop a realtime bot
the0 bot delete <bot_id>
```

## Next Steps

With your first C# bot deployed, explore these topics:

- [Configuration](./configuration) - Complete bot-config.yaml reference
- [Bot Types](./bot-types) - Scheduled vs realtime execution models
- [Metrics](./metrics) - Dashboard metrics and structured logging
- [Custom Frontends](./custom-frontends) - Build React dashboards for your bot
- [Testing](./testing) - Local testing patterns and best practices
