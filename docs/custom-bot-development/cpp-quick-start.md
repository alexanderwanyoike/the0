---
title: "C++ Quick Start"
description: "Build your first C++ trading bot with the0"
tags: ["custom-bots", "cpp", "c", "gcc", "quick-start"]
order: 13
---

# C++ Quick Start

C++ delivers deterministic performance with direct memory control, making it the language of choice for latency-sensitive trading systems. There's no garbage collection, no runtime overhead—just compiled machine code executing your strategy. This guide walks through building an SMA crossover bot that monitors stock prices and emits trading signals.

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

You'll also need GCC or Clang installed locally for building, along with CMake 3.14+ and libcurl development headers.

## Project Structure

Create a new directory for your bot:

```bash
mkdir sma-crossover
cd sma-crossover
```

A C++ bot requires these files:

```
sma-crossover/
├── CMakeLists.txt       # Build configuration
├── main.cpp             # Bot entry point
├── bot-config.yaml      # Bot metadata and runtime settings
├── bot-schema.json      # Configuration schema for users
└── build/
    └── sma_bot          # Compiled binary (after cmake build)
```

The entry point in `bot-config.yaml` must point to the compiled binary, not the source file. You compile locally before deploying.

## Defining Bot Metadata

Create `bot-config.yaml`:

```yaml
name: sma-crossover
description: "SMA crossover strategy bot with Yahoo Finance data"
version: 1.0.0
author: "your-name"
type: realtime
runtime: gcc13

entrypoints:
  bot: build/sma_bot

schema:
  bot: bot-schema.json

readme: README.md

metadata:
  categories: [trading, technical-analysis]
  tags: [sma, crossover, cpp]
  complexity: beginner
```

The `entrypoints.bot` field points to the compiled binary. The binary name comes from your CMakeLists.txt configuration.

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

## Configuring CMakeLists.txt

Create `CMakeLists.txt`:

```cmake
cmake_minimum_required(VERSION 3.14)
project(sma_bot)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include(FetchContent)

# Fetch the0 SDK header
FetchContent_Declare(the0_sdk
    GIT_REPOSITORY https://github.com/alexanderwanyoike/the0.git
    GIT_TAG main
    SOURCE_SUBDIR sdk/cpp
)
FetchContent_MakeAvailable(the0_sdk)

# Fetch nlohmann/json (required by the0 SDK)
FetchContent_Declare(json
    GIT_REPOSITORY https://github.com/nlohmann/json.git
    GIT_TAG v3.11.3
)
FetchContent_MakeAvailable(json)

# Find libcurl for HTTP requests
find_package(CURL REQUIRED)

# Create executable
add_executable(sma_bot main.cpp)
target_include_directories(sma_bot PRIVATE ${the0_sdk_SOURCE_DIR})
target_link_libraries(sma_bot PRIVATE nlohmann_json::nlohmann_json CURL::libcurl)
```

The SDK is header-only. CMake FetchContent downloads it automatically along with nlohmann/json for JSON parsing.

## Writing the Bot Logic

Create `main.cpp`:

```cpp
#include <the0.h>
#include <iostream>
#include <string>
#include <vector>
#include <chrono>
#include <thread>
#include <optional>
#include <cmath>
#include <curl/curl.h>

using json = the0::json;

// Bot state persists across iterations
struct BotState {
    std::optional<double> prevShortSma;
    std::optional<double> prevLongSma;
};

// Callback for CURL response
static size_t WriteCallback(void* contents, size_t size, size_t nmemb, std::string* userp) {
    userp->append((char*)contents, size * nmemb);
    return size * nmemb;
}

double roundTo(double value, int decimals) {
    double multiplier = std::pow(10.0, decimals);
    return std::round(value * multiplier) / multiplier;
}

// Fetch data from Yahoo Finance
std::vector<double> fetchYahooFinance(CURL* curl, const std::string& symbol) {
    std::vector<double> prices;
    std::string url = "https://query1.finance.yahoo.com/v8/finance/chart/" + symbol + "?interval=1d&range=1mo";
    std::string response;

    curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "the0-sma-bot/1.0");
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, 30L);

    CURLcode res = curl_easy_perform(curl);
    if (res != CURLE_OK) {
        throw std::runtime_error(std::string("CURL error: ") + curl_easy_strerror(res));
    }

    json data = json::parse(response);

    if (data.contains("chart") &&
        data["chart"].contains("result") &&
        !data["chart"]["result"].empty()) {

        auto& result = data["chart"]["result"][0];
        if (result.contains("indicators") &&
            result["indicators"].contains("quote") &&
            !result["indicators"]["quote"].empty()) {

            auto& closeData = result["indicators"]["quote"][0]["close"];
            for (const auto& price : closeData) {
                if (!price.is_null()) {
                    prices.push_back(price.get<double>());
                }
            }
        }
    }

    return prices;
}

// Calculate Simple Moving Average
double calculateSMA(const std::vector<double>& prices, size_t period) {
    if (prices.size() < period) return 0.0;

    double sum = 0.0;
    for (size_t i = prices.size() - period; i < prices.size(); ++i) {
        sum += prices[i];
    }
    return sum / static_cast<double>(period);
}

// Check for crossover
std::optional<std::string> checkCrossover(double prevShort, double prevLong,
                                           double currShort, double currLong) {
    // Golden cross: short SMA crosses above long SMA
    if (prevShort <= prevLong && currShort > currLong) {
        return "BUY";
    }
    // Death cross: short SMA crosses below long SMA
    if (prevShort >= prevLong && currShort < currLong) {
        return "SELL";
    }
    return std::nullopt;
}

int main() {
    // Parse configuration from environment
    auto [botId, config] = the0::parse();

    // Extract configuration with defaults
    std::string symbol = config.value("symbol", "AAPL");
    int shortPeriod = config.value("short_period", 5);
    int longPeriod = config.value("long_period", 20);
    int updateIntervalMs = config.value("update_interval_ms", 60000);

    the0::log("Bot " + botId + " started - " + symbol +
              " SMA(" + std::to_string(shortPeriod) + "/" +
              std::to_string(longPeriod) + ")");

    // Initialize CURL
    curl_global_init(CURL_GLOBAL_DEFAULT);
    CURL* curl = curl_easy_init();
    if (!curl) {
        the0::error("Failed to initialize CURL");
    }

    BotState state;

    // Main loop - runs until process is terminated
    while (true) {
        try {
            std::vector<double> prices = fetchYahooFinance(curl, symbol);

            if (prices.size() < static_cast<size_t>(longPeriod)) {
                the0::log("Insufficient data: need " + std::to_string(longPeriod) +
                         " prices, have " + std::to_string(prices.size()));
                std::this_thread::sleep_for(std::chrono::milliseconds(updateIntervalMs));
                continue;
            }

            // Calculate current price and change
            double currentPrice = prices.back();
            double previousPrice = prices[prices.size() - 2];
            double changePct = ((currentPrice - previousPrice) / previousPrice) * 100.0;

            // Emit price metric
            the0::metric("price", {
                {"symbol", symbol},
                {"value", roundTo(currentPrice, 2)},
                {"change_pct", roundTo(changePct, 3)}
            });

            // Calculate SMAs
            double shortSma = calculateSMA(prices, shortPeriod);
            double longSma = calculateSMA(prices, longPeriod);

            // Emit SMA metric
            the0::metric("sma", {
                {"symbol", symbol},
                {"short_sma", roundTo(shortSma, 2)},
                {"long_sma", roundTo(longSma, 2)},
                {"short_period", shortPeriod},
                {"long_period", longPeriod}
            });

            // Check for crossover signal
            if (state.prevShortSma.has_value() && state.prevLongSma.has_value()) {
                auto signal = checkCrossover(
                    state.prevShortSma.value(),
                    state.prevLongSma.value(),
                    shortSma,
                    longSma
                );

                if (signal.has_value()) {
                    double confidence = std::min(std::abs(shortSma - longSma) / longSma * 100.0, 0.95);
                    std::string direction = signal.value() == "BUY" ? "above" : "below";

                    the0::metric("signal", {
                        {"type", signal.value()},
                        {"symbol", symbol},
                        {"price", roundTo(currentPrice, 2)},
                        {"confidence", roundTo(confidence, 2)},
                        {"reason", "SMA" + std::to_string(shortPeriod) + " crossed " +
                                  direction + " SMA" + std::to_string(longPeriod)}
                    });
                }
            }

            // Update state for next iteration
            state.prevShortSma = shortSma;
            state.prevLongSma = longSma;

        } catch (const std::exception& e) {
            the0::log(std::string("Error: ") + e.what());
        }

        std::this_thread::sleep_for(std::chrono::milliseconds(updateIntervalMs));
    }

    curl_easy_cleanup(curl);
    curl_global_cleanup();
    return 0;
}
```

The bot fetches historical prices from Yahoo Finance, calculates short and long Simple Moving Averages, and emits signals when crossovers occur. State persists across loop iterations, allowing the bot to detect when averages cross.

## SDK Functions

The C++ SDK provides these functions in the `the0` namespace:

### the0::parse()

Reads `BOT_ID` and `BOT_CONFIG` from environment variables. Returns `std::pair<std::string, the0::json>`:

```cpp
auto [botId, config] = the0::parse();

std::string symbol = config.value("symbol", "AAPL");
double amount = config.value("amount", 100.0);
int retries = config.value("retries", 3);
```

### the0::metric(type, data)

Emits a metric to the platform dashboard:

```cpp
the0::metric("price", {
    {"symbol", "AAPL"},
    {"value", 150.25},
    {"change_pct", 1.5}
});

the0::metric("signal", {
    {"type", "BUY"},
    {"confidence", 0.85}
});
```

### the0::log(message)

Writes a log message:

```cpp
the0::log("Processing " + symbol);
the0::log("Price updated: " + std::to_string(price));
```

### the0::success(message)

Reports successful execution for scheduled bots:

```cpp
the0::success("Analysis complete");
```

### the0::error(message)

Reports failure and terminates with exit code 1. This function does not return:

```cpp
if (prices.empty()) {
    the0::error("No price data available");
}
```

### the0::result(data)

Outputs a custom JSON result:

```cpp
the0::result({
    {"status", "success"},
    {"trade_id", "abc123"},
    {"filled_amount", 0.5}
});
```

## Building

Build with CMake:

```bash
mkdir build
cd build
cmake ..
make
```

The binary appears at `build/sma_bot`, matching the entry point in `bot-config.yaml`.

## Testing Locally

Test by setting environment variables:

```bash
export BOT_ID="test-bot"
export BOT_CONFIG='{"symbol":"AAPL","short_period":5,"long_period":20,"update_interval_ms":5000}'
export CODE_MOUNT_DIR="/tmp"

./build/sma_bot
```

The bot should start fetching prices and emitting metrics. Press Ctrl+C to stop.

## Deploying

Deploy your compiled bot to the platform:

```bash
the0 custom-bot deploy
```

The CLI packages the compiled binary along with configuration files and uploads everything. Unlike interpreted languages, you must compile before deploying.

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

With your first C++ bot deployed, explore these topics:

- [Configuration](./configuration) - Complete bot-config.yaml reference
- [Bot Types](./bot-types) - Scheduled vs realtime execution models
- [Metrics](./metrics) - Dashboard metrics and structured logging
- [Custom Frontends](./custom-frontends) - Build React dashboards for your bot
- [Testing](./testing) - Local testing patterns and best practices
