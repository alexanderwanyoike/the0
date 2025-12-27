---
title: "C/C++ Quick Start"
description: "Build your first C/C++ trading bot with the0"
tags: ["custom-bots", "cpp", "c", "gcc", "quick-start"]
order: 13
---

# C/C++ Quick Start Guide

Build high-performance trading bots in C or C++ with the0's GCC 13 runtime. C++ is ideal for ultra-low-latency strategies where you need direct memory control and maximum performance.

---

## Why C++ for Trading Bots?

C++ remains the language of choice for professional trading systems:

- **Minimal Latency**: Direct hardware access with no runtime overhead
- **Deterministic Performance**: No garbage collection or JIT compilation pauses
- **Fine-grained Control**: Manual memory management for critical paths
- **Mature Ecosystem**: Decades of battle-tested libraries for finance
- **Cross-platform**: Same code runs on Linux servers and Windows desktops

**When to Choose C++:**
- High-frequency trading (HFT) strategies
- Co-located execution systems
- Processing tick-by-tick market data
- When microseconds matter

**Popular Libraries for Trading:**
- `libcurl` - HTTP/HTTPS client for REST APIs
- `nlohmann/json` - Modern JSON parsing (used by the0 SDK)
- `Boost.Beast` - WebSocket client for streaming data
- `Decimal` - Fixed-point arithmetic for prices
- `OpenSSL` - Secure communications

---

## Prerequisites

- GCC or Clang compiler (for local development)
- CMake 3.14+ (recommended) or Make
- the0 CLI installed
- Valid the0 API key
- Basic understanding of C++ pointers and memory

---

## Project Structure

```
my-cpp-bot/
├── CMakeLists.txt        # CMake project file (recommended)
├── main.cpp              # Your bot entry point
├── bot-config.yaml       # Bot configuration
├── bot-schema.json       # Parameter schema
├── config.json           # Example configuration
└── README.md             # Documentation
```

> **Note:** The SDK header (`the0.h`) is fetched automatically via CMake FetchContent.

---

## Step 1: Create Your Project

```bash
mkdir my-cpp-bot
cd my-cpp-bot
```

---

## Step 2: Create CMakeLists.txt

CMake handles dependency management and cross-platform builds:

```cmake
cmake_minimum_required(VERSION 3.14)
project(my-cpp-bot)

# Use C++17 for modern features
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include(FetchContent)

# Fetch the0 SDK header
FetchContent_Declare(the0_sdk
    GIT_REPOSITORY https://github.com/alexanderwanyoike/the0.git
    GIT_TAG v1.1.0
    SOURCE_SUBDIR sdk/cpp
)
FetchContent_MakeAvailable(the0_sdk)

# Fetch nlohmann/json (required by the0 SDK)
FetchContent_Declare(json
    GIT_REPOSITORY https://github.com/nlohmann/json.git
    GIT_TAG v3.11.3
)
FetchContent_MakeAvailable(json)

# Create executable
add_executable(my-cpp-bot main.cpp)
target_include_directories(my-cpp-bot PRIVATE ${the0_sdk_SOURCE_DIR})
target_link_libraries(my-cpp-bot PRIVATE nlohmann_json::nlohmann_json)

# Optional: Link libcurl for HTTP requests
# find_package(CURL REQUIRED)
# target_link_libraries(my-cpp-bot PRIVATE CURL::libcurl)
```

**Alternative: Simple Makefile** (requires nlohmann-json installed):

```bash
# Install nlohmann-json first:
# Ubuntu/Debian: sudo apt install nlohmann-json3-dev
# macOS: brew install nlohmann-json
```

```makefile
CXX = g++
CXXFLAGS = -std=c++17 -O2 -Wall

all: my-cpp-bot

my-cpp-bot: main.cpp
	$(CXX) $(CXXFLAGS) -o $@ $<

clean:
	rm -f my-cpp-bot
```

---

## Step 3: Write Your Bot

Create `main.cpp`:

```cpp
#include "the0.h"
#include <iostream>

/**
 * Main entry point for the trading bot.
 *
 * The the0 SDK handles:
 * - Reading BOT_ID and BOT_CONFIG from environment
 * - JSON parsing with nlohmann/json
 * - Writing results to the correct output file
 */
int main() {
    // Parse bot configuration from environment
    // Returns (bot_id, config) where config is nlohmann::json
    auto [bot_id, config] = the0::parse();

    // Log to stderr (stdout is reserved for certain outputs)
    std::cerr << "Bot " << bot_id << " starting..." << std::endl;

    // Access configuration with type-safe methods
    // .value() provides a default if key is missing
    std::string symbol = config.value("symbol", "BTC/USDT");
    double amount = config.value("amount", 100.0);

    std::cerr << "Trading " << symbol << " with amount " << amount << std::endl;

    // ===========================================
    // YOUR TRADING LOGIC GOES HERE
    // ===========================================

    // Example: Validate configuration
    if (amount <= 0) {
        the0::error("Amount must be positive");
        // Note: error() exits the program
    }

    // Example: Fetch market data (implement with libcurl)
    // double price = fetch_price(symbol);

    // Example: Execute trade
    // auto order = place_order(symbol, amount);

    // ===========================================
    // END OF TRADING LOGIC
    // ===========================================

    // Output result with additional data
    the0::result({
        {"status", "success"},
        {"message", "Trade executed"},
        {"data", {
            {"symbol", symbol},
            {"amount", amount},
            {"timestamp", "2024-01-01T00:00:00Z"}
        }}
    });

    return 0;
}
```

---

## Step 4: Create Bot Configuration

Create `bot-config.yaml`:

```yaml
name: my-cpp-bot
description: "A high-performance C++ trading bot"
version: "1.0.0"
author: "Your Name"
type: scheduled
runtime: gcc13

# The entrypoint is the source file - gets compiled automatically
entrypoints:
  bot: main.cpp

schema:
  bot: bot-schema.json

readme: README.md

metadata:
  categories: [trading]
  instruments: [crypto]
  tags: [cpp, high-performance, low-latency]
```

**Note:** The `runtime: gcc13` tells the platform to compile your bot with GCC 13. You don't need to compile locally.

---

## Step 5: Define Parameter Schema

Create `bot-schema.json`:

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "C++ Bot Configuration",
  "description": "Configuration for the C++ trading bot",
  "properties": {
    "symbol": {
      "type": "string",
      "title": "Trading Symbol",
      "description": "The trading pair (e.g., BTC/USDT)",
      "default": "BTC/USDT"
    },
    "amount": {
      "type": "number",
      "title": "Trade Amount",
      "description": "Amount in base currency to trade",
      "default": 100,
      "minimum": 0.01
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
    }
  },
  "required": ["symbol", "api_key", "api_secret"]
}
```

---

## Step 6: Test Locally

```bash
# Build with CMake
mkdir build && cd build
cmake ..
make

# Set environment variables for testing
export BOT_ID="test-bot-123"
export BOT_CONFIG='{"symbol":"BTC/USDT","amount":100}'
export CODE_MOUNT_DIR="/tmp"

# Run
./my-cpp-bot
```

---

## Step 7: Deploy

```bash
the0 custom-bot deploy
```

The platform will:
1. Compile your C++ code with `g++ -std=c++17 -O2`
2. Link required libraries
3. Package and deploy the binary

No need to compile locally - it all happens in the cloud!

---

## SDK API Reference

The `the0.h` header provides these functions in the `the0` namespace. Uses `nlohmann::json` (aliased as `the0::json`) for type-safe JSON handling.

### `the0::parse() -> std::pair<std::string, the0::json>`

Parse bot configuration from environment variables:

```cpp
auto [bot_id, config] = the0::parse();

// Access values with type safety and defaults
std::string symbol = config.value("symbol", "BTC/USDT");
double amount = config.value("amount", 100.0);
int retries = config.value("retries", 3);

// Check if key exists
if (config.contains("api_key")) {
    std::string key = config["api_key"];
}
```

### `the0::parse_raw() -> std::pair<std::string, std::string>`

Parse configuration returning raw JSON string (for custom parsing):

```cpp
auto [bot_id, config_str] = the0::parse_raw();
// Parse with your preferred JSON library
```

### `the0::success(const std::string& message)`

Output a success result:

```cpp
the0::success("Trade completed");
// Outputs: {"status":"success","message":"Trade completed"}
```

### `the0::error(const std::string& message)`

Output an error result and exit with code 1:

```cpp
if (amount <= 0) {
    the0::error("Amount must be positive");
    // Program exits here
}
// This line never reached if amount <= 0
```

### `the0::result(const the0::json& data)`

Output a custom JSON result:

```cpp
the0::result({
    {"status", "success"},
    {"trade_id", "abc123"},
    {"filled_amount", 0.5},
    {"average_price", 45123.50}
});
```

### `the0::result(status, message, data)`

Output a result with status, message, and additional data:

```cpp
the0::result("success", "Trade executed", {
    {"trade_id", "abc123"},
    {"price", 45000.50}
});
```

---

## Example: Price Fetcher with libcurl

Here's a complete example that fetches real price data:

```cpp
#include "the0.h"
#include <curl/curl.h>
#include <iostream>
#include <sstream>

// Callback for curl to write response data
static size_t WriteCallback(void* contents, size_t size, size_t nmemb, std::string* userp) {
    userp->append((char*)contents, size * nmemb);
    return size * nmemb;
}

// Fetch price from Binance API
double fetch_binance_price(const std::string& symbol) {
    CURL* curl = curl_easy_init();
    if (!curl) {
        throw std::runtime_error("Failed to initialize curl");
    }

    std::string url = "https://api.binance.com/api/v3/ticker/price?symbol=" + symbol;
    std::string response;

    curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, 10L);

    CURLcode res = curl_easy_perform(curl);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        throw std::runtime_error(std::string("Request failed: ") + curl_easy_strerror(res));
    }

    // Parse JSON response
    auto json = the0::json::parse(response);
    return std::stod(json["price"].get<std::string>());
}

int main() {
    auto [bot_id, config] = the0::parse();

    std::string symbol = config.value("symbol", "BTCUSDT");
    std::cerr << "Bot " << bot_id << " fetching price for " << symbol << std::endl;

    try {
        double price = fetch_binance_price(symbol);
        std::cerr << "Current price: $" << price << std::endl;

        the0::result({
            {"status", "success"},
            {"message", "Price fetched"},
            {"data", {
                {"symbol", symbol},
                {"price", price}
            }}
        });
    } catch (const std::exception& e) {
        the0::error(std::string("Failed to fetch price: ") + e.what());
    }

    return 0;
}
```

Update CMakeLists.txt to link libcurl:

```cmake
find_package(CURL REQUIRED)
target_link_libraries(my-cpp-bot PRIVATE nlohmann_json::nlohmann_json CURL::libcurl)
```

---

## Best Practices

### 1. Error Handling with Exceptions

Use C++ exceptions for clean error handling:

```cpp
#include "the0.h"
#include <stdexcept>

int main() {
    try {
        auto [bot_id, config] = the0::parse();

        // Validate configuration
        if (!config.contains("api_key")) {
            throw std::runtime_error("Missing required field: api_key");
        }

        // Your trading logic
        execute_trade(config);

        the0::success("Trade completed");
        return 0;
    } catch (const std::exception& e) {
        the0::error(e.what());
    }
}
```

### 2. RAII for Resource Management

Use smart pointers and RAII patterns:

```cpp
#include <memory>

class TradingClient {
public:
    TradingClient(const std::string& api_key) {
        // Initialize connection
    }
    ~TradingClient() {
        // Cleanup automatically
    }

    std::string place_order(const std::string& symbol, double amount) {
        // Trade logic
        return "order_123";
    }
};

int main() {
    auto [bot_id, config] = the0::parse();

    // Client automatically cleaned up when out of scope
    auto client = std::make_unique<TradingClient>(config["api_key"]);
    auto order_id = client->place_order(config["symbol"], config["amount"]);

    the0::success("Order placed: " + order_id);
    return 0;
}
```

### 3. Logging

Both stdout and stderr go to your bot's logs:

```cpp
std::cout << "Starting trade..." << std::endl;    // Goes to log
std::cerr << "DEBUG: price = " << price << std::endl;  // Goes to log
```

### 4. Precise Decimal Arithmetic

For financial calculations, avoid floating-point errors:

```cpp
// Option 1: Use integers (cents/satoshis)
int64_t price_cents = 4512350;  // $45,123.50
int64_t quantity_sats = 50000000;  // 0.5 BTC

// Option 2: Use a decimal library
// Consider: https://github.com/vpiotr/decimal_for_cpp
```

---

## Build Systems

### CMake (Recommended)

CMake handles cross-platform builds and dependency management:

```cmake
cmake_minimum_required(VERSION 3.14)
project(my-bot)

set(CMAKE_CXX_STANDARD 17)

# Fetch dependencies automatically
include(FetchContent)
FetchContent_Declare(json
    GIT_REPOSITORY https://github.com/nlohmann/json.git
    GIT_TAG v3.11.3
)
FetchContent_MakeAvailable(json)

add_executable(my-bot main.cpp)
target_link_libraries(my-bot PRIVATE nlohmann_json::nlohmann_json)
```

### Makefile (Simple Projects)

For simple projects without external dependencies:

```makefile
CXX = g++
CXXFLAGS = -std=c++17 -O2 -Wall -Wextra

my-bot: main.cpp
	$(CXX) $(CXXFLAGS) -o $@ $< -lcurl
```

---

## Related Documentation

- [Configuration Reference](/custom-bot-development/configuration)
- [Bot Types](/custom-bot-development/bot-types)
- [Custom Frontends](/custom-bot-development/custom-frontends)
- [Deployment Guide](/custom-bot-development/deployment)
