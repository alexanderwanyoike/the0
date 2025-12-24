---
title: "C/C++ Quick Start"
description: "Build your first C/C++ trading bot with the0"
tags: ["custom-bots", "cpp", "c", "gcc", "quick-start"]
order: 13
---

# C/C++ Quick Start Guide

Build a trading bot in C or C++ with the0's GCC 13 runtime.

---

## Prerequisites

- GCC or Clang compiler (for local development)
- CMake 3.10+ or Make
- the0 CLI installed
- Valid the0 API key

---

## Project Structure

```
my-cpp-bot/
├── CMakeLists.txt        # CMake project file (or Makefile)
├── main.cpp              # Your bot entry point
├── bot-config.yaml       # Bot configuration
├── bot-schema.json       # Parameter schema
└── README.md             # Documentation
```

---

## Step 1: Create Your Project

```bash
mkdir my-cpp-bot
cd my-cpp-bot
```

---

## Step 2: Create CMakeLists.txt

```cmake
cmake_minimum_required(VERSION 3.10)
project(my-cpp-bot)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_executable(my-cpp-bot main.cpp)
```

Or use a simple `Makefile`:

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
#include <cstdlib>
#include <iostream>
#include <string>

int main() {
    // Get bot configuration from environment
    const char* bot_id_env = std::getenv("BOT_ID");
    const char* config_env = std::getenv("BOT_CONFIG");

    std::string bot_id = bot_id_env ? bot_id_env : "unknown";
    std::string config = config_env ? config_env : "{}";

    std::cerr << "Bot " << bot_id << " starting..." << std::endl;
    std::cerr << "Config: " << config << std::endl;

    // Your trading logic here
    // Example: Parse config, fetch prices, execute trades

    // Output success result
    std::cout << "{\"status\":\"success\",\"message\":\"Bot executed successfully\"}" << std::endl;

    return 0;
}
```

---

## Step 4: Using the SDK (Optional)

Copy `the0.h` from `sdk/cpp/` for cleaner code:

```cpp
#include "the0.h"

int main() {
    auto [bot_id, config] = the0::parse();

    std::cerr << "Bot " << bot_id << " starting..." << std::endl;

    // Your trading logic here

    the0::success("Bot executed successfully");
    return 0;
}
```

---

## Step 5: Create Bot Configuration

Create `bot-config.yaml`:

```yaml
name: my-cpp-bot
description: "A C++ trading bot"
version: "1.0.0"
author: "Your Name"
type: scheduled
runtime: gcc13

entrypoints:
  bot: main.cpp

schema:
  bot: bot-schema.json

readme: README.md
```

---

## Step 6: Define Parameter Schema

Create `bot-schema.json`:

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "Bot Configuration",
  "description": "Configuration for the C++ trading bot",
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

## Step 7: Deploy

```bash
# Deploy your bot
the0 custom-bot deploy
```

The build happens automatically in Docker - no need to compile locally!

---

## SDK API Reference

The `the0.h` header provides these functions in the `the0` namespace:

### `the0::parse() -> std::pair<std::string, std::string>`

Parse bot configuration from environment variables.

```cpp
auto [bot_id, config] = the0::parse();
// bot_id: Value of BOT_ID env var
// config: Value of BOT_CONFIG env var (JSON string)
```

### `the0::success(const std::string& message)`

Output a success result to stdout.

```cpp
the0::success("Trade completed");
// Outputs: {"status":"success","message":"Trade completed"}
```

### `the0::error(const std::string& message)`

Output an error result and exit with code 1.

```cpp
the0::error("Failed to connect");
// Outputs: {"status":"error","message":"Failed to connect"}
// Exits with code 1
```

### `the0::result(const std::string& json_str)`

Output a custom JSON result.

```cpp
the0::result("{\"status\":\"success\",\"trade_id\":\"abc123\"}");
```

---

## Adding Dependencies

For JSON parsing, consider [nlohmann/json](https://github.com/nlohmann/json):

```cpp
#include "the0.h"
#include <nlohmann/json.hpp>

int main() {
    auto [bot_id, config_str] = the0::parse();

    auto config = nlohmann::json::parse(config_str);
    std::string symbol = config.value("symbol", "BTC/USDT");
    double amount = config.value("amount", 100.0);

    std::cerr << "Trading " << symbol << " with amount " << amount << std::endl;

    the0::success("Bot executed");
    return 0;
}
```

Update your `CMakeLists.txt`:

```cmake
cmake_minimum_required(VERSION 3.10)
project(my-cpp-bot)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include(FetchContent)
FetchContent_Declare(json
    GIT_REPOSITORY https://github.com/nlohmann/json.git
    GIT_TAG v3.11.3
)
FetchContent_MakeAvailable(json)

add_executable(my-cpp-bot main.cpp)
target_link_libraries(my-cpp-bot PRIVATE nlohmann_json::nlohmann_json)
```

---

## Example: HTTP Request Bot

Using libcurl for HTTP requests:

```cpp
#include "the0.h"
#include <curl/curl.h>
#include <sstream>

static size_t WriteCallback(void* contents, size_t size, size_t nmemb, std::string* userp) {
    userp->append((char*)contents, size * nmemb);
    return size * nmemb;
}

int main() {
    auto [bot_id, config] = the0::parse();
    std::cerr << "Bot " << bot_id << " fetching data..." << std::endl;

    CURL* curl = curl_easy_init();
    if (!curl) {
        the0::error("Failed to initialize curl");
    }

    std::string response;
    curl_easy_setopt(curl, CURLOPT_URL, "https://api.example.com/price");
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        the0::error(std::string("Request failed: ") + curl_easy_strerror(res));
    }

    std::cerr << "Response: " << response << std::endl;
    the0::success("Data fetched successfully");
    return 0;
}
```

---

## Best Practices

### Error Handling

Use try-catch for robust error handling:

```cpp
#include "the0.h"
#include <stdexcept>

int main() {
    try {
        auto [bot_id, config] = the0::parse();

        // Your trading logic
        if (bot_id.empty()) {
            throw std::runtime_error("Bot ID is empty");
        }

        the0::success("Trade completed");
        return 0;
    } catch (const std::exception& e) {
        the0::error(e.what());
    }
}
```

### Logging

Use stderr for logs (stdout is reserved for JSON output):

```cpp
std::cerr << "DEBUG: Processing trade..." << std::endl;  // Logs
std::cout << "{...}";  // Reserved for JSON result
```

### Memory Safety

Consider using smart pointers and RAII:

```cpp
#include <memory>

class TradingClient {
public:
    TradingClient() { /* initialize */ }
    ~TradingClient() { /* cleanup */ }

    void executeTrade(const std::string& symbol, double amount) {
        // Trade logic
    }
};

int main() {
    auto client = std::make_unique<TradingClient>();
    client->executeTrade("BTC/USDT", 100.0);
    return 0;
}
```

---

## Build Systems

### CMake (Recommended)

CMake is detected first and is the recommended build system:

```cmake
cmake_minimum_required(VERSION 3.10)
project(my-bot)

set(CMAKE_CXX_STANDARD 17)
add_executable(my-bot main.cpp)
```

### Makefile

Simple Makefile for smaller projects:

```makefile
CXX = g++
CXXFLAGS = -std=c++17 -O2 -Wall

my-bot: main.cpp
	$(CXX) $(CXXFLAGS) -o $@ $<
```

---

## Related Documentation

- [Configuration Reference](/custom-bot-development/configuration)
- [Bot Types](/custom-bot-development/bot-types)
- [Deployment Guide](/custom-bot-development/deployment)
