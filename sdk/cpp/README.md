# the0 C/C++ SDK

Header-only SDK for building trading bots on the0 platform in C or C++.

Uses [nlohmann/json](https://github.com/nlohmann/json) for robust JSON handling.

## Installation

1. Copy `the0.h` to your project
2. Ensure nlohmann/json is available (see below)

```cpp
#include "the0.h"
```

## Requirements

- C++17 or later
- [nlohmann/json](https://github.com/nlohmann/json) v3.x

### Installing nlohmann/json

**Option 1: CMake FetchContent (Recommended)**
```cmake
include(FetchContent)
FetchContent_Declare(json
    GIT_REPOSITORY https://github.com/nlohmann/json.git
    GIT_TAG v3.11.3
)
FetchContent_MakeAvailable(json)
target_link_libraries(my-bot PRIVATE nlohmann_json::nlohmann_json)
```

**Option 2: System Package**
```bash
# Ubuntu/Debian
sudo apt install nlohmann-json3-dev

# macOS
brew install nlohmann-json

# vcpkg
vcpkg install nlohmann-json
```

## Usage

### Basic Example

```cpp
#include "the0.h"
#include <iostream>

int main() {
    // Parse bot configuration from environment
    auto [bot_id, config] = the0::parse();

    std::cerr << "Bot " << bot_id << " starting" << std::endl;

    // Access configuration values with type safety
    std::string symbol = config.value("symbol", "BTC/USDT");
    double amount = config.value("amount", 100.0);

    std::cerr << "Trading " << symbol << " with amount " << amount << std::endl;

    // Your trading logic here

    the0::success("Bot executed successfully");
    return 0;
}
```

### C++11/14 Compatible Version

```cpp
#include "the0.h"
#include <iostream>

int main() {
    std::pair<std::string, the0::json> input = the0::parse();
    std::string bot_id = input.first;
    the0::json config = input.second;

    std::cerr << "Bot " << bot_id << " starting" << std::endl;

    the0::success("Bot executed successfully");
    return 0;
}
```

## API Reference

### `the0::parse() -> std::pair<std::string, the0::json>`

Parse bot configuration from environment variables.

- Returns: `(bot_id, config)` tuple
- `bot_id`: Value of `BOT_ID` environment variable (empty string if not set)
- `config`: Parsed JSON object from `BOT_CONFIG` (empty object if not set or invalid)

```cpp
auto [bot_id, config] = the0::parse();
std::string symbol = config.value("symbol", "BTC/USDT");
int count = config.value("count", 10);
```

### `the0::parse_raw() -> std::pair<std::string, std::string>`

Parse bot configuration, returning raw config string. Use if you prefer a different JSON library.

```cpp
auto [bot_id, config_str] = the0::parse_raw();
// Parse config_str with your preferred JSON library
```

### `the0::success(const std::string& message)`

Output a success result to stdout.

```cpp
the0::success("Trade completed");
// Outputs: {"message":"Trade completed","status":"success"}
```

### `the0::error(const std::string& message)`

Output an error result to stdout and exit with code 1.

```cpp
the0::error("Failed to connect");
// Outputs: {"message":"Failed to connect","status":"error"}
// Then exits with code 1
```

### `the0::result(const the0::json& data)`

Output a custom JSON result to stdout.

```cpp
the0::result({
    {"status", "success"},
    {"trade_id", "abc123"},
    {"filled_amount", 0.5}
});
```

### `the0::result(status, message, data)`

Output a result with status, message, and additional data.

```cpp
the0::result("success", "Trade executed", {
    {"trade_id", "abc123"},
    {"price", 45000.50}
});
// Outputs: {"message":"Trade executed","price":45000.5,"status":"success","trade_id":"abc123"}
```

## Project Setup

### Using CMake (Recommended)

Create a `CMakeLists.txt`:

```cmake
cmake_minimum_required(VERSION 3.14)
project(my-cpp-bot)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Fetch nlohmann/json
include(FetchContent)
FetchContent_Declare(json
    GIT_REPOSITORY https://github.com/nlohmann/json.git
    GIT_TAG v3.11.3
)
FetchContent_MakeAvailable(json)

add_executable(my-cpp-bot main.cpp)
target_link_libraries(my-cpp-bot PRIVATE nlohmann_json::nlohmann_json)
```

### Using Makefile

Requires nlohmann/json installed system-wide:

```makefile
CXX = g++
CXXFLAGS = -std=c++17 -O2 -Wall

all: my-cpp-bot

my-cpp-bot: main.cpp
	$(CXX) $(CXXFLAGS) -o $@ $<

clean:
	rm -f my-cpp-bot
```

## Bot Configuration

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

## Deployment

```bash
# Deploy your bot
the0 custom-bot deploy
```

The CLI automatically builds your C++ project in Docker before deployment.

## Best Practices

### Error Handling

```cpp
#include "the0.h"
#include <stdexcept>

int main() {
    try {
        auto [bot_id, config] = the0::parse();

        if (bot_id.empty()) {
            the0::error("Bot ID not provided");
        }

        // Validate required config
        if (!config.contains("symbol")) {
            the0::error("Missing required field: symbol");
        }

        std::string symbol = config["symbol"];

        // Your trading logic
        the0::success("Trade completed for " + symbol);
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
std::cout << "...";  // Reserved for JSON result - use the0::result() instead
```

### Working with JSON Config

```cpp
auto [bot_id, config] = the0::parse();

// Get with default value
std::string symbol = config.value("symbol", "BTC/USDT");
double amount = config.value("amount", 100.0);
bool dry_run = config.value("dry_run", false);

// Check if key exists
if (config.contains("api_key")) {
    std::string api_key = config["api_key"];
}

// Iterate over array
if (config.contains("symbols") && config["symbols"].is_array()) {
    for (const auto& sym : config["symbols"]) {
        std::cerr << "Symbol: " << sym << std::endl;
    }
}

// Nested objects
if (config.contains("exchange")) {
    std::string name = config["exchange"].value("name", "unknown");
    bool testnet = config["exchange"].value("testnet", false);
}
```

### Custom Result Data

```cpp
the0::result("success", "Trade executed", {
    {"trade_id", "12345"},
    {"symbol", "BTC/USDT"},
    {"side", "buy"},
    {"price", 45000.50},
    {"quantity", 0.1},
    {"timestamp", "2024-01-15T10:30:00Z"}
});
```

## Testing

The SDK includes a comprehensive test suite using [doctest](https://github.com/doctest/doctest) (fetched via CMake FetchContent):

```bash
cd tests
mkdir -p build && cd build
cmake ..
make
./the0_test
```

## License

SDK (`the0.h`): Apache 2.0 - See LICENSE file in the root of this repository.
