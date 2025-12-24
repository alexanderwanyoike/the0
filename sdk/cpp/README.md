# the0 C/C++ SDK

Header-only SDK for building trading bots on the0 platform in C or C++.

## Installation

Simply copy `the0.h` to your project or include it directly:

```cpp
#include "the0.h"
```

## Requirements

- C++11 or later
- Standard library only (no external dependencies)

## Usage

### Basic Example

```cpp
#include "the0.h"
#include <iostream>

int main() {
    // Parse bot configuration from environment
    auto [bot_id, config] = the0::parse();

    std::cerr << "Bot " << bot_id << " starting" << std::endl;
    std::cerr << "Config: " << config << std::endl;

    // Your trading logic here

    the0::success("Bot executed successfully");
    return 0;
}
```

### C++11 Compatible Version

```cpp
#include "the0.h"
#include <iostream>

int main() {
    std::pair<std::string, std::string> input = the0::parse();
    std::string bot_id = input.first;
    std::string config = input.second;

    std::cerr << "Bot " << bot_id << " starting" << std::endl;

    the0::success("Bot executed successfully");
    return 0;
}
```

## API Reference

### `the0::parse() -> std::pair<std::string, std::string>`

Parse bot configuration from environment variables.

- Returns: `(bot_id, config)` tuple
- `bot_id`: Value of `BOT_ID` environment variable (empty string if not set)
- `config`: Value of `BOT_CONFIG` environment variable (defaults to `"{}"`)

### `the0::success(const std::string& message)`

Output a success result to stdout.

```cpp
the0::success("Trade completed");
// Outputs: {"status":"success","message":"Trade completed"}
```

### `the0::error(const std::string& message)`

Output an error result to stdout and exit with code 1.

```cpp
the0::error("Failed to connect");
// Outputs: {"status":"error","message":"Failed to connect"}
// Then exits with code 1
```

### `the0::result(const std::string& json_str)`

Output a custom JSON result to stdout.

```cpp
the0::result("{\"status\":\"success\",\"trade_id\":\"abc123\",\"amount\":100.5}");
```

## Project Setup

### Using CMake

Create a `CMakeLists.txt`:

```cmake
cmake_minimum_required(VERSION 3.10)
project(my-cpp-bot)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_executable(my-cpp-bot main.cpp)
```

### Using Makefile

Create a `Makefile`:

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

### Logging

Use stderr for logs (stdout is reserved for JSON output):

```cpp
std::cerr << "DEBUG: Processing trade..." << std::endl;
```

### Error Handling

```cpp
#include "the0.h"
#include <stdexcept>

int main() {
    try {
        auto [bot_id, config] = the0::parse();

        // Your trading logic

        the0::success("Trade completed");
        return 0;
    } catch (const std::exception& e) {
        the0::error(e.what());
    }
}
```

### Configuration Parsing

For JSON parsing, consider using a library like nlohmann/json:

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

## License

Apache 2.0 - See LICENSE file in the root of this repository.
