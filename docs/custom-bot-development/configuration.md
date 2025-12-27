---
title: "Configuration"
description: "Complete guide to bot configuration and schemas"
tags: ["custom-bots", "configuration", "schemas"]
order: 3
---

# Bot Configuration Reference

This comprehensive guide covers all aspects of configuring custom bots, from basic metadata to advanced schema definitions.

---

## Bot Configuration File (bot-config.yaml)

The `bot-config.yaml` file is the main configuration file that defines your bot's metadata, structure, and requirements.

### Complete Structure

```yaml
name: string # Required: Bot identifier
description: string # Required: Brief description
version: string # Required: Semantic version (e.g., "1.0.0")
author: string # Required: Author name or organization
type: string # Required: scheduled | realtime
runtime: string # Required: python3.11 | nodejs20 | rust-stable | dotnet8 | gcc13 | scala3 | ghc96

entrypoints: # Required: Entry point definitions
  bot: string # Required: Main bot file

schema: # Required: Schema definitions
  bot: string # Required: Bot parameter schema file

readme: string # Required: Documentation file

metadata: # Optional: Additional metadata
  categories: [string] # Bot categories for organization
  instruments: [string] # Supported instruments (stocks, crypto, forex)
  exchanges: [string] # Supported exchanges
  tags: [string] # Searchable tags
  #... Additional metadata fields
```

### Required Fields

#### Basic Information

```yaml
name: momentum-trading-bot
description: "A momentum-based trading strategy using technical indicators"
version: 1.2.0
author: TradingExperts
type: scheduled
runtime: python3.11
readme: README.md
```

**Field Descriptions:**

- **name**: Unique identifier for your custom bot (lowercase, hyphens allowed)
- **description**: Brief, clear description of what your bot does
- **version**: [Semantic versioning](https://semver.org/) format (MAJOR.MINOR.PATCH)
- **author**: Your name, organization, or handle
- **type**: Execution model for your bot
- **runtime**: Execution environment
- **readme**: Path to a markdown file with detailed documentation

#### Entry Points

```yaml
entrypoints:
  bot: main.py # Main trading logic
```

**Entry Point Types:**

- **bot**: Main execution entry point (required)
- **webhook**: Webhook event handler (for event-driven bots)
- **analysis**: Custom analysis functions
- **custom**: Any additional entry points

#### Entrypoint Paths by Runtime

The `entrypoints.bot` field specifies the exact path to your bot's executable or script. The runtime uses this path directly to start your bot.

| Runtime | Entrypoint Example | Description |
|---------|-------------------|-------------|
| **Python** | `main.py` | Python script file |
| **Node.js** | `main.js` | JavaScript entry file |
| **Rust** | `target/release/my-bot` | Compiled binary path |
| **C#/.NET** | `bin/Release/net8.0/publish/MyBot.dll` | Published DLL path |
| **C/C++** | `build/my-bot` | Compiled binary path |
| **Scala** | `target/scala-3.3.4/my-bot-assembly-1.0.0.jar` | Assembly JAR path |
| **Haskell** | `dist-newstyle/build/.../my-bot` | Cabal build output path |

**Important:** For compiled languages, the entrypoint path must match exactly where your build system outputs the executable. The runtime will not search for binaries - it uses the specified path directly.

**Example configurations:**

```yaml
# Rust bot
runtime: rust-stable
entrypoints:
  bot: target/release/sma-bot

# C# bot
runtime: dotnet8
entrypoints:
  bot: bin/Release/net8.0/publish/SmaBot.dll

# C++ bot (CMake)
runtime: gcc13
entrypoints:
  bot: build/sma_bot

# Scala bot
runtime: scala3
entrypoints:
  bot: target/scala-3.3.4/sma-bot-assembly-1.0.0.jar

# Haskell bot
runtime: ghc96
entrypoints:
  bot: dist-newstyle/build/x86_64-linux/ghc-9.6.6/sma-bot-1.0.0/x/sma-bot/opt/build/sma-bot/sma-bot
```

#### Schema Definitions

```yaml
schema:
  bot: bot-schema.json
```

Each entry point should have a corresponding schema file defining its parameters.

### Bot Types

#### Scheduled Bots

```yaml
type: scheduled
```

**Characteristics:**

- Execute on fixed schedules (cron expressions)
- Suitable for: DCA strategies, portfolio rebalancing, periodic analysis
- Execution: Single run per trigger
- State: Stateless between executions

**Use Cases:**

- Daily portfolio rebalancing
- Weekly DCA purchases
- End-of-day analysis
- Monthly performance reports

#### Realtime Bots

```yaml
type: realtime
```

**Characteristics:**

- Run continuously, processing live data
- Execution: Continuous loop
- State: Maintains state across iterations

**Use Cases:**

- Grid trading strategies
- Market making
- Arbitrage detection
- Scalping

### Runtime Environments

#### Python 3.11

```yaml
runtime: python3.11
```

**Benefits:**

- Extensive trading libraries (ccxt, pandas, numpy, scikit-learn)
- Machine learning capabilities
- Scientific computing ecosystem
- Popular in quantitative finance

**Dependency Management:**

- Use `requirements.txt` for dependencies
- Automatic Docker-based vendoring
- Support for compiled packages

#### Node.js 20

```yaml
runtime: nodejs20
```

**Benefits:**

- Excellent async/await support
- Large npm ecosystem
- Good web3 support for crypto bots

#### Rust (Stable)

```yaml
runtime: rust-stable
```

**Benefits:**

- High performance with zero-cost abstractions
- Memory safety without garbage collection
- Excellent for high-frequency and low-latency trading
- Strong type system catches errors at compile time
- Growing ecosystem for financial applications

**Dependency Management:**

- Use `Cargo.toml` for dependencies
- Include `Cargo.lock` for reproducible builds
- Dependencies are compiled at deploy time

#### C# / .NET 8

```yaml
runtime: dotnet8
```

**Benefits:**

- Mature ecosystem with excellent tooling
- Strong async/await patterns for concurrent operations
- Cross-platform support with .NET 8
- Rich standard library for financial calculations
- NuGet package ecosystem

**Dependency Management:**

- Use `.csproj` for project configuration
- NuGet packages for dependencies
- Compiled and published at deploy time

#### C/C++ (GCC 13)

```yaml
runtime: gcc13
```

**Benefits:**

- Maximum performance for compute-intensive strategies
- Direct hardware access and memory control
- Extensive libraries for numerical computing
- Ideal for ultra-low-latency trading

**Dependency Management:**

- Use `CMakeLists.txt` or `Makefile` for builds
- System libraries and header-only libraries
- Compiled at deploy time

#### Scala 3

```yaml
runtime: scala3
```

**Benefits:**

- Functional programming paradigm
- JVM ecosystem and Java interoperability
- Excellent for complex data transformations
- Strong type system with type inference
- Reactive streaming patterns

**Dependency Management:**

- Use `build.sbt` for SBT projects
- Maven/Ivy dependencies via SBT
- Compiled to fat JAR at deploy time

#### Haskell (GHC 9.6)

```yaml
runtime: ghc96
```

**Benefits:**

- Pure functional programming
- Strong static typing with type inference
- Excellent for algorithmic correctness
- Lazy evaluation for efficient data processing
- Mathematical approach to trading logic

**Dependency Management:**

- Use `.cabal` or `package.yaml` for Cabal projects
- Hackage package ecosystem
- Compiled at deploy time

### Metadata Configuration

Metadata provides additional context for your bot, improving discoverability and organization in the marketplace.

#### Categories

```yaml
metadata:
  categories:
    - trading # General trading strategies
    - arbitrage # Arbitrage strategies
    - market-making # Market making strategies
    - analysis # Analysis and signals
    - portfolio # Portfolio management
    - defi # DeFi strategies
```

#### Instruments

```yaml
metadata:
  instruments:
    - crypto # Cryptocurrencies
    - stocks # Stock markets
    - forex # Foreign exchange
    - commodities # Commodity markets
    - options # Options trading
    - futures # Futures contracts
```

#### Exchanges

```yaml
metadata:
  exchanges:
    - binance # Binance
    - coinbase # Coinbase Pro
    - kraken # Kraken
    - ftx # FTX
    - bitmex # BitMEX
    - interactive-brokers # Interactive Brokers
```

#### Tags

```yaml
metadata:
  tags:
    - momentum # Momentum strategies
    - mean-reversion # Mean reversion strategies
    - technical-analysis # Technical analysis
    - machine-learning # ML-based strategies
    - beginner # Beginner-friendly
    - high-frequency # High-frequency trading
```

#### Risk and Complexity

```yaml
metadata:
  risk_level: medium # low | medium | high
  complexity: intermediate # beginner | intermediate | advanced
  min_capital: 1000 # Minimum recommended capital ($)
  max_capital: 100000 # Maximum recommended capital ($)
```

---

## JSON Schema Configuration

JSON schemas define the parameters your bot accepts and validate user inputs.

### Bot Schema (bot-schema.json)

#### Basic Structure

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "Bot Configuration",
  "description": "Configuration parameters for the trading bot",
  "properties": {
    // Parameter definitions
  },
  "required": ["symbol", "api_key"],
  "additionalProperties": false
}
```

For more information on JSON Schema, see the [JSON Schema documentation](https://json-schema.org/).

---

## Validation and Testing

### JSON Schema Validation

```bash
# Validate schema files
jsonschema --instance config.json --schema bot-schema.json
```

### YAML Configuration Validation

```bash
# Validate YAML configuration files
python -c "import yaml; yaml.safe_load(open('bot-config.yaml'))"

# Or use yq for more detailed validation
yq eval '.' bot-config.yaml > /dev/null
```

### Debugging Tips

1. **Use Validation Tools**: Validate schemas and configurations early
2. **Test Incrementally**: Test each component individually
3. **Check Examples**: Compare with working examples
4. **Read Error Messages**: Pay attention to specific validation errors
5. **Use the CLI**: Leverage CLI validation commands

---

## Private Package Configuration

Each runtime supports private packages through standard configuration files. The CLI respects these config files during builds.

### Python

Add private indexes to `requirements.txt`:

```txt
--extra-index-url https://pypi.example.com/simple/
requests==2.28.0
my-private-package==1.0.0
```

Or configure in `~/.the0/secrets.json`:

```json
{
  "pip_index_url": "https://user:token@pypi.example.com/simple/"
}
```

### Node.js

Create `.npmrc` in your project:

```ini
@myorg:registry=https://npm.pkg.github.com
//npm.pkg.github.com/:_authToken=${GITHUB_TOKEN}
```

### Rust

Add git dependencies in `Cargo.toml`:

```toml
[dependencies]
my-private-crate = { git = "https://github.com/myorg/private-crate" }
```

Or configure `.cargo/config.toml` for private registries:

```toml
[registries.my-registry]
index = "https://my-registry.example.com/index"

[net]
git-fetch-with-cli = true
```

### C# / .NET

Create `nuget.config` in your project root:

```xml
<?xml version="1.0" encoding="utf-8"?>
<configuration>
  <packageSources>
    <add key="nuget.org" value="https://api.nuget.org/v3/index.json" />
    <add key="private" value="https://nuget.example.com/v3/index.json" />
  </packageSources>
  <packageSourceCredentials>
    <private>
      <add key="Username" value="user" />
      <add key="ClearTextPassword" value="%NUGET_TOKEN%" />
    </private>
  </packageSourceCredentials>
</configuration>
```

### Scala

Add resolvers in `build.sbt`:

```scala
resolvers += "Private" at "https://maven.example.com/releases"

credentials += Credentials(
  "Private Repo",
  "maven.example.com",
  sys.env.getOrElse("MAVEN_USER", ""),
  sys.env.getOrElse("MAVEN_TOKEN", "")
)
```

### Haskell

Configure in `cabal.project`:

```cabal
source-repository-package
  type: git
  location: https://github.com/myorg/private-package
  tag: v1.0.0
```

### C/C++

Use CMake FetchContent with git:

```cmake
include(FetchContent)
FetchContent_Declare(
  private_lib
  GIT_REPOSITORY https://github.com/myorg/private-lib.git
  GIT_TAG v1.0.0
)
FetchContent_MakeAvailable(private_lib)
```

### Global Secrets

Configure secrets via CLI for credentials passed to builds:

```bash
# Universal (all languages) - for git-based private dependencies
the0 auth secrets set github-token ghp_xxxxxxxxxxxx

# Python - private PyPI index
the0 auth secrets set pip-index-url https://user:pass@pypi.example.com/simple/

# Node.js - private npm registry
the0 auth secrets set npm-token npm_xxxxxxxxxxxx

# C#/.NET - private NuGet feed
the0 auth secrets set nuget-token oy2xxxxxxxxxxxxx

# Rust - private Cargo registry
the0 auth secrets set cargo-registry-token xxxxxxxxxxxxx

# Scala - private Maven repository
the0 auth secrets set maven-user myuser
the0 auth secrets set maven-token xxxxxxxxxxxxx
```

View configured secrets:

```bash
the0 auth secrets show
```

These secrets are stored in `~/.the0/secrets.json` and passed as environment variables during builds:

| Secret | Environment Variable | Used By |
|--------|---------------------|---------|
| `github-token` | `GITHUB_TOKEN` | All (git deps) |
| `pip-index-url` | `PIP_EXTRA_INDEX_URL` | Python |
| `npm-token` | `NPM_TOKEN` | Node.js |
| `nuget-token` | `NUGET_TOKEN` | C#/.NET |
| `cargo-registry-token` | `CARGO_REGISTRY_TOKEN` | Rust |
| `maven-user` | `MAVEN_USER` | Scala |
| `maven-token` | `MAVEN_TOKEN` | Scala |

---

## Related Documentation

- [Quick Start Guide](/custom-bot-development/python-quick-start) - Build your first bot
- [Testing Guide](/custom-bot-development/testing) - Validation and testing
- [Deployment Guide](/custom-bot-development/deployment) - Deploying your bot
