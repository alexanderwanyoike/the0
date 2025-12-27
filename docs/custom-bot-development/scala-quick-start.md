---
title: "Scala Quick Start"
description: "Build your first Scala 3 trading bot with the0"
tags: ["custom-bots", "scala", "jvm", "quick-start"]
order: 14
---

# Scala Quick Start Guide

Build trading bots in Scala 3 with the0's JVM-based runtime. Scala combines functional programming elegance with Java ecosystem access, making it excellent for complex trading strategies.

---

## Why Scala for Trading Bots?

Scala offers unique advantages for algorithmic trading:

- **Functional Programming**: Immutable data and pure functions reduce bugs
- **Type Safety**: Strong typing catches errors at compile time
- **JVM Ecosystem**: Access to all Java libraries and frameworks
- **Concurrency**: Built-in support for actors and futures
- **Pattern Matching**: Elegant handling of market conditions
- **Expression-Oriented**: Every construct returns a value

**When to Choose Scala:**
- Complex strategies with many market conditions
- Teams coming from Java or functional programming
- Need for concurrent/parallel processing
- Building on existing JVM infrastructure

**Popular Libraries for Trading:**
- `sttp` - Modern HTTP client with functional API
- `circe` - Type-safe JSON encoding/decoding
- `Akka` - Actor-based concurrency (for realtime bots)
- `cats` / `cats-effect` - Functional programming utilities
- `fs2` - Streaming data processing

---

## Prerequisites

- Scala 3.x and SBT installed ([setup guide](https://www.scala-lang.org/download/))
- the0 CLI installed
- Valid the0 API key
- Basic understanding of Scala syntax

---

## Project Structure

```
my-scala-bot/
├── build.sbt             # SBT project file
├── project/
│   ├── build.properties  # SBT version
│   └── plugins.sbt       # sbt-assembly plugin
├── src/main/scala/
│   └── Main.scala        # Your bot entry point
├── bot-config.yaml       # Bot configuration
├── bot-schema.json       # Parameter schema
├── config.json           # Example configuration
└── README.md             # Documentation
```

---

## Step 1: Create Your Project

```bash
mkdir my-scala-bot
cd my-scala-bot
mkdir -p src/main/scala project
```

---

## Step 2: Create build.sbt

```scala
ThisBuild / scalaVersion := "3.3.3"
ThisBuild / version := "1.0.0"

// the0 SDK - add via git dependency
lazy val the0Sdk = RootProject(uri("https://github.com/alexanderwanyoike/the0.git#v1.1.0") / "sdk" / "scala")

lazy val root = (project in file("."))
  .dependsOn(the0Sdk)
  .settings(
    name := "my-scala-bot",

    // sbt-assembly configuration
    assembly / mainClass := Some("Main"),
    assembly / assemblyJarName := "my-scala-bot-assembly.jar",

    // Dependencies
    libraryDependencies ++= Seq(
      // JSON parsing
      "io.circe" %% "circe-core" % "0.14.6",
      "io.circe" %% "circe-parser" % "0.14.6",

      // HTTP client (optional)
      "com.softwaremill.sttp.client3" %% "core" % "3.9.3"
    )
  )
```

> **Note:** Replace `v1.1.0` with the latest release tag. Alternatively, copy `src/main/scala/the0/Input.scala` from the SDK to your project.

---

## Step 3: Configure SBT

Create `project/build.properties`:

```properties
sbt.version=1.9.9
```

Create `project/plugins.sbt`:

```scala
// Plugin to create fat JARs with all dependencies
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.1.5")
```

---

## Step 4: Copy the SDK

Copy the `the0` package from `sdk/scala/` to `src/main/scala/`. The SDK provides:
- Configuration parsing from environment variables
- Result output functions
- Type-safe JSON handling

---

## Step 5: Write Your Bot

Create `src/main/scala/Main.scala`:

```scala
import the0.Input
import io.circe.parser._

/**
 * Main entry point for the trading bot.
 *
 * The the0 SDK handles:
 * - Reading BOT_ID and BOT_CONFIG from environment
 * - Writing results to the correct output file
 * - Proper exit codes for success/failure
 */
object Main extends App {
  // Parse bot configuration from environment
  val (botId, configJson) = Input.parse()

  System.err.println(s"Bot $botId starting...")

  // Parse JSON configuration with Circe
  parse(configJson) match {
    case Right(json) =>
      // Extract configuration with defaults
      val cursor = json.hcursor
      val symbol = cursor.get[String]("symbol").getOrElse("BTC/USDT")
      val amount = cursor.get[Double]("amount").getOrElse(100.0)

      System.err.println(s"Trading $symbol with amount $amount")

      // ===========================================
      // YOUR TRADING LOGIC GOES HERE
      // ===========================================

      // Example: Validate configuration
      if (amount <= 0) {
        Input.error("Amount must be positive")
      }

      // Example: Fetch market data
      // val price = fetchPrice(symbol)

      // Example: Execute trade
      // val orderId = placeTrade(symbol, amount)

      // ===========================================
      // END OF TRADING LOGIC
      // ===========================================

      // Signal success
      Input.success(s"Trade executed for $symbol")

    case Left(error) =>
      Input.error(s"Failed to parse config: ${error.message}")
  }
}
```

---

## Step 6: Create Bot Configuration

Create `bot-config.yaml`:

```yaml
name: my-scala-bot
description: "A functional Scala trading bot"
version: "1.0.0"
author: "Your Name"
type: scheduled
runtime: scala3

# The entrypoint is the source file
entrypoints:
  bot: src/main/scala/Main.scala

schema:
  bot: bot-schema.json

readme: README.md

metadata:
  categories: [trading]
  instruments: [crypto]
  tags: [scala, functional, jvm]
```

**Note:** The `runtime: scala3` tells the platform to compile with Scala 3. You don't need to compile locally.

---

## Step 7: Define Parameter Schema

Create `bot-schema.json`:

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "Scala Bot Configuration",
  "description": "Configuration for the Scala trading bot",
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

## Step 8: Test Locally

```bash
# Build fat JAR
sbt assembly

# Set environment variables
export BOT_ID="test-bot-123"
export BOT_CONFIG='{"symbol":"BTC/USDT","amount":100}'
export CODE_MOUNT_DIR="/tmp"

# Run
java -jar target/scala-3.3.3/my-scala-bot-assembly.jar
```

---

## Step 9: Deploy

```bash
the0 custom-bot deploy
```

The platform will:
1. Compile your Scala code with SBT
2. Create a fat JAR with all dependencies
3. Deploy to the JVM runtime

No need to compile locally - it all happens in the cloud!

---

## SDK API Reference

The `the0.Input` object provides these functions:

### `Input.parse(): (String, String)`

Parse bot configuration from environment variables:

```scala
val (botId, configJson) = Input.parse()
// botId: Value of BOT_ID env var
// configJson: JSON string from BOT_CONFIG
```

### `Input.success(message: String): Unit`

Output a success result:

```scala
Input.success("Trade completed")
// Outputs: {"status":"success","message":"Trade completed"}
```

### `Input.error(message: String): Nothing`

Output an error result and exit with code 1:

```scala
if (amount <= 0) {
  Input.error("Amount must be positive")
  // Exits here - code below never runs
}
```

### `Input.result(status: String, message: String): Unit`

Output a result with custom status:

```scala
Input.result("warning", "Rate limit approaching")
```

### `Input.resultRaw(json: String): Unit`

Output raw JSON:

```scala
Input.resultRaw("""{"status":"success","trade_id":"abc123","filled":0.5}""")
```

---

## Example: Price Fetcher with sttp

Here's a complete example that fetches real price data:

```scala
import the0.Input
import io.circe.parser._
import sttp.client3._

object Main extends App {
  val (botId, configJson) = Input.parse()

  parse(configJson) match {
    case Right(json) =>
      val symbol = json.hcursor.get[String]("symbol").getOrElse("BTCUSDT")

      System.err.println(s"Bot $botId fetching price for $symbol")

      // Fetch price from Binance
      val backend = HttpClientSyncBackend()
      val response = basicRequest
        .get(uri"https://api.binance.com/api/v3/ticker/price?symbol=$symbol")
        .send(backend)

      response.body match {
        case Right(body) =>
          parse(body).flatMap(_.hcursor.get[String]("price")) match {
            case Right(price) =>
              System.err.println(s"Current price: $$$price")
              Input.resultRaw(s"""{"status":"success","symbol":"$symbol","price":$price}""")
            case Left(e) =>
              Input.error(s"Failed to parse price: ${e.message}")
          }
        case Left(error) =>
          Input.error(s"Request failed: $error")
      }

    case Left(error) =>
      Input.error(s"Failed to parse config: ${error.message}")
  }
}
```

---

## Best Practices

### 1. Pattern Matching for Error Handling

Use Scala's pattern matching for clean error handling:

```scala
import the0.Input
import scala.util.{Try, Success, Failure}

object Main extends App {
  val (botId, config) = Input.parse()

  Try {
    // Your trading logic
    executeTrade("BTC/USDT", 100.0)
  } match {
    case Success(tradeId) =>
      Input.success(s"Trade $tradeId completed")
    case Failure(e) =>
      Input.error(s"Trade failed: ${e.getMessage}")
  }

  def executeTrade(symbol: String, amount: Double): String = {
    // Trade logic here
    "trade_123"
  }
}
```

### 2. Functional Configuration Parsing

Use for-comprehensions for clean config extraction:

```scala
import io.circe.parser._
import io.circe.Decoder

case class Config(symbol: String, amount: Double, apiKey: String)

object Config {
  implicit val decoder: Decoder[Config] = Decoder.forProduct3(
    "symbol", "amount", "api_key"
  )(Config.apply)
}

// In main:
parse(configJson).flatMap(_.as[Config]) match {
  case Right(config) =>
    System.err.println(s"Trading ${config.symbol}")
    // Use strongly-typed config
  case Left(error) =>
    Input.error(s"Invalid config: ${error.message}")
}
```

### 3. Logging

Both stdout and stderr go to your bot's logs:

```scala
println("Starting trade...")                      // Goes to log
System.err.println("DEBUG: Details...")           // Goes to log
```

### 4. Immutable Data

Prefer immutable data structures:

```scala
// Good - immutable case class
case class Trade(symbol: String, amount: Double, price: Double) {
  def total: Double = amount * price
}

val trade = Trade("BTC/USDT", 0.5, 45000.0)
val updatedTrade = trade.copy(price = 45100.0)  // Creates new instance
```

---

## Adding Dependencies

Add libraries to `build.sbt`:

```scala
libraryDependencies ++= Seq(
  // JSON
  "io.circe" %% "circe-core" % "0.14.6",
  "io.circe" %% "circe-parser" % "0.14.6",
  "io.circe" %% "circe-generic" % "0.14.6",  // For automatic derivation

  // HTTP
  "com.softwaremill.sttp.client3" %% "core" % "3.9.3",
  "com.softwaremill.sttp.client3" %% "circe" % "3.9.3",  // JSON integration

  // Functional programming
  "org.typelevel" %% "cats-core" % "2.10.0",

  // Date/time
  "java.time" % "java-time" % "1.0.0"
)
```

---

## Related Documentation

- [Configuration Reference](/custom-bot-development/configuration)
- [Bot Types](/custom-bot-development/bot-types)
- [Custom Frontends](/custom-bot-development/custom-frontends)
- [Deployment Guide](/custom-bot-development/deployment)
