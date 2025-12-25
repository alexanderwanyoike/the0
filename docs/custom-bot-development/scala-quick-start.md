---
title: "Scala Quick Start"
description: "Build your first Scala 3 trading bot with the0"
tags: ["custom-bots", "scala", "jvm", "quick-start"]
order: 14
---

# Scala Quick Start Guide

Build a trading bot in Scala 3 with the0's scala3 runtime.

---

## Prerequisites

- Scala 3.x and SBT (for local development)
- the0 CLI installed
- Valid the0 API key

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

lazy val root = (project in file("."))
  .settings(
    name := "my-scala-bot",
    assembly / mainClass := Some("Main"),
    assembly / assemblyJarName := "my-scala-bot-assembly.jar"
  )
```

---

## Step 3: Configure SBT

Create `project/build.properties`:

```properties
sbt.version=1.9.9
```

Create `project/plugins.sbt`:

```scala
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.1.5")
```

---

## Step 4: Write Your Bot

Create `src/main/scala/Main.scala`:

```scala
object Main extends App {
  // Get bot configuration from environment
  val botId = sys.env.getOrElse("BOT_ID", "unknown")
  val config = sys.env.getOrElse("BOT_CONFIG", "{}")

  System.err.println(s"Bot $botId starting...")
  System.err.println(s"Config: $config")

  // Your trading logic here
  // Example: Parse config, fetch prices, execute trades

  // Output success result
  println("""{"status":"success","message":"Bot executed successfully"}""")
}
```

---

## Step 5: Using the SDK (Recommended)

Copy the `the0` package from `sdk/scala/` for proper JSON handling:

```scala
import the0.Input

object Main extends App {
  val (botId, config) = Input.parse()

  System.err.println(s"Bot $botId starting...")

  // Your trading logic here

  Input.success("Bot executed successfully")
}
```

---

## Step 6: Create Bot Configuration

Create `bot-config.yaml`:

```yaml
name: my-scala-bot
description: "A Scala trading bot"
version: "1.0.0"
author: "Your Name"
type: scheduled
runtime: scala3

entrypoints:
  bot: src/main/scala/Main.scala

schema:
  bot: bot-schema.json

readme: README.md
```

---

## Step 7: Define Parameter Schema

Create `bot-schema.json`:

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "Bot Configuration",
  "description": "Configuration for the Scala trading bot",
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

## Step 8: Deploy

```bash
# Deploy your bot
the0 custom-bot deploy
```

The build happens automatically in Docker - no need to compile locally!

---

## SDK API Reference

The `the0.Input` object provides these functions:

### `Input.parse(): (String, String)`

Parse bot configuration from environment variables.

```scala
val (botId, config) = Input.parse()
// botId: Value of BOT_ID env var
// config: JSON string from BOT_CONFIG
```

### `Input.success(message: String): Unit`

Output a success result to stdout.

```scala
Input.success("Trade completed")
// Outputs: {"status":"success","message":"Trade completed"}
```

### `Input.error(message: String): Nothing`

Output an error result and exit with code 1.

```scala
Input.error("Failed to connect")
// Outputs: {"status":"error","message":"Failed to connect"}
// Exits with code 1
```

### `Input.result(status: String, message: String): Unit`

Output a custom result with status and message.

```scala
Input.result("warning", "Rate limit approaching")
```

### `Input.resultRaw(json: String): Unit`

Output a raw JSON string to stdout.

```scala
Input.resultRaw("""{"status":"success","trade_id":"abc123"}""")
```

---

## Example: HTTP Request Bot

Using sttp for HTTP requests:

```scala
import the0.Input
import sttp.client3._

object Main extends App {
  val (botId, config) = Input.parse()
  System.err.println(s"Bot $botId fetching data...")

  val backend = HttpClientSyncBackend()

  val response = basicRequest
    .get(uri"https://api.example.com/price")
    .send(backend)

  response.body match {
    case Right(body) =>
      System.err.println(s"Response: $body")
      Input.success("Data fetched successfully")
    case Left(error) =>
      Input.error(s"Request failed: $error")
  }
}
```

Add to `build.sbt`:

```scala
libraryDependencies += "com.softwaremill.sttp.client3" %% "core" % "3.9.3"
```

---

## Example: JSON Parsing with Circe

```scala
import the0.Input
import io.circe.parser._

object Main extends App {
  val (botId, configJson) = Input.parse()

  parse(configJson) match {
    case Right(json) =>
      val symbol = json.hcursor.get[String]("symbol").getOrElse("BTC/USDT")
      val amount = json.hcursor.get[Double]("amount").getOrElse(100.0)

      System.err.println(s"Trading $symbol with amount $amount")

      // Your trading logic here

      Input.success(s"Trade completed for $symbol")

    case Left(error) =>
      Input.error(s"Failed to parse config: ${error.message}")
  }
}
```

Add to `build.sbt`:

```scala
libraryDependencies ++= Seq(
  "io.circe" %% "circe-core" % "0.14.6",
  "io.circe" %% "circe-parser" % "0.14.6"
)
```

---

## Best Practices

### Error Handling

Use Try for robust error handling:

```scala
import the0.Input
import scala.util.{Try, Success, Failure}

object Main extends App {
  Try {
    val (botId, config) = Input.parse()

    if (botId.isEmpty) {
      throw new IllegalArgumentException("Bot ID is required")
    }

    // Your trading logic here

    Input.success("Bot executed successfully")
  } match {
    case Success(_) => // Already handled
    case Failure(e) => Input.error(e.getMessage)
  }
}
```

### Logging

You can freely use `println` for debugging - the SDK's output functions use a special marker (`THE0_RESULT:`) that the runtime recognizes:

```scala
// These all go to logs - use freely for debugging
println("Starting trade execution...")
println(s"Current price: $price")
System.err.println("Warning: High volatility detected")

// This is the bot's result (automatically prefixed with marker)
Input.success("Trade completed successfully")
```

For structured logging and metrics, see the [Bot Metrics & Logging](/custom-bot-development/metrics) guide.

### Async Operations with Futures

```scala
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import the0.Input

object Main extends App {
  val (botId, config) = Input.parse()

  val result = Await.result(
    Future {
      // Async trading logic
      "Trade completed"
    },
    30.seconds
  )

  Input.success(result)
}
```

---

## Related Documentation

- [Configuration Reference](/custom-bot-development/configuration)
- [Bot Types](/custom-bot-development/bot-types)
- [Deployment Guide](/custom-bot-development/deployment)
