---
title: "Scala Quick Start"
description: "Build your first Scala 3 trading bot with the0"
tags: ["custom-bots", "scala", "jvm", "quick-start"]
order: 15
---

# Scala Quick Start

Scala combines functional programming elegance with access to the entire Java ecosystem, making it excellent for complex trading strategies that benefit from immutable data and pattern matching. The strong type system catches errors at compile time, and the JVM provides mature performance characteristics. This guide walks through building an SMA crossover bot that monitors stock prices and emits trading signals.

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

You'll also need Scala 3 and SBT installed locally for building. See [scala-lang.org/download](https://www.scala-lang.org/download/) for setup instructions.

## Project Structure

Create a new directory for your bot:

```bash
mkdir sma-crossover
cd sma-crossover
mkdir -p src/main/scala project
```

A Scala bot requires these files:

```
sma-crossover/
├── build.sbt                # SBT project configuration
├── project/
│   ├── build.properties     # SBT version
│   └── plugins.sbt          # sbt-assembly plugin
├── src/main/scala/
│   └── Main.scala           # Bot entry point
├── bot-config.yaml          # Bot metadata and runtime settings
├── bot-schema.json          # Configuration schema for users
└── target/scala-3.3.1/
    └── sma-bot-assembly-1.0.0.jar  # Fat JAR (after sbt assembly)
```

The entry point in `bot-config.yaml` must point to the assembled JAR file. You compile and assemble locally before deploying.

## Defining Bot Metadata

Create `bot-config.yaml`:

```yaml
name: sma-crossover
description: "SMA crossover strategy bot with Yahoo Finance data"
version: 1.0.0
author: "your-name"
type: realtime
runtime: scala3

entrypoints:
  bot: target/scala-3.3.1/sma-bot-assembly-1.0.0.jar

schema:
  bot: bot-schema.json

readme: README.md

metadata:
  categories: [trading, technical-analysis]
  tags: [sma, crossover, scala, functional]
  complexity: beginner
```

The `entrypoints.bot` field points to the assembled JAR. The path must match your sbt-assembly output.

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

## Configuring SBT

Create `build.sbt`:

```scala
ThisBuild / scalaVersion := "3.3.1"
ThisBuild / version := "1.0.0"

lazy val root = (project in file("."))
  .settings(
    name := "sma-bot",

    assembly / mainClass := Some("Main"),
    assembly / assemblyJarName := "sma-bot-assembly-1.0.0.jar",

    libraryDependencies ++= Seq(
      "the0" %% "the0-sdk" % "0.1.0",
      "io.circe" %% "circe-core" % "0.14.6",
      "io.circe" %% "circe-parser" % "0.14.6",
      "com.softwaremill.sttp.client3" %% "core" % "3.9.3"
    )
  )
```

Create `project/build.properties`:

```properties
sbt.version=1.9.9
```

Create `project/plugins.sbt`:

```scala
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.1.5")
```

## Writing the Bot Logic

Create `src/main/scala/Main.scala`:

```scala
import the0.Input
import io.circe.parser._
import sttp.client3._
import scala.util.{Try, Success, Failure}

object Main extends App {
  // Bot state
  var prevShortSma: Option[Double] = None
  var prevLongSma: Option[Double] = None

  // Parse configuration from environment
  val (botId, configJson) = Input.parse()

  val config = parse(configJson).getOrElse(io.circe.Json.Null)
  val cursor = config.hcursor

  val symbol = cursor.get[String]("symbol").getOrElse("AAPL")
  val shortPeriod = cursor.get[Int]("short_period").getOrElse(5)
  val longPeriod = cursor.get[Int]("long_period").getOrElse(20)
  val updateIntervalMs = cursor.get[Int]("update_interval_ms").getOrElse(60000)

  Input.log(s"Bot $botId started - $symbol SMA($shortPeriod/$longPeriod)")

  val backend = HttpClientSyncBackend()

  // Main loop - runs until process is terminated
  while (true) {
    Try {
      val prices = fetchYahooFinance(backend, symbol)

      if (prices.length < longPeriod) {
        Input.log(s"Insufficient data: need $longPeriod prices, have ${prices.length}")
      } else {
        // Calculate current price and change
        val currentPrice = prices.last
        val previousPrice = prices(prices.length - 2)
        val changePct = (currentPrice - previousPrice) / previousPrice * 100

        // Emit price metric
        Input.metric("price", Map(
          "symbol" -> symbol,
          "value" -> roundTo(currentPrice, 2),
          "change_pct" -> roundTo(changePct, 3)
        ))

        // Calculate SMAs
        val shortSma = calculateSma(prices, shortPeriod)
        val longSma = calculateSma(prices, longPeriod)

        // Emit SMA metric
        Input.metric("sma", Map(
          "symbol" -> symbol,
          "short_sma" -> roundTo(shortSma, 2),
          "long_sma" -> roundTo(longSma, 2),
          "short_period" -> shortPeriod,
          "long_period" -> longPeriod
        ))

        // Check for crossover signal
        (prevShortSma, prevLongSma) match {
          case (Some(prevShort), Some(prevLong)) =>
            checkCrossover(prevShort, prevLong, shortSma, longSma).foreach { signal =>
              val confidence = Math.min(Math.abs(shortSma - longSma) / longSma * 100, 0.95)
              val direction = if (signal == "BUY") "above" else "below"

              Input.metric("signal", Map(
                "type" -> signal,
                "symbol" -> symbol,
                "price" -> roundTo(currentPrice, 2),
                "confidence" -> roundTo(confidence, 2),
                "reason" -> s"SMA$shortPeriod crossed $direction SMA$longPeriod"
              ))
            }
          case _ => ()
        }

        // Update state
        prevShortSma = Some(shortSma)
        prevLongSma = Some(longSma)
      }
    } match {
      case Failure(e) => Input.log(s"Error: ${e.getMessage}")
      case Success(_) => ()
    }

    Thread.sleep(updateIntervalMs)
  }

  def fetchYahooFinance(backend: SttpBackend[Identity, Any], symbol: String): Vector[Double] = {
    val url = s"https://query1.finance.yahoo.com/v8/finance/chart/$symbol?interval=1d&range=1mo"
    val response = basicRequest
      .get(uri"$url")
      .header("User-Agent", "the0-sma-bot/1.0")
      .send(backend)

    response.body match {
      case Right(body) =>
        parse(body).toOption.flatMap { json =>
          json.hcursor
            .downField("chart")
            .downField("result")
            .downArray
            .downField("indicators")
            .downField("quote")
            .downArray
            .downField("close")
            .as[Vector[Option[Double]]].toOption
            .map(_.flatten)
        }.getOrElse(Vector.empty)
      case Left(_) => Vector.empty
    }
  }

  def calculateSma(prices: Vector[Double], period: Int): Double = {
    if (prices.length < period) 0.0
    else prices.takeRight(period).sum / period
  }

  def checkCrossover(prevShort: Double, prevLong: Double,
                     currShort: Double, currLong: Double): Option[String] = {
    if (prevShort <= prevLong && currShort > currLong) Some("BUY")
    else if (prevShort >= prevLong && currShort < currLong) Some("SELL")
    else None
  }

  def roundTo(value: Double, decimals: Int): Double = {
    val multiplier = Math.pow(10, decimals)
    Math.round(value * multiplier) / multiplier
  }
}
```

## SDK Functions

The Scala SDK provides these functions in the `the0.Input` object:

### Input.parse()

Reads `BOT_ID` and `BOT_CONFIG` from environment variables. Returns a tuple of the bot ID and configuration JSON string:

```scala
val (botId, configJson) = Input.parse()
```

### Input.metric(type, data)

Emits a metric to the platform dashboard:

```scala
Input.metric("price", Map("symbol" -> "AAPL", "value" -> 150.25))
Input.metric("signal", Map("type" -> "BUY", "confidence" -> 0.85))
```

### Input.log(message)

Writes a log message:

```scala
Input.log(s"Processing $symbol")
Input.log(s"Error: ${e.getMessage}")
```

### Input.success(message)

Reports successful execution for scheduled bots:

```scala
Input.success("Analysis complete")
```

### Input.error(message)

Reports failure and terminates with exit code 1:

```scala
if (prices.isEmpty) {
  Input.error("No price data available")
}
```

### Input.result(json)

Outputs a custom JSON result:

```scala
Input.resultRaw("""{"status":"success","trade_id":"abc123"}""")
```

## Building

Build the fat JAR with sbt-assembly:

```bash
sbt assembly
```

The JAR appears at `target/scala-3.3.1/sma-bot-assembly-1.0.0.jar`, matching the entry point in `bot-config.yaml`.

## Testing Locally

Test by setting environment variables:

```bash
export BOT_ID="test-bot"
export BOT_CONFIG='{"symbol":"AAPL","short_period":5,"long_period":20,"update_interval_ms":5000}'
export CODE_MOUNT_DIR="/tmp"

java -jar target/scala-3.3.1/sma-bot-assembly-1.0.0.jar
```

## Deploying

Deploy your assembled JAR to the platform:

```bash
the0 custom-bot deploy
```

The CLI packages the JAR along with configuration files and uploads everything. You must assemble locally before deploying.

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

With your first Scala bot deployed, explore these topics:

- [Configuration](./configuration) - Complete bot-config.yaml reference
- [Bot Types](./bot-types) - Scheduled vs realtime execution models
- [Metrics](./metrics) - Dashboard metrics and structured logging
- [Custom Frontends](./custom-frontends) - Build React dashboards for your bot
- [Testing](./testing) - Local testing patterns and best practices
