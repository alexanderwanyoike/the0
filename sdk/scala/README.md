# the0-sdk (Scala)

SDK for building trading bots on the0 platform in Scala 3.

## Installation

### Option 1: Git Dependency (Recommended)

Add to your `build.sbt`:

```sbt
// Use a specific release tag
lazy val the0Sdk = RootProject(uri("https://github.com/alexanderwanyoike/the0.git#v1.1.0") / "sdk" / "scala")

lazy val root = (project in file("."))
  .dependsOn(the0Sdk)
```

> **Note:** Replace `v1.1.0` with the latest release tag.

### Option 2: Copy the Package

Copy every file in `src/main/scala/the0/` to your project's source directory.

## Requirements

- Scala 3.x
- SBT 1.9+
- sbt-assembly plugin (for creating fat JARs)

## Usage

### Basic Example

```scala
import the0.Input

object Main extends App {
  // Parse bot configuration from environment
  val (botId, config) = Input.parse()

  System.err.println(s"Bot $botId starting")
  System.err.println(s"Config: $config")

  // Your trading logic here

  Input.success("Bot executed successfully")
}
```

### With JSON Parsing

Using a JSON library like circe or play-json:

```scala
import the0.Input
import io.circe.parser._

object Main extends App {
  val (botId, configJson) = Input.parse()

  // Parse config as JSON
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

## API Reference

### `Input.parse(): (String, String)`

Parse bot configuration from environment variables.

- Returns: `(botId, configJson)` tuple
- `botId`: Value of `BOT_ID` environment variable (empty string if not set)
- `configJson`: Value of `BOT_CONFIG` environment variable (`"{}"` if not set)

```scala
val (botId, config) = Input.parse()
```

### `Input.success(message: String[, data: String]): Unit`

Write a success result to the result file, `/bot/result.json`. The optional `data` is JSON text, placed under the result's `data` key.

```scala
Input.success("Trade completed")
// Writes: {"status":"success","message":"Trade completed"}

Input.success("Trade completed", """{"order_id":"o-1","filled":0.5}""")
// Writes: {"status":"success","message":"Trade completed","data":{"order_id":"o-1","filled":0.5}}
```

### `Input.error(message: String[, data: String]): Nothing`

Write an error result to the result file, print the message to stderr as an error log line, and exit with code 1. The optional `data` is JSON text and goes in the result only.

```scala
Input.error("Failed to connect")
// Writes: {"status":"error","message":"Failed to connect"}
// Prints: {"level":"error","message":"Failed to connect","timestamp":"2026-10-12T09:30:00.123Z"}
// Then exits with code 1
```

### `Input.result(status: String, message: String): Unit`

Write a custom result with status and message to the result file.

```scala
Input.result("warning", "Rate limit approaching")
// Writes: {"status":"warning","message":"Rate limit approaching"}
```

### `Input.resultRaw(json: String): Unit`

Write a raw JSON string to the result file.

```scala
Input.resultRaw("""{"status":"success","trade_id":"abc123","price":45000.50}""")
```

### `Input.log(message: String, data: String = "{}", level: LogLevel = LogLevel.Info): Unit`

Print a log line on stderr. The fields of `data`, a JSON object as text, go alongside `message`. The timestamp is a UTC ISO 8601 string.

```scala
Input.log("Order placed", """{"order_id":"o-1"}""")
// Prints: {"level":"info","message":"Order placed","timestamp":"2026-10-12T09:30:00.123Z","order_id":"o-1"}
```

### `Input.metric(metricType: String, json: String): Unit`

Print a metric line on stdout. The fields of `json`, a JSON object as text, go alongside `_metric`. The timestamp is a UTC ISO 8601 string.

```scala
Input.metric("price", """{"symbol":"BTC/USD","value":45000.5}""")
// Prints: {"_metric":"price","timestamp":"2026-10-12T09:30:00.123Z","symbol":"BTC/USD","value":45000.5}
```

### Query params

A query handler reads its params in two ways:
- `req.get(key)` and `req.getOrElse(key, default)` return the param as text. A string comes back as itself, and any other JSON value as its JSON text, such as `5`, `true`, `null` or `{"side":"buy"}`.
- `req.getJson(key)` returns the param as a `JsonValue` (`Null`, `Bool`, `Num`, `Str`, `Arr` or `Obj`), keeping its JSON type.

```scala
Query.handler("/signals") { req =>
  val limit = req.getJson("limit") match {
    case Some(JsonValue.Num(n)) => n.toInt
    case _ => 10
  }
  s"""{"limit": $limit}"""
}
```

In query mode, `State.set`, `State.delete` and `State.clear` throw `Query.ReadOnlyStateError`, which is the same type as `the0.ReadOnlyStateError`.

## Project Setup

### build.sbt

```sbt
ThisBuild / scalaVersion := "3.4.0"
ThisBuild / version := "1.0.0"

lazy val root = (project in file("."))
  .settings(
    name := "my-scala-bot",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % "0.14.6",
      "io.circe" %% "circe-parser" % "0.14.6"
    ),
    assembly / mainClass := Some("Main"),
    assembly / assemblyJarName := "my-scala-bot-assembly.jar"
  )
```

### project/plugins.sbt

```sbt
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.1.5")
```

## Bot Configuration

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

## Deployment

```bash
# Deploy your bot
the0 custom-bot deploy
```

The CLI automatically builds your Scala project using `sbt assembly` in Docker before deployment.

## Best Practices

### Error Handling

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

Use stderr for logs (stdout is reserved for JSON output):

```scala
System.err.println("DEBUG: Processing trade...")  // Logs
println("{...}")  // Reserved for JSON result - use Input.success/error/result instead
```

### Async Operations

```scala
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

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

## License

Apache 2.0 - See LICENSE file in the root of this repository.
