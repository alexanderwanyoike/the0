# the0-sdk (Scala)

SDK for building trading bots on the0 platform in Scala 3.

## Installation

### Option 1: GitHub Packages (Maven)

Add the GitHub Packages resolver and dependency to your `build.sbt`:

```sbt
resolvers += "GitHub Package Registry" at "https://maven.pkg.github.com/alexanderwanyoike/the0"
credentials += Credentials(
  "GitHub Package Registry",
  "maven.pkg.github.com",
  "YOUR_GITHUB_USERNAME",
  sys.env.getOrElse("GITHUB_TOKEN", "")
)

libraryDependencies += "dev.the0" %% "the0-sdk" % "0.1.0"
```

### Option 2: Copy the Package

Copy `src/main/scala/the0/Input.scala` to your project's source directory.

### Option 3: Local Dependency

```sbt
lazy val the0Sdk = ProjectRef(file("path/to/sdk/scala"), "root")

lazy val root = (project in file("."))
  .dependsOn(the0Sdk)
```

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

### `Input.success(message: String): Unit`

Output a success result to stdout.

```scala
Input.success("Trade completed")
// Outputs: {"status":"success","message":"Trade completed"}
```

### `Input.error(message: String): Nothing`

Output an error result to stdout and exit with code 1.

```scala
Input.error("Failed to connect")
// Outputs: {"status":"error","message":"Failed to connect"}
// Then exits with code 1
```

### `Input.result(status: String, message: String): Unit`

Output a custom result with status and message.

```scala
Input.result("warning", "Rate limit approaching")
// Outputs: {"status":"warning","message":"Rate limit approaching"}
```

### `Input.resultRaw(json: String): Unit`

Output a raw JSON string to stdout.

```scala
Input.resultRaw("""{"status":"success","trade_id":"abc123","price":45000.50}""")
```

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

## Publishing (Maintainers)

This package is published to GitHub Packages (Maven).

### Prerequisites

1. Create a GitHub Personal Access Token with `write:packages` scope:
   https://github.com/settings/tokens/new?scopes=write:packages,read:packages

2. Set the token as an environment variable:
   ```bash
   export GITHUB_TOKEN="ghp_your_token_here"
   ```

### Publish

```bash
sbt publish
```

### Version Bump

Update the version in `build.sbt`:

```sbt
ThisBuild / version := "0.2.0"
```

Then publish.

## License

Apache 2.0 - See LICENSE file in the root of this repository.
