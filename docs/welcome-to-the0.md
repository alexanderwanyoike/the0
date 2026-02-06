---
title: "Welcome to the0"
description: "An open-source platform for building, deploying, and managing algorithmic trading bots"
order: 1
---

# Welcome to the0

the0 is an open-source algorithmic trading platform that provides the infrastructure for building, deploying, and managing trading bots. The platform handles execution scheduling, log aggregation, metrics collection, and dashboard visualization so developers can focus on trading logic rather than operational concerns.

## Core Concepts

the0 is built around three fundamental concepts: custom bots, bot instances, and execution models.

A **custom bot** is a reusable bot definition that contains your trading logic, configuration schema, and metadata. Custom bots are language-agnostic and can be written in Python, TypeScript, Rust, C#, Scala, C++, or Haskell. Each custom bot defines what configuration parameters it accepts through a JSON Schema, making it possible to create multiple instances with different settings.

A **bot instance** is a running deployment of a custom bot with specific configuration values. For example, a single "SMA Crossover" custom bot can have multiple instances: one monitoring AAPL with a 5/20 period crossover, another monitoring MSFT with a 10/50 period crossover.

**Execution models** determine how your bot runs. the0 supports two models: scheduled bots that run on a cron schedule (daily, hourly, every minute) and realtime bots that run continuously until stopped.

## Platform Architecture

the0 uses a microservices architecture with the following components:

The **CLI** (`the0`) is the primary interface for deploying custom bots and managing bot instances. It handles dependency vendoring, compilation for compiled languages, and deployment to the platform.

The **API** orchestrates bot lifecycle operations including deployment, scheduling, and log retrieval.

The **Runtime Services** use a master-worker architecture. The master handles workload allocation and distributes jobs to workers. Workers instantiate and manage bot containers through reconciliation loops, ensuring bots reach and maintain their desired state. The bot runner handles realtime execution while the scheduler manages cron-based scheduled execution.

The **Dashboard** provides a web interface for monitoring bot status, viewing logs, and visualizing metrics emitted by your bots.

## Supported Languages

the0 provides official SDKs for seven languages, each offering consistent APIs for configuration parsing, result reporting, and metrics emission:

| Language | Runtime | SDK Source |
|----------|---------|------------|
| Python | python3.11 | `the0-sdk` (PyPI) |
| TypeScript/Node.js | nodejs20 | `@alexanderwanyoike/the0-node` (GitHub Packages) |
| Rust | rust-stable | `the0-sdk` (crates.io) |
| C# | dotnet8 | `The0.Sdk` (NuGet) |
| Scala | scala3 | `the0-sdk` (GitHub Repository) |
| C++ | gcc13 | `the0-sdk` (GitHub Repository) |
| Haskell | ghc96 | `the0-sdk` (GitHub Repository) |

All SDKs provide the same core functions: `parse()` for reading configuration, `success()` and `error()` for reporting results, `metric()` for emitting dashboard metrics, and `log()` for structured logging.

## Design Principles

the0 is intentionally unopinionated about trading strategies. The platform provides execution infrastructure, not trading algorithms. You bring your own strategy, data sources, and broker integrations.

Configuration uses open standards. Bot definitions are specified in YAML (`bot-config.yaml`) and parameter schemas use JSON Schema draft-07. This makes bots portable and tooling-friendly.

The platform is framework-agnostic. Your bot code can use any libraries available for your chosen language. The only requirement is implementing a main entry point that the runtime can invoke.

## Scope and Limitations

the0 is designed for retail algorithmic trading with execution latencies in the seconds-to-minutes range. It is not suitable for high-frequency trading (HFT) strategies that require microsecond or nanosecond execution speeds.

The platform does not provide market data feeds or broker integrations directly. Bots are responsible for connecting to their own data sources and executing trades through their chosen brokers.

## Next Steps

To get started with the0:

1. [Install the CLI](/the0-cli/installation) and configure authentication
2. Review the [terminology](/terminology/) to understand key concepts
3. Follow a [language quick start guide](/custom-bot-development/python-quick-start) to build your first bot
4. Learn about [bot configuration](/custom-bot-development/configuration) for advanced customization
