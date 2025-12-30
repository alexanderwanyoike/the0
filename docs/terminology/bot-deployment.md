---
title: "Bot Deployment"
description: "The process of deploying trading bots to the platform"
tags: ["terminology", "deployment"]
order: 5
---

# Bot Deployment

Bot deployment refers to two distinct processes: deploying a custom bot definition to the platform, and deploying bot instances from that definition. Understanding the difference between these two operations is essential for working with the0.

## Custom Bot Deployment

Custom bot deployment uploads your bot's code, configuration, and schema to the platform. This makes the bot available for creating instances.

```bash
the0 custom-bot deploy
```

This command runs from your bot's project directory and performs several steps:

1. Reads `bot-config.yaml` to determine the bot's metadata and runtime
2. Vendors dependencies (installs packages into the project for Python and Node.js)
3. Compiles code for compiled languages (Rust, C#, Scala, C++, Haskell)
4. Builds the frontend if `hasFrontend: true` is set
5. Packages everything into a deployable archive
6. Uploads to the platform

After deployment, you can view your custom bots with:

```bash
the0 custom-bot list
```

## Bot Instance Deployment

Bot instance deployment creates a running instance of an existing custom bot with specific configuration values.

```bash
the0 bot deploy config.json
```

The configuration file specifies which custom bot to use and provides parameter values:

```json
{
  "name": "my-trading-bot",
  "type": "realtime/sma-crossover",
  "version": "1.0.0",
  "symbol": "AAPL",
  "short_period": 5,
  "long_period": 20,
  "update_interval_ms": 60000
}
```

The `type` field format is `{execution-model}/{custom-bot-name}`. The `version` field specifies which version of the custom bot to use.

## Deployment Workflow

A typical deployment workflow follows this sequence:

1. Develop your custom bot locally with `bot-config.yaml`, entry point, and schema
2. Test locally by setting `BOT_ID` and `BOT_CONFIG` environment variables
3. Deploy the custom bot with `the0 custom-bot deploy`
4. Create a configuration file for your bot instance
5. Deploy the instance with `the0 bot deploy config.json`
6. Monitor logs with `the0 bot logs <bot_id>`

## Updating Deployments

To update a custom bot, increment the version in `bot-config.yaml` and run `the0 custom-bot deploy` again. The new version is stored alongside existing versions.

Bot instances are immutable with respect to version. To use a newer version of a custom bot, delete the existing instance and deploy a new one:

```bash
the0 bot delete <bot_id>
the0 bot deploy config.json  # with updated version
```

You can update a bot instance's configuration parameters using `the0 bot update`, but this does not change the underlying custom bot version.

## Related Topics

- [Bots](/terminology/bots) - Running bot instances
- [Custom Bots](/terminology/custom-bots) - Bot definitions and templates
- [Custom Bot Commands](/the0-cli/custom-bot-commands) - CLI reference for custom bot deployment
- [Bot Commands](/the0-cli/bot-commands) - CLI reference for bot instance management
