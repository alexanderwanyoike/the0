---
title: "Bots"
description: "Understanding bot instances in the0 platform"
tags: ["terminology", "bots"]
order: 1
---

# Bots

These are the actual trading bots instances that you create and run in the platform. They are can either be created on the platform or using the CLI and viewed on the [dashboard](/dashboard). Bots are instances of [Custom Bots](./custom-bots) that are deployed to the platform. They run independently and can be monitored, backtested, and managed through the platform.

---

## Key Concepts

### Bot Instances

A Bot is a running instance of a Custom Bot template. Think of it this way:

- **Custom Bot**: The **blueprint** defines the trading logic
- **Bot**: The actual running instance executing trades

### Creating Bots

Bots can be created in two ways:

1. **Through the Platform UI**: Navigate to your [Custom Bots](/custom-bots) and select a bot or [My Bots](/user-bots) section and click "Deploy"
2. **Using the CLI**: Use the `the0 bot deploy` command with your configuration (see [Bot Commands](/docs/the0-CLI/bot-commands) for details)

### Bot Lifecycle

Once deployed, a Bot:

- Runs independently based on its type (`scheduled` or `realtime`)
- Executes the trading logic defined in its Custom Bot template
- Can be started, stopped

### Management Features

- **Logs**: Access to execution logs and debugging information
- **Configuration**: Ability to update parameters without redeploying
- **Backtesting**: Test modifications before applying to live trading

---

## Related Terms

- [Custom Bots](/docs/terminology/custom-bots) - The templates that Bots are created from
- [Bot Deployment](/docs/terminology/bot-deployment) - The process of creating Bot instances
