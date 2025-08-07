---
title: "Bot Deployment"
description: "The process of deploying trading bots to the platform"
tags: ["terminology", "deployment"]
order: 5
---

# Bot Deployment

Bot Deployment is the process of deploying a Bot to the platform, configuring it with specific parameters, and launching it for live or paper trading. This process can be done through the platform UI or using the CLI, providing flexibility for different user preferences and automation needs.

---

## Overview

Deployment transforms a Custom Bot template into a running Bot instance that:

- Executes trading logic according to its configuration
- Connects to specified exchanges and data sources
- Runs continuously or on schedule based on bot type
- Reports performance metrics and logs

> Exchanges, Data Sources, Asset Classes and other aspects are all up to the Custom Bot developer to define. The0 platform provides the infrastructure to deploy and run these bots, but the actual trading logic is defined in the Custom Bot by the developer.

---

## Deployment Methods

### Platform UI Deployment

1. **Navigate to Bot Source**:

Go to [Custom Bots](/custom-bots) section. Note you have to deploy a custom bot before it appears in the custom bots section and also the custom bot will need to be reviewed by the 0vers33r before it can be approved for deployment.

2. **Configure Deployment**:

   - Click "Deploy" on the desired bot
   - Fill in configuration parameters

3. **Launch**:
   - Review configuration
   - Confirm deployment
   - Monitor startup logs

> Note you can always update the configuration of a Bot after it has been deployed. This allows you to change parameters without having to redeploy the bot.

---

### CLI Deployment

```bash
# Deploy a custom bot
the0 bot deploy <config.json>
```

for more details see [Bot Commands](/docs/the0-CLI/bot-commands).

## Configuration Process

### Parameter Configuration

During deployment, you must provide:

- **Required Parameters**: As defined in the bot's schema
- **Scheduling**: For `scheduled` bots, cron expressions or intervals

---

## Best Practices

### Before Deploying

- Always test with paper trading first
- Review bot documentation thoroughly
- If available, backtest the **shit** out of it
- Understand all configuration parameters

---

## Related Terms

- [Bots](/docs/terminology/bots) - The deployed instances
- [Custom Bots](/docs/terminology/custom-bots) - Templates for deployment
- [Monitoring](/docs/terminology/monitoring) - Post-deployment tracking
