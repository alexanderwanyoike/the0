---
title: "Custom Bots"
description: "Bot templates and blueprints in the0"
tags: ["terminology", "custom-bots", "development"]
order: 2
---

# Custom Bots

Are can be though of as the blueprints for [Bots](./bots). They are the blueprints that define the actual trading algorithm that will be executed by the [Bots](./bots). Custom Bots are created using the The0 CLI and can be shared with other users through the Custom Bot Marketplace. They are defined in a structured way using YAML and JSON Schema, allowing for easy customization and deployment.

---

## Understanding Custom Bots

### Blueprint Architecture

Custom Bots serve as reusable programs that:

- Define the trading algorithm and logic
- Specify configuration parameters through JSON Schema
- Include all necessary dependencies and requirements
- Can be instantiated multiple times as different Bots

### Key Components

1. **bot-config.yaml**: The main configuration file defining the bot's metadata and structure
2. **Entry Points**: The main execution functions for bot logic and backtesting
3. **Schemas**: JSON Schema definitions for bot and backtest configuration
4. **Dependencies**: Language-specific dependency files (requirements.txt, package.json)

### Development Process

Custom Bots are:

- Created using either Python, JavaScript (more languages coming soon!)
- Developed locally with your preferred tools and frameworks
- Tested thoroughly before deployment
- Deployed to the platform using `the0 custom-bot deploy`

### Sharing and Monetization

Once created, Custom Bots can be:

- Kept private for personal use
- Published to the marketplace for free
- Monetized by setting a price in the marketplace

---

## Benefits

- **Reusability**: Deploy the same Custom Bot multiple times with different configurations
- **Version Control**: Manage different versions of your trading strategies
- **Collaboration**: Share and sell your strategies to other traders
- **Flexibility**: Want to build ML bots with Python (with scikit-learn) or web3 powered bots with JavaScript? The0 supports them both!\*\*

> _Note: Support for more languages coming soon! Watch this space 🎉_

> _Note: There are some limitations on the libraries you can use your maximum code size is 1GB, so no pyTorch or TensorFlow yet! But you can use scikit-learn, pandas, and other lightweight libraries for classical machine learning. If you want to use heavy weight modules delegate that model to an infrence endpoint and make the bot query that_

---

## Related Terms

- [Bots](/docs/terminology/bots) - The running instances of Custom Bots
- [Marketplace](/docs/terminology/marketplace) - Where Custom Bots are shared and sold
- [Bot Deployment](/docs/terminology/bot-deployment) - Creating Bot instances from Custom Bots
