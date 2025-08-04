---
title: 'Backtesting'
description: 'Testing trading strategies with historical data'
tags: ['terminology', 'backtesting', 'testing']
order: 6
---

# Backtesting

Backtesting is the process of testing a trading strategy using historical market data to evaluate its potential performance. In the0, developers can implement any backtesting logic they prefer, including sophisticated methods like walk-forward analysis and Monte Carlo simulations. The platform requires backtest functions to return results in a structured format containing **metrics**, **plots**, and **tables**. see the [Backtest Development Guide](/docs/custom-bot-development/backtesting) for more details on implementation.

![First image description](/docs/backtesting-1.png) ![Second image description](/docs/backtesting-2.png)

---

## Overview

Backtesting allows traders to:

- Evaluate strategy performance before risking real capital
- Identify potential issues and edge cases
- Optimize parameters for better results
- Build confidence in their trading approach

---

## How to perform Backtesting

To perform backtesting for a Custom Bot/User Bot.

### Custom Bots

- Go to the [Custom Bots](/custom-bots) section
- Select a bot and click "Backtest"
- Configure the backtest parameters (date range, data source, etc.)
- Run the backtest and view results

### User Bots

- Go to the [User Bots](/user-bots) section
- Select a bot and click "Backtest"
- Configure the backtest parameters (date range, data source, etc.)
- Run the backtest and view results

> Not all Custom Bots or User Bots may support backtesting. Check the bot details for availability.

---

## Related Terms

- [Custom Bots](/docs/terminology/custom-bots) - Define backtest entry points
- [Monitoring](/docs/terminology/monitoring) - Track live performance vs backtest
