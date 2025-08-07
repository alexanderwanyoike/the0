---
title: "Welcome to the0"
description: "Your comprehensive algorithmic trading platform"
order: 1
---

# Welcome to the0

The0 is an automated trading platform that allows users to **create**, **deploy**, and **manage** trading bots of all shapes and sizes. It allows developers to create custom bots in either Python or JavaScript, and provides a marketplace for developers to share and discover bots to traders. The platform also includes features for **backtesting**, **monitoring**, and **deploying** bots in a secure and compliant manner.

## What is Algorithmic Trading?

Algorithmic trading is the use of computer algorithms to automate trading decisions and execute trades in financial markets. It allows traders to leverage technology to analyze market data, identify trading opportunities, and execute trades.

Algos do not sleep, eat, or take breaks, making them ideal for 24/7 trading environments. They can be used for a variety of strategies, including market making, arbitrage, and trend following.

## What the0 Does

The0 provides a comprehensive platform for algorithmic trading, allowing users to create and deploy trading bots with ease. It offers a user-friendly interface for bot creation, a marketplace for sharing and discovering bots.

- **Open Standards**: The0 provides uses open standards such as `YAML` and `jsonschema` for bot creation, allowing developers to define their bots in a _structured_ and _standardized_ way. This makes it easy to create, share, and deploy bots across different environments.

- **Framework Agnostic**: The0 does not constrain developers to a library or framework, allowing them to use any library or framework they prefer for their bot development (within reason sorry not pytorch). As long as the bot defines a an entrypoint `main` function, bot configuration in `bot-config.yaml`, and provides input configuration schemas, it can be deployed on the platform.

- **Execution Models**: We only provide **bot types** eg. `scheduled` and `realtime` that users can use to define their bots. Think of them as _execution models_ rather than algos. This gives bot developers the flexibility to implement their own trading logic and algos.

## What the0 Does Not Do

- **Not a Specific Library/Framework**: The0 does not provide a specific library or framework for bot development, allowing users to use any technology stack they prefer.

- **Not Strategy-Specific**: The0 does not provide a specific trading strategy or algorithm, allowing users to create their own strategies and algorithms.

- **Not HFT Platform**: The0 is not a HFT (High Frequency Trading) platform, it is designed for retail traders and developers who want to create and deploy trading bots without the complexity of HFT systems. It is not designed for ultra-low latency trading or high-frequency strategies that require specialized infrastructure and hardware to tuned to microsecond or nanosecond execution speeds (e.g FPGAs) for specific sets of strategies which would is not scalable for a platform like this.

> **💸 HFT Reality Check**  
> If you want to know more about HFT watch [this](https://www.youtube.com/watch?v=iwRaNYa8yTw) youtube video and understand that you will need millions of dollars in infrastructure to even get started (sorry to kill your dreams). Honestly you dont need low latency trading to make money in the markets, you can make a lot of money with simple strategies that are not HFT.
