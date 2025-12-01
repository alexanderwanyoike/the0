# Changelog

All notable changes to the frontend service will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Removed

- Backtest pages (`/backtests`, `/backtests/create`, `/backtests/[id]`)
- Backtest components (`backtest-creation-form`, `bot-selection-form`, `backtest-results`, `metrics-summary`, `plots-display`, `tables-display`)
- Backtest hooks (`use-backtests`, `use-backtest`, `use-backtest-creation`, `use-combined-bot-search`, `use-backtest-sse`)
- Backtest API routes (`/api/backtests`)
- Backtest types and utilities (`Backtest` type, `canBotBeBacktested` function)
- "Backtests" navigation item from dashboard sidebar
- "Backtest" button from custom bot action buttons
- "Custom Backtesting" feature from landing page
- "Test" step from "How It Works" section on landing page

### Changed

- Landing page hero section now displays 3 key features instead of 4
- "How It Works" section now shows 3 steps (Develop → Deploy → Monitor)
