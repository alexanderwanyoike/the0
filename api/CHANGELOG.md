# Changelog

All notable changes to the API service will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Removed

- Removed backtest module and all related functionality (#33)
  - Deleted `src/backtest/` module
  - Deleted `src/database/schema/backtests.ts`
  - Removed BacktestModule from app.module.ts
  - Removed backtest schema from database connection
  - Removed BACKTEST_BUCKET from configuration
  - Removed backtest event subjects from NATS service
  - Removed backtest entrypoints and schemas from CustomBotConfig types
