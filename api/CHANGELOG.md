# Changelog

All notable changes to the API service will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Structured logging with `nestjs-pino` (#70)
  - Added `LoggerModule` with environment-aware configuration (pretty in dev, JSON in prod)
  - Added `pino-http` for automatic HTTP request/response logging with timing
  - Added `pino-pretty` for readable development logs
  - Added sensitive data redaction for auth headers and cookies
  - Environment variables: `LOG_LEVEL` (debug/info/warn/error), `NODE_ENV` controls output format
- Reusable mock logger helper for tests at `src/test/mock-logger.ts`

### Changed

- Migrated from `winston` to `nestjs-pino` for structured logging (#70)
  - Updated all services to use `PinoLogger` via `@InjectPinoLogger()` decorator
  - Updated abstract repository classes to use pino
  - Updated standalone database scripts (migrate, seed) to use pino
- Added `LoggerModule` import to all feature modules requiring logger injection
  - `CustomBotModule`, `BotModule`, `NatsModule`, `LogsModule`, `AuthModule`
- Updated test files to provide mock PinoLogger providers
  - `custom-bot.service.spec.ts`, `storage.service.spec.ts`, `nats.service.spec.ts`
  - `bot.controller.spec.ts`, `bot.service.spec.ts`

### Removed

- Removed `winston` dependency in favor of `nestjs-pino`
- Removed 28+ emoji instances from log messages across 12 files
- Removed verbose debug logging (JWT payload dumps, query traces, endpoint entry logs)
- Removed backtest module and all related functionality (#33)
  - Deleted `src/backtest/` module
  - Deleted `src/database/schema/backtests.ts`
  - Removed BacktestModule from app.module.ts
  - Removed backtest schema from database connection
  - Removed BACKTEST_BUCKET from configuration
  - Removed backtest event subjects from NATS service
  - Removed backtest entrypoints and schemas from CustomBotConfig types
