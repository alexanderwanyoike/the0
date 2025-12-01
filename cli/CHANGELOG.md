# Changelog

All notable changes to the the0 CLI will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Removed
- **Backtest commands** - Removed `the0 backtest deploy`, `the0 backtest list`, and `the0 backtest delete` commands as part of platform simplification
- **Backtest schema support** - The `the0 custom-bot schema` command no longer accepts `backtest` as an entry point type; only `bot` schemas are supported
- **Backtest configuration fields** - Removed `entrypoints.backtest` and `schema.backtest` fields from `bot-config.yaml` configuration

### Changed
- **Custom bot schema command** - Simplified usage from `schema <bot|backtest> <version> <custom-bot-name>` to `schema <version> <custom-bot-name>`
- **Bot configuration** - The `Entrypoints` and `Schema` structs now only contain the `Bot` field

## [0.0.0-dev] - Development

### Added
- Initial CLI implementation with auth, bot, and custom-bot commands
- Docker-based Python dependency vendoring
- File exclusion system with `.the0ignore` support
- Self-update functionality
- API key authentication and management
