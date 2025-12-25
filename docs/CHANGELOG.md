# Changelog

All notable changes to the docs service will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- **Rust Quick Start Guide** - Complete guide for building Rust trading bots with the0 SDK
  - SDK installation and usage with `the0::input` module
  - Project structure and configuration examples
  - Best practices for error handling, validation, and logging

### Changed

- **Migrated from Next.js to VitePress** - Simplified documentation framework
- Updated dark/light mode colors to match frontend exactly
- Simplified home page to documentation index (removed hero layout)
- Updated deployment documentation to remove marketplace publishing references

### Added

- VitePress configuration with sidebar navigation and local search
- Custom theme with the0 brand colors
- `llms.txt` for AI tool parseability
- `netlify.toml` for Netlify deployment
- `nginx.conf` for local Docker serving

### Removed

- Next.js framework and all React components
- Custom documentation services (file-system.ts, search-service.ts)
- shadcn/ui components
- Compliance section and 0vers33r references
- Marketplace publishing documentation
- Backtest CLI commands documentation
- Backtesting terminology and implementation guides

## [0.1.0] - 2024-12-02

### Added

- Initial documentation site with Next.js 15
- Custom bot development guides
- CLI documentation
- Terminology section
