# Changelog

All notable changes to the the0 CLI will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- **C/C++ GCC 13 language support** - Build C/C++ bots locally using Docker before deployment
  - `CppBuilder` implementation of `LanguageBuilder` interface
  - Detects `CMakeLists.txt` (priority) or `Makefile` projects
  - CMake projects: installs cmake, builds with `cmake .. && make`
  - Makefile projects: builds with `make`
  - Uses `gcc:13` Docker image for cross-platform builds
- **C# .NET 8 language support** - Build C# bots locally using Docker before deployment
  - `DotnetBuilder` implementation of `LanguageBuilder` interface
  - Detects `.csproj` projects and builds with `dotnet publish -c Release`
  - Uses `mcr.microsoft.com/dotnet/sdk:8.0` Docker image for cross-platform builds
- **LanguageBuilder interface** - Refactored language builds with common interface
  - Extracted Rust builder to `builder_rust.go`
  - Added `builder_dotnet.go` for C# support
  - Extensible pattern for adding new compiled languages
- **Rust language support** - Build Rust bots locally using Docker before deployment
  - `ShouldBuildRust()` - Detects Cargo.toml projects
  - `PerformRustBuild()` - Compiles Rust projects in `rust:latest` Docker container
  - `BuildRustIfNeeded()` - Entry point for conditional Rust building
  - Private git dependency support via `GITHUB_TOKEN` environment variable
- **Centralized logger package** - New `internal/logger` package with modern CLI output (spinners, colored icons)
- **Verbose mode** - Added `-v/--verbose` global flag for detailed output during operations
- **Improved error messages** - Validation errors (HTTP 400) now display full response details

### Changed
- **Professional CLI output** - Replaced cyberpunk-themed messages with clean, professional text
- **Spinner-based progress** - Operations now show animated spinners instead of static text
- **Custom bot schema command** - Simplified usage from `schema <bot|backtest> <version> <custom-bot-name>` to `schema <version> <custom-bot-name>`
- **Bot configuration** - The `Entrypoints` and `Schema` structs now only contain the `Bot` field

### Removed
- **Self-update command** - Removed `the0 self-update` command (use package manager or manual download)
- **Check-update command** - Removed `the0 check-update` command
- **Startup update check** - Removed automatic update check on CLI startup
- **Backtest commands** - Removed `the0 backtest deploy`, `the0 backtest list`, and `the0 backtest delete` commands as part of platform simplification
- **Backtest schema support** - The `the0 custom-bot schema` command no longer accepts `backtest` as an entry point type; only `bot` schemas are supported
- **Backtest configuration fields** - Removed `entrypoints.backtest` and `schema.backtest` fields from `bot-config.yaml` configuration

## [0.0.0-dev] - Development

### Added
- Initial CLI implementation with auth, bot, and custom-bot commands
- Docker-based Python dependency vendoring
- File exclusion system with `.the0ignore` support
- API key authentication and management
