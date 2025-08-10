# the0 CLI

🚀 **the0 Custom Bot CLI** - Deploy your trading bots like a boss!

A comprehensive command-line tool for managing trading bots, deploying custom algorithms, and interacting with the the0 platform ecosystem.

## Installation

### From Source
```bash
git clone https://github.com/the0platform/cli.git
cd cli
make build
make install
```

### From Releases
Download the latest binary from the [releases page](https://github.com/the0platform/cli/releases).

## Quick Start

1. **Set up your API key:**
   ```bash
   the0 auth login
   ```

2. **Deploy a bot instance:**
   ```bash
   # Create a config.json file with your bot configuration
   echo '{"name": "my-trading-bot", "type": "trading", "version": "1.0.0"}' > config.json
   the0 bot deploy config.json
   ```

3. **List your deployed bots:**
   ```bash
   the0 bot list
   ```

## Commands Reference

### 🔐 Authentication Commands
Manage your API credentials and authentication status.

```bash
the0 auth login          # Set or update API key
the0 auth status         # Check API key validity  
the0 auth logout         # Remove saved API key
```

**Examples:**
```bash
# Login with your API key
the0 auth login
# Enter your the0 API key: your-api-key-here

# Check if your API key is valid
the0 auth status
# ✅ API key is valid (expires: never)

# Logout and remove stored credentials
the0 auth logout
# 👋 Logged out successfully
```

### 🤖 Bot Instance Management
Deploy, manage, and monitor your trading bot instances.

#### Deploy Bot Instance
```bash
the0 bot deploy <config.json>
```
Deploy a new bot instance using a configuration file. The bot name is automatically extracted from the config file.

**Config file format:**
```json
{
  "name": "my-trading-bot",
  "type": "trading",
  "version": "1.0.0",
  "schedule": "0 0 * * *",
  "parameters": {
    "risk_level": "medium",
    "max_position_size": 1000
  }
}
```

#### List Bot Instances
```bash
the0 bot list
```
Display all your deployed bot instances with their status, configuration, and metadata.

#### Update Bot Instance
```bash
the0 bot update <bot_id> <config.json>
```
Update an existing bot instance with new configuration.

#### Delete Bot Instance
```bash
the0 bot delete <bot_id>
```
Delete a deployed bot instance. Requires confirmation.

**Examples:**
```bash
# Deploy a new bot
the0 bot deploy my-bot-config.json
# 🚀 Starting bot deployment process...
# 📦 Bot name: algorithmic-trader
# ✅ Config file loaded successfully!
# 🎉 BOOM! Your bot instance is now deployed!

# List all bot instances
the0 bot list
# 🤖 Found 3 bot instance(s):
# ID       | Name              | Type    | Version | Schedule  | Created At
# bot_123  | algorithmic-trader| trading | 1.0.0   | 0 0 * * * | 2024-01-15 10:30
# bot_456  | arbitrage-bot     | trading | 2.1.0   | N/A       | 2024-01-14 15:45

# Update a bot
the0 bot update bot_123 updated-config.json
# 🔄 Starting bot update process...
# 🎉 Bot updated successfully!

# Delete a bot (with confirmation)
the0 bot delete bot_123
# ⚠️ Are you sure you want to delete bot 'bot_123'?
# This action cannot be undone! 💥
# Type 'yes' to confirm: yes
# 💀 Bot deleted successfully!
```

### 🛠️ Custom Bot Development
Deploy and manage your custom trading algorithms to the the0 marketplace.

#### Deploy Custom Bot
```bash
the0 custom-bot deploy
```
Package and deploy your custom bot to the the0 marketplace from the current directory.

**🐳 Docker-Based Python Vendoring:**
If your bot includes a `requirements.txt` file, the CLI automatically performs dependency vendoring using Docker:
- Detects Python dependencies in `requirements.txt`
- Uses Linux x86-64 Python 3.11 container for cross-platform compatibility
- Vendors packages to `vendor/` directory for deployment
- Gracefully falls back if Docker is unavailable

**📁 File Exclusion with .the0ignore:**
Control which files are included in your bot deployment with a `.the0ignore` file:
- Gitignore-style pattern matching for flexible file exclusion
- Supports glob patterns (`*`, `**`, `?`) and negation (`!pattern`)
- Built-in protection for `vendor/` and `node_modules/` directories
- Default exclusions for common temporary files (`.log`, `.pyc`, `__pycache__/`, etc.)

**Required files in your bot directory:**
```
my-bot/
├── bot-config.yaml      # Bot configuration and metadata
├── main.py             # Main bot entry point
├── backtest.py         # Backtesting script
├── schema.json         # Backtest parameter schema
├── bot-schema.json     # Bot parameter schema
├── requirements.txt    # Python dependencies (optional)
├── .the0ignore         # File exclusion patterns (optional)
├── vendor/             # Vendored dependencies (auto-generated)
└── README.md           # Bot documentation
```

**Bot configuration (bot-config.yaml):**
```yaml
name: my-awesome-bot
description: "A trading bot that makes money while you sleep"
version: 1.0.0
author: your-name
type: scheduled  # or 'realtime', 'event'

entrypoints:
  bot: main.py
  backtest: backtest.py

schema:
  bot: bot-schema.json
  backtest: schema.json

readme: README.md

metadata:
  tags: ["arbitrage", "crypto"]
  category: "defi"
```

**File exclusion (.the0ignore):**
```
# Exclude log files but keep important ones
*.log
!production.log
!important.log

# Exclude temporary files
*.tmp
*.temp
**/*.cache

# Exclude test directories and files
test/
tests/
__tests__/
*_test.py
test_*.py

# Exclude documentation (except main README)
docs/**/*.md
!docs/README.md

# Exclude development and build artifacts
.env
.env.local
build/
dist/
**/*.pyc
**/*.pyo
__pycache__/

# Exclude OS-specific files
.DS_Store
Thumbs.db

# Exclude configuration files except production
config_*
!config_production.yaml

# Note: vendor/ and node_modules/ are automatically protected
```

#### List Custom Bot Versions
```bash
the0 custom-bot versions <type|name>
```
List all available versions for a specific custom bot type or name.

#### List Custom Bots
```bash
the0 custom-bot list
```
Display all custom bots you've deployed to the marketplace.

#### Get Custom Bot Schema
```bash
the0 custom-bot schema <bot|backtest> <version> <custom-bot-name>
```
Retrieve the JSON schema for either the bot or backtest entrypoint of a custom bot.


### 🔄 CLI Self-Update
Keep your CLI up-to-date with automatic update notifications and seamless binary updates.

#### Check for Updates
```bash
the0 check-update
```
Check if a newer version of the CLI is available without installing it.

**Options:**
- `--timeout duration` - Request timeout (default: 10s)

#### Self-Update
```bash
the0 self-update
```
Download and install the latest version of the CLI binary.

**Options:**
- `--check-only` - Only check for updates, don't install
- `--force` - Force update even if already on latest version  
- `--yes` - Skip confirmation prompt
- `--timeout duration` - Download timeout (default: 5m)

**Features:**
- 🚀 **Automatic startup notifications** - CLI shows update availability on startup
- 🔒 **Secure downloads** - SHA256 checksum verification prevents corrupted updates
- 📱 **Cross-platform** - Works on Windows, macOS, and Linux (Intel/ARM)
- ⚡ **Fast checks** - Update detection completes in <2 seconds
- 🛡️ **Safe updates** - Binary replacement with rollback on failure
- 🔧 **Progress indication** - Real-time download progress during updates

**Examples:**
```bash
# Check for updates manually
the0 check-update
# 🔍 Checking for updates...
# ⚠️  Update available!
#    Current: 1.0.0
#    Latest:  1.0.1
# 
# 💡 Run 'the0 self-update' to update now.

# Update to latest version
the0 self-update
# 🔍 Checking for updates...
# ⚠️  Update available!
#    Current: 1.0.0
#    Latest:  1.0.1
# 
# ❓ Do you want to update now? [y/N]: y
# 
# 🚀 Starting update process...
# 📦 Downloading: 25% (1024/4096 bytes)
# 📦 Downloading: 50% (2048/4096 bytes)
# 📦 Downloading: 100% (4096/4096 bytes)
# ✅ Successfully updated to version 1.0.1
# 
# 🎉 Update completed successfully!
#    New version: 1.0.1
# 
# 💡 The update will take effect the next time you run the CLI.

# Check for updates only (no installation)
the0 self-update --check-only
# 🔍 Checking for updates...
# ✅ You're already running the latest version: 1.0.1

# Force update to latest version
the0 self-update --force --yes
# 🔄 Forcing update to latest version: 1.0.1
# 🚀 Starting update process...
# ✅ Successfully updated to version 1.0.1
```

## Configuration

### Environment Variables
- `THE0_API_URL` - Override API base URL (default: `https://api.the0.dev`)
- `THE0_CLI_UPDATE_CHANNEL` - Set update channel (`production` or `staging`, default: `production`)
- `THE0_QUIET` - Suppress startup update notifications when set to any value

### Configuration Files
- **Bot instances**: Use JSON configuration files with required fields (`name`, `type`, `version`)
- **Custom bots**: Use YAML configuration files (`bot-config.yaml`) with comprehensive metadata
- **Authentication**: API keys stored securely in user's home directory

## Bot Configuration Examples

### Simple Trading Bot
```json
{
  "name": "simple-trader",
  "type": "trading",
  "version": "1.0.0",
  "schedule": "*/15 * * * *",
  "parameters": {
    "symbol": "BTCUSDT",
    "strategy": "momentum",
    "risk_level": "low"
  }
}
```

### Analysis Bot
```json
{
  "name": "market-analyzer",
  "type": "analysis",
  "version": "2.0.1",
  "parameters": {
    "timeframe": "1h",
    "indicators": ["RSI", "MACD", "Bollinger"],
    "alert_threshold": 0.05
  }
}
```

### Real-time Event Bot
```json
{
  "name": "news-trader",
  "type": "event",
  "version": "1.2.0",
  "parameters": {
    "sources": ["twitter", "reddit", "news"],
    "sentiment_threshold": 0.7,
    "max_trades_per_day": 5
  }
}
```

## Error Handling

The CLI provides detailed error messages and suggestions:

```bash
# Missing required field in config
the0 bot deploy invalid-config.json
# ❌ Bot name not found in config file. Please add a 'name' field to your config.json

# Invalid API key
the0 bot list
# ❌ Authentication failed: API key is invalid or revoked
# 🔑 API key appears to be invalid. Let's get a new one...

# Bot not found
the0 bot delete nonexistent-bot
# ❌ bot not found: nonexistent-bot
```

## Development

### Prerequisites
- Go 1.21+
- Make
- Docker (optional, for Python dependency vendoring)

### Building
```bash
make build                           # Build development binary (version: 0.0.0-dev)
make build VERSION=v2025.01.15-123  # Build with custom version
make install                         # Install CLI locally
make release                         # Cross-platform release builds
make release VERSION=v2025.01.15-123 # Release builds with custom version
```

**Version Injection**: The CLI supports version injection at build time. The CI pipeline automatically injects the appropriate version during builds:
- **Local development**: Shows `0.0.0-dev` 
- **Staging builds**: Shows `develop-YYYY.MM.DD-RUN_NUMBER` (e.g., `develop-2025.01.15-164`)
- **Production builds**: Shows `vYYYY.MM.DD-RUN_NUMBER` (e.g., `v2025.01.15-164`)

### Testing
```bash
make test           # Run all tests
make test-coverage  # Run tests with coverage report
make lint           # Run linter (requires golangci-lint)
make fmt            # Format code
make verify         # Full verification (format + lint + test + build)
```

### Project Structure

```
.
├── main.go              # CLI entry point
├── cmd/                 # Command implementations
│   ├── auth.go         # Authentication commands
│   ├── bot.go          # Bot instance management
│   ├── custom_bot.go   # Custom bot development
│   ├── user_bot.go     # User bot management
│   ├── check_update.go # Update checking command
│   └── self_update.go  # Self-update command
├── internal/           # Internal packages
│   ├── api.go         # API client and data structures
│   ├── auth.go        # Authentication logic
│   ├── config.go      # Configuration handling
│   ├── ignore.go      # .the0ignore file parsing and pattern matching
│   ├── vendor.go      # Docker-based Python vendoring
│   ├── zip.go         # File packaging utilities
│   ├── version.go     # Version parsing and comparison
│   └── updater.go     # Self-update logic and binary management
├── tests/             # Test files
│   ├── api_test.go    # API client tests
│   ├── auth_test.go   # Authentication tests
│   ├── bot_test.go    # Bot management tests
│   ├── cli_test.go    # CLI command tests
│   ├── config_test.go # Configuration tests
│   ├── ignore_test.go # File ignore pattern tests
│   ├── vendor_test.go # Docker vendoring tests
│   ├── zip_test.go    # ZIP utilities tests
│   ├── version_test.go # Version parsing tests
│   ├── updater_test.go # Self-update logic tests
│   └── integration_update_test.go # Update command integration tests
├── Makefile           # Build automation
├── go.mod            # Go modules
├── go.sum            # Go module checksums
├── CLAUDE.md         # AI assistant memory
└── README.md         # This file
```

## API Reference

The CLI interacts with the the0 platform API:

- **Base URL**: `https://api.the0.dev`
- **Authentication**: API key via `Authorization: ApiKey <key>` header
- **Rate Limits**: Automatic retry with exponential backoff
- **Error Handling**: Comprehensive error messages with suggested actions

### Key Endpoints
- `GET /auth/validate-api-key` - Validate API key
- `GET /bot` - List bot instances
- `POST /bot` - Deploy bot instance
- `PUT /bot/{id}` - Update bot instance
- `DELETE /bot/{id}` - Delete bot instance
- `GET /custom-bots` - List custom bots
- `POST /custom-bots/{name}` - Deploy custom bot
- `GET /user-bots` - List user bots

## Troubleshooting

### Common Issues

**Authentication Problems:**
```bash
# If you get authentication errors, try logging in again
the0 auth logout
the0 auth login
```

**Config File Issues:**
```bash
# Validate your JSON config file
cat config.json | jq .  # Should not show syntax errors
```

**Network Issues:**
```bash
# Check connectivity to the API
curl -I https://api.the0.dev/
```

**Update Issues:**
```bash
# If you see update notifications but don't want them
export THE0_QUIET=1
the0 help  # No update notifications

# To check for updates manually
the0 check-update

# If updates fail due to permissions
sudo the0 self-update  # May be needed on some systems

# To use staging channel (internal use only)
export THE0_CLI_UPDATE_CHANNEL=staging
the0 check-update
```

**Permission Issues:**
```bash
# Make sure the binary is executable
chmod +x the0
```

**Docker/Vendoring Issues:**
```bash
# Check if Docker is installed and running
docker --version
docker info

# If Docker is unavailable, the CLI will proceed without vendoring
# Manual vendoring (if needed):
pip install --target vendor -r requirements.txt
```

## Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Make your changes
4. Run `make verify` to ensure quality
5. Commit your changes (`git commit -m 'Add amazing feature'`)
6. Push to the branch (`git push origin feature/amazing-feature`)
7. Submit a pull request

## License

MIT License - see LICENSE file for details.

## Support

- 📚 Documentation: [docs.the0.dev](https://docs.the0.dev)
- 💬 Discord: [Join for support](https://discord.gg/g5mp57nK)
- 🐛 Issues: [GitHub Issues](https://github.com/the0platform/cli/issues)

---

*Built with ❤️ for the trading community by Alex and Claude*