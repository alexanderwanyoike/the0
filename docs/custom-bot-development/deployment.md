---
title: "Deployment"
description: "Deploy custom bots to the0 platform"
tags: ["custom-bots", "deployment", "cli"]
order: 6
---

# Deploying Custom Bots

Deployment is the process of uploading your custom bot to the platform so that you (or others) can create bot instances from it. The CLI handles packaging your code, vendoring dependencies, and uploading everything to the platform. This guide walks through the deployment process and explains what happens at each step.

## Prerequisites

Before deploying, ensure your bot directory contains the required files:

```
my-bot/
├── bot-config.yaml      # Bot metadata and settings
├── main.py              # Entry point (language-specific)
├── bot-schema.json      # Configuration schema
├── README.md            # Documentation
└── requirements.txt     # Dependencies (Python)
```

For other languages, the dependency file varies: `package.json` for Node.js, `Cargo.toml` for Rust, and so on. The entry point must match what's specified in your bot-config.yaml.

Test your bot locally before deploying. Set the environment variables and run directly:

```bash
export BOT_ID="test-bot"
export BOT_CONFIG='{"symbol":"AAPL"}'
export CODE_MOUNT_DIR="/tmp"
python main.py
```

If the bot runs without errors and emits the expected metrics, you're ready to deploy.

## The Deploy Command

From your bot directory, run:

```bash
the0 custom-bot deploy
```

The CLI performs several steps automatically:

1. **Validates configuration** - Checks that bot-config.yaml and bot-schema.json are valid and consistent
2. **Vendors dependencies** - For Python, runs pip install into a vendor directory using Docker for compatibility
3. **Packages the bot** - Creates a zip archive of your code, dependencies, and configuration
4. **Uploads to the platform** - Sends the package to the platform's storage
5. **Registers the version** - Makes the new version available for creating bot instances

The CLI shows progress via a spinner as it works through each step, ending with:

```
'sma-crossover' v1.0.0 deployed successfully
```

## Verifying Deployment

After deploying, verify that your bot is available:

```bash
# List your custom bots
the0 custom-bot list

# Check versions of a specific bot
the0 custom-bot versions sma-crossover

# Get the schema to verify it's correct
the0 custom-bot schema 1.0.0 sma-crossover
```

The `versions` command shows all deployed versions of your bot. Users can deploy instances of any available version.

## Creating Bot Instances

Once your custom bot is deployed, create instances that actually run your code. Create a JSON configuration file specifying the custom bot type, version, and parameters:

```json
{
  "name": "my-sma-bot",
  "type": "realtime/sma-crossover",
  "version": "1.0.0",
  "config": {
    "symbol": "AAPL",
    "short_period": 5,
    "long_period": 20,
    "update_interval_ms": 60000
  }
}
```

For scheduled bots, include a cron expression:

```json
{
  "name": "daily-portfolio-check",
  "type": "scheduled/portfolio-tracker",
  "version": "1.0.0",
  "schedule": "0 9 * * 1-5",
  "config": {
    "symbols": ["BTC", "ETH", "SOL"],
    "initial_value": 10000
  }
}
```

Deploy the instance:

```bash
the0 bot deploy instance-config.json
```

## Version Management

Each deployment creates a new version identified by the version field in bot-config.yaml. Follow semantic versioning: increment the patch version for bug fixes, minor version for new features, and major version for breaking changes.

```yaml
# bot-config.yaml
version: 1.2.3
#        │ │ └─ Patch: Bug fixes
#        │ └─── Minor: New features
#        └───── Major: Breaking changes
```

**Versions are immutable.** Once you deploy version 1.0.0, you cannot modify it. To make changes, increment the version number and deploy again. Bot instances are also tied to specific versions—to use a newer version, you must delete the instance and create a new one.

```bash
# Deploy a new version
# First, update version in bot-config.yaml to 1.0.1
the0 custom-bot deploy

# Delete old instance
the0 bot delete <bot_id>

# Create new instance with updated version
the0 bot deploy updated-config.json
```

## Dependency Vendoring

The platform runs bots in isolated containers without internet access during execution. All dependencies must be vendored (bundled) with your bot at deploy time.

### Python

The CLI automatically vendors Python dependencies when it detects a requirements.txt file. It uses Docker to ensure compatibility with the platform's Linux environment:

```bash
# CLI runs this automatically during deploy
docker run --rm -v $(pwd):/app python:3.11 \
  pip install -r requirements.txt -t /app/vendor
```

Pin your dependencies to specific versions for reproducibility:

```txt
ccxt==4.0.0
pandas==2.0.0
numpy==1.24.0
```

### Node.js

For Node.js bots, the CLI runs `npm install` to create a node_modules directory:

```bash
# CLI runs this automatically
npm install --production
```

### Compiled Languages

For Rust, C++, C#, Scala, and Haskell, you must compile your bot before deploying. The CLI uploads the compiled binary along with any runtime dependencies. Ensure your build output matches the entry point in bot-config.yaml.

## Excluding Files

Create a `.the0ignore` file to exclude files from deployment. This works like .gitignore:

```
# .the0ignore
__pycache__/
*.pyc
.env
.git/
tests/
*.log
```

The CLI automatically excludes common files like `.git`, `node_modules` (before vendoring), and `__pycache__`.

## Monitoring Deployed Bots

After creating bot instances, monitor their execution:

```bash
# List running instances
the0 bot list

# View logs
the0 bot logs <bot_id>

# Stream logs in real-time
the0 bot logs <bot_id> -w
```

Logs show your bot's output, including structured log messages and any errors that occur during execution.

## Troubleshooting

**Validation errors**: Check that bot-config.yaml and bot-schema.json are valid YAML/JSON. The CLI reports specific syntax errors.

**Dependency errors**: Ensure all dependencies are listed in requirements.txt or package.json. Missing dependencies cause runtime failures.

**Entry point not found**: Verify that the entry point in bot-config.yaml matches your actual file. For compiled languages, ensure the binary exists at the specified path.

**Version already exists**: You cannot redeploy the same version. Increment the version number in bot-config.yaml and deploy again.

## Related

- [Configuration](./configuration) - bot-config.yaml reference
- [Testing](./testing) - Test before deployment
- [Bot Commands](/the0-cli/bot-commands) - Managing bot instances
