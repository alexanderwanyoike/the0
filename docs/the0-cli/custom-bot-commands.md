---
title: "Custom Bot Commands"
description: "Deploy and manage your custom trading algorithms"
tags: ["cli", "custom-bots", "development"]
order: 4
---

# Custom Bot Commands

The `custom-bot` commands deploy and manage custom bot definitions on the0 platform.

## Deploy

Deploy a custom bot from the current directory:

```bash
the0 custom-bot deploy
```

The CLI reads `bot-config.yaml` and performs the following steps:

1. Validates configuration and semantic version
2. Vendors dependencies (Python: pip install to `vendor/`, Node.js: npm install)
3. Compiles code for compiled languages (Rust, C#, Scala, C++, Haskell)
4. Builds frontend if `hasFrontend: true` is set
5. Packages files into a deployable archive (respecting `.the0ignore`)
6. Uploads to the platform

**Required project structure:**

```
my-bot/
├── bot-config.yaml      # Bot metadata and configuration
├── main.py              # Entry point (or main.ts, main.rs, etc.)
├── bot-schema.json      # Configuration parameter schema
├── requirements.txt     # Dependencies (Python)
└── README.md            # Documentation
```

**Example bot-config.yaml:**

```yaml
name: sma-crossover
description: "SMA crossover strategy bot"
version: 1.0.0
author: "your-name"
type: realtime
runtime: python3.11

entrypoints:
  bot: main.py

schema:
  bot: bot-schema.json

readme: README.md

metadata:
  categories: [trading, technical-analysis]
  tags: [sma, crossover]
```

Each deployment requires a unique version. Increment the `version` field before deploying updates.

## List

Display all your deployed custom bots:

```bash
the0 custom-bot list
```

Output shows bot name, latest version, creation date, and type.

## Versions

List all versions of a specific custom bot:

```bash
the0 custom-bot versions <name>
```

**Example:**

```bash
the0 custom-bot versions sma-crossover
```

## Schema

Retrieve the JSON Schema for a custom bot's configuration parameters:

```bash
the0 custom-bot schema <version> <name>
```

Use this to understand what parameters are required when deploying bot instances.

**Example:**

```bash
the0 custom-bot schema 1.0.0 sma-crossover
```

## The .the0ignore File

Exclude files from deployment by creating a `.the0ignore` file. The syntax is similar to `.gitignore`:

```
# Exclude test files
tests/
*_test.py

# Exclude local configuration
*.local.yaml
.env

# Exclude build artifacts
__pycache__/
*.pyc
```

The following directories are always included regardless of ignore patterns:

- `vendor/` - Python dependencies
- `node_modules/` - Node.js dependencies

Default exclusions when no `.the0ignore` exists:

- `*.log`, `*.tmp`, `*.temp`
- `test/`, `tests/`, `__tests__/`
- `.git/`, `build/`, `dist/`
- `__pycache__/`, `*.pyc`, `*.pyo`
- `.DS_Store`, `Thumbs.db`

## Related

- [Bot Commands](./bot-commands) - Deploy bot instances from custom bots
- [Secrets](./secrets) - Configure private dependency access
- [Custom Bots](/terminology/custom-bots) - Understanding custom bot concepts
