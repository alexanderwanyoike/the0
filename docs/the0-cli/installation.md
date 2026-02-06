---
title: "Installation"
description: "How to install the the0 CLI"
tags: ["cli", "installation"]
order: 1
---

# Installation

The `the0` CLI is a Go binary that can be installed from source or downloaded as a pre-built release.

## Prerequisites

To deploy bots that use Python or Node.js dependencies, you need Docker installed and running. The CLI uses Docker to vendor dependencies in a consistent Linux environment.

## Install from Source

Building from source requires Go 1.21 or later.

```bash
# Clone the repository
git clone https://github.com/alexanderwanyoike/the0.git
cd the0/cli

# Build and install to ~/bin
make install
```

This compiles the CLI and copies it to `~/bin/the0`. Ensure `~/bin` is in your PATH:

```bash
export PATH="$HOME/bin:$PATH"
```

Add this line to your shell configuration file (`~/.bashrc`, `~/.zshrc`, etc.) to make it permanent.

## Verify Installation

Confirm the CLI is installed correctly:

```bash
the0 --version
the0 --help
```

The help command displays all available commands and global flags.

## Configuration

The CLI stores configuration in `~/.the0/`:

- `auth.json` - API authentication credentials

No manual configuration is required. The CLI creates this directory automatically when you first authenticate.

## Next Steps

- [Authentication](./authentication) - Configure your API credentials
- [Custom Bot Commands](./custom-bot-commands) - Deploy your first bot
