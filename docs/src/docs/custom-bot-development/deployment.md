---
title: "Deployment"
description: "Deploy and manage custom bots on the0 platform"
tags: ["custom-bots", "deployment", "production"]
order: 6
---

# Deploying Custom Bots

This guide covers the complete deployment process for custom bots, from local development to production deployment on the the0 platform.

## Deployment Overview

### Deployment Flow

```
Local Development → Validation → Package → Upload → Review → Publish
```

1. **Local Development**: Build and test your bot locally
2. **Validation**: Ensure all requirements are met
3. **Package**: Bundle bot files and dependencies
4. **Upload**: Deploy to the0 platform
5. **Review**: Platform validation and security checks
6. **Publish**: Make available for deployment

## Pre-Deployment Checklist

### Required Files

Ensure your bot directory contains all required files:

```bash
my-bot/
├── bot-config.yaml          ✓ Required
├── main.py                  ✓ Required (or main.js)
├── bot-schema.json          ✓ Required
├── README.md                ✓ Required
├── requirements.txt         ✓ Optional for Python
├── package.json             ✓ Optional for JavaScript
└── vendor/                  ✓ Auto-generated
```

### Validation Checklist

- [ ] All required files present
- [ ] Valid YAML configuration
- [ ] Valid JSON schemas
- [ ] Entry points exist and are executable
- [ ] Dependencies properly specified
- [ ] README documentation complete
- [ ] Local testing successful

## Deployment Process

```bash
# Deploy your custom bot
the0 custom-bot deploy
```

### Step 3: Verify Deployment

```bash
# List your deployed custom bots
the0 custom-bot list

# Check specific bot details
the0 custom-bot versions momentum-trader

# Get schema to verify deployment
the0 custom-bot schema bot 1.0.0 momentum-trader
```

## Creating Bot Instances

Once your custom bot is deployed, create instances to run it:

### Configuration File

Create a configuration file for your bot instance:

```json
{
  "name": "my-momentum-bot",
  "type": "custom/momentum-trader",
  "version": "1.0.0",
  "schedule": "0 9 * * *",
  "parameters": {
    "symbol": "BTCUSDT",
    "position_size": 100,
    "rsi_period": 14,
    "rsi_oversold": 30,
    "rsi_overbought": 70,
    "api_key": "your-api-key",
    "api_secret": "your-api-secret",
    "dry_run": true
  }
}
```

### Deploy Bot Instance

```bash
# Deploy bot instance
the0 bot deploy momentum-config.json

# Verify deployment
the0 bot list

# Monitor bot logs
the0 bot logs my-momentum-bot
```

## Dependency Management

### Python Dependencies

#### Automatic Vendoring

The CLI automatically vendors Python dependencies using Docker:

```bash
# When you run deployment
the0 custom-bot deploy

# CLI detects requirements.txt and vendors dependencies
🐳 Docker detected, vendoring Python dependencies...
```

#### Requirements.txt Best Practices

```txt
# Pin exact versions for reproducibility
ccxt==3.0.91
pandas==1.5.3
numpy==1.24.3
plotly==5.14.1

# Use compatible release operators for flexibility
scikit-learn~=1.2.0

# Avoid system packages
# Don't include: pip, setuptools, wheel

# Group related dependencies
# Trading libraries
ccxt>=3.0.0
ta-lib>=0.4.25

# Data processing
pandas>=1.5.0
numpy>=1.24.0

# Visualization
plotly>=5.12.0
matplotlib>=3.6.0

# Utilities
python-dotenv>=0.19.0
requests>=2.28.0
```

### JavaScript Dependencies

#### Package.json Configuration

```json
{
  "name": "momentum-trader",
  "version": "1.0.0",
  "description": "Momentum trading bot",
  "main": "main.js",
  "engines": {
    "node": ">=20.0.0"
  },
  "dependencies": {
    "ccxt": "^3.0.91",
    "axios": "^1.4.0",
    "ws": "^8.13.0",
    "mathjs": "^11.9.0",
    "plotly.js-dist": "^2.24.1"
  },
  "devDependencies": {
    "mocha": "^10.2.0",
    "chai": "^4.3.7",
    "sinon": "^15.1.0"
  },
  "scripts": {
    "test": "mocha",
    "start": "node main.js"
  }
}
```

## Version Management

### Semantic Versioning

Follow [semantic versioning](https://semver.org/) for your bot versions:

- **MAJOR**: Incompatible API changes
- **MINOR**: Backwards-compatible functionality additions
- **PATCH**: Backwards-compatible bug fixes

```yaml
# bot-config.yaml
version: 1.2.3
#        │ │ └─ Patch: Bug fixes
#        │ └─── Minor: New features
#        └───── Major: Breaking changes
```

### Version Deployment

```bash
# Update version in bot-config.yaml
version: 1.1.0

# Deploy new version
the0 custom-bot deploy

# Users can now deploy the new version
the0 bot deploy config-with-v1.1.0.json
```

### Managing Multiple Versions

```bash
# List all versions
the0 custom-bot versions momentum-trader

# Deploy specific version
{
  "type": "custom/momentum-trader",
  "version": "1.0.0"  // Specific version
}

# Use latest version (default)
{
  "type": "custom/momentum-trader"
  // No version specified = latest
}
```

> Note you can NOT update a version once it has been deployed. If you need to update a version you will need to create a new version with a new version number.

## Platform Validation Process

### Automated Checks

When you deploy, the platform performs automated validation:

1. **File Structure**: Validates all required files are present
2. **Configuration**: Validates YAML and JSON syntax
3. **Schema Compliance**: Ensures schemas are valid JSON Schema
4. **Code Analysis**: Basic static analysis for common issues
5. **Security Scanning**: Checks for security vulnerabilities
6. **Dependency Analysis**: Validates dependencies and licenses

### Security Review

For marketplace publication, additional security review includes:

- Advanced security scanning
- Compliance checking

## Monitoring and Maintenance

### Post-Deployment Monitoring

```bash
# Monitor bot instances
the0 bot list

# Check bot logs
the0 bot logs <bot-id>

# Example: Monitor specific bot logs (use -w t)
the0 bot logs f0ef680b-a70b-4f2d-afac-98a074e8b844
⚡ Accessing bot logs from the grid: f0ef680b-a70b-4f2d-afac-98a074e8b844
[20250708] INFO:CustomBot:Executing bot
INFO:root:Generating dummy cryptocurrency data...
INFO:root:Generated 30 days of dummy data
INFO:root:=== Trading Analysis Update ===
INFO:root:Current Bitcoin Price: $32603.81
INFO:root:Predicted Next Price: $31150.02 (Confidence: 0.96)
INFO:root:Moving Average Signal: SELL
INFO:root:Price Momentum: DOWN
INFO:root:Volatility: 0.7195
INFO:root:Recent Anomalies: 0
INFO:root:Price Change: -1.35%
INFO:root:7-day MA: $34144.21
INFO:root:21-day MA: $40343.34
INFO:CustomBot:Bot execution completed

```

### Updating Deployed Bots

```bash
# Update bot configuration
edit bot-config.yaml

# Increment version
version: 1.0.1

# Deploy update
the0 custom-bot deploy

# Update existing instances
the0 bot update my-bot-id updated-config.json
```

## Next Steps

After successful deployment:

1. **Test Thoroughly**: Use paper trading to validate behavior
2. **Monitor Performance**: Set up comprehensive monitoring
3. **Gather Feedback**: Collect user feedback and iterate
4. **Optimize**: Continuously improve performance and features
5. **Scale**: Consider publishing to marketplace

For marketplace publication, see [Publishing a Custom Bot](/docs/custom-bot-marketplace/publishing-a-custom-bot).
