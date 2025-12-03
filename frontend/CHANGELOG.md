# Changelog

All notable changes to the frontend service will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Removed

- AI Agent Workbench feature and all related code

  - AI Agent page (`/ai-agent`) and session pages
  - AI Agent API proxy routes (`/api/ai-agent/*`)
  - AI Agent components (chat, settings, artifacts, setup, layout)
  - AI Agent hooks (useChat, useChatSessions, useStreamingChat, useArtifacts, etc.)
  - AI Agent stores (chatStore, artifactsStore, settingsStore, themeStore)
  - AI Agent API service (`/lib/ai-agent/api.ts`)
  - AI Agent types (`/types/ai-agent.ts`)
  - Artifact ZIP generator utility (`/lib/zipGenerator.ts`)
  - AI Agent navigation item from dashboard sidebar
  - `aiAgentUrl` configuration from config.ts

- Deploy page (`/deploy/[name]/[version]`) - Bot deployment now exclusively via CLI
- Deploy form components (`form-generator`, `bot-update-confirmation-dialog`, `bot-documentation`)
- "Deploy Bot" button from custom bot detail page
- "Update Config" button from running bot detail page (replaced with CLI instructions)

### Added

- Schema display component on custom bot detail page showing full JSON schema
- "Deploy via CLI" modal with deployment commands, example config, and helpful CLI commands
- "Update via CLI" modal on running bot detail page with update commands and bot ID copy functionality

### Changed

- Custom bot detail page now emphasizes CLI-first deployment workflow
- Running bot detail page uses CLI modal instead of navigating to deploy form
- Simplified action-buttons component by removing unused marketplace approval logic

- Backtest pages (`/backtests`, `/backtests/create`, `/backtests/[id]`)
- Backtest components (`backtest-creation-form`, `bot-selection-form`, `backtest-results`, `metrics-summary`, `plots-display`, `tables-display`)
- Backtest hooks (`use-backtests`, `use-backtest`, `use-backtest-creation`, `use-combined-bot-search`, `use-backtest-sse`)
- Backtest API routes (`/api/backtests`)
- Backtest types and utilities (`Backtest` type, `canBotBeBacktested` function)
- "Backtests" navigation item from dashboard sidebar
- "Backtest" button from custom bot action buttons
- "Custom Backtesting" feature from landing page
- "Test" step from "How It Works" section on landing page

### Changed

- Landing page hero section now displays 3 key features instead of 4
- "How It Works" section now shows 3 steps (Develop → Deploy → Monitor)
