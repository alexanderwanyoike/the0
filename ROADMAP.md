# the0 Roadmap

This roadmap outlines the planned development direction for the0. For live status and detailed issue tracking, see our [GitHub Project](https://github.com/alexanderwanyoike/the0/projects).

> **Note**: This roadmap is subject to change based on community feedback and priorities. Dates are estimates.

## Current Focus: v0.1.0 - Simplification & Removal (Q1 2026)

**Priority**: Reduce complexity, remove unused features, establish solid foundation

### What's Being Removed

**Backtest Runner** (`component/runtime`, `component/api`, `component/frontend`, `component/cli`)
- Reality: All backtesting happens locally - faster iteration and better control
- Action: Remove backtest runner service, API endpoints, UI pages, CLI commands
- Priority: `priority/high`

**Security Enforcement from 0vers33r** (`component/0vers33r`)
- Current: 0vers33r blocks deployment based on security rules
- New Vision: Transform into code analyzer and insight generator
- Rationale: Self-hosted platform doesn't need enforcement - analysis is more valuable
- Priority: `priority/high`

**UI Deployment Forms** (`component/frontend`)
- Reality: CLI + config files is the primary workflow
- Action: Remove deployment forms, keep monitoring dashboard only
- Priority: `priority/medium`

### What's Being Simplified

**Runtime Architecture** (`component/runtime`)
- Issue: Overly complex, tries to support both Docker and Kubernetes simultaneously
- Plan: Separate environment-specific runners (Docker runner, K8s runner)
- Priority: `priority/high`, `difficulty/4-epic`

**AI Agent System** (`component/ai-service`)
- Context: Previous implementation (PR #28) was too complex
- Plan: Salvage and simplify - keep researcher, orchestrator; simplify engineer to bot builder
- Priority: `priority/medium`

### What's Being Added

**Frontend Test Infrastructure** (`component/frontend`, `type/test`)
- Crisis: Frontend is basically untested
- Plan: Setup Jest/React Testing Library, add tests for components/hooks/pages, establish patterns
- Target: >50% coverage baseline
- Priority: `priority/critical`

---

## Phase 1: v0.2.0 - Core Observability (Q1 2026)

**Vision**: Be the DataDog of algorithmic trading

### Real-time Metrics Streaming

- **Kill polling** - implement WebSockets/Server-Sent Events
- Real-time P&L, trade execution logs, position changes
- `component/frontend`, `component/api`, `priority/high`

### Log Aggregation & Parsing

- Parse bot logs automatically
- Extract metrics from log patterns
- Store structured log data for analysis
- `component/runtime`, `priority/high`

### Bot Resource Monitoring

- CPU, memory, disk I/O, network per bot
- Container resource limits vs actual usage
- Alerts when approaching limits
- `component/runtime`, `component/frontend`, `priority/medium`

### Intelligent Error Analysis

- Capture full stack traces
- 0vers33r analyzes errors with bot code context
- AI agent explains errors in plain English
- Link errors to specific code lines
- `component/0vers33r`, `component/ai-service`, `priority/high`

---

## Phase 2: v0.3.0 - Intelligence Layer (Q2 2026)

**Vision**: AI-powered insights and assistance

### 0vers33r Code Reviewer Transformation

- From security enforcer to code analyst
- Understand abstract strategy concepts
- Generate research reports about bots
- Feed insights to AI agent
- `component/0vers33r`, `priority/high`

### AI Agent Integration with Metrics

- Access to all logs, metrics, code
- Proactive notifications (e.g., "Your bot's Sharpe dropped 50%")
- Interactive troubleshooting
- Research report generation
- `component/ai-service`, `priority/medium`

### ML Model Persistence

- Model storage in MinIO
- API endpoints for model upload/download
- Versioning for models
- Integration with bot runtime to load/save automatically
- `component/runtime`, `component/api`, `priority/medium`

---

## Phase 3: v0.4.0 - Polish & Scale (Q3 2026)

**Vision**: Production-ready execution engine

### Kubernetes Deployment

- Helm charts for one-command install
- Native K8s Jobs for scheduled bots
- Native K8s CronJobs for recurring schedules
- Proper pod lifecycle management
- `component/infrastructure`, `priority/high`, `difficulty/4-epic`

### Service Versioning & Release Management

- Semantic versioning for all services
- Version compatibility matrix
- Release notes automation
- Changelog generation
- Tagged Docker images
- `type/infrastructure`, `priority/medium`

### CLI Improvements

- Custom API endpoint configuration
- Environment profiles (prod, staging, local)
- Cleaner output and progress indicators
- Better error messages
- Shell completions (bash, zsh, fish)
- `component/cli`, `priority/medium`

---

## Future / Research (2026+)

**Vision**: Advanced features and ecosystem expansion

### MCP Protocol Integration

- Expose the0 data via Model Context Protocol
- API MCP server (bot management, jobs, config)
- Data service MCP server (logs, metrics aggregation)
- Enable AI agents to query real data
- `component/api`, `type/feature`, `priority/low`

### Frontend Revamp

- Monitoring dashboard only (remove deployment UI)
- WebSocket-powered real-time updates
- Lightweight charts for high-frequency data
- Focus on performance
- `component/frontend`, `priority/low`

### Advanced Features (Community Driven)

- Multi-account support
- Risk management module
- Algorithm marketplace
- Enterprise SSO & RBAC
- Community bot registry

---

## Philosophy & Principles

### What the0 Is

**A production-grade execution engine with world-class observability**

- Deploy and run strategies reliably
- Monitor everything in real-time
- Understand what's happening (AI assistant)
- Persist state (ML models, data)
- Scale infrastructure (Kubernetes)

### What the0 Is Not

- Strategy development platform (that's local)
- Backtesting system (that's local)
- Parameter optimization tool (that's local)

### User Workflow

```
1. Develop locally (Claude Code + backtest scripts)
2. Optimize locally (parameter sweeps)
3. Deploy via CLI (the0 platform)
4. Monitor in real-time (the0 UI)
5. AI assistant helps interpret & troubleshoot
6. Persist models & state automatically
```

---

## Contributing to the Roadmap

We welcome feedback on this roadmap:

- **Discord**: Discuss in `#roadmap` channel
- **GitHub Issues**: Suggest features or changes
- **GitHub Discussions**: Long-form proposals

See [CONTRIBUTING.md](CONTRIBUTING.md) for how to contribute.

---

## Success Metrics

**v0.1.0 (Simplification)**
- Can deploy bot in < 2 minutes via CLI
- Codebase reduced by removing unused features
- Frontend test coverage >50%
- Runtime architecture simplified

**v0.2.0 (Observability)**
- Real-time P&L without page refresh
- See bot resource usage per container
- Error analysis with AI explanation < 30 seconds
- Never use polling for metrics

**v0.3.0 (Intelligence)**
- ML bots persist state automatically
- 0vers33r generates strategy research reports
- AI agent proactively alerts on issues
- Code insights enhance development

**v0.4.0 (Production)**
- Kubernetes deployment via Helm
- All services versioned and tracked
- CLI supports multiple environments
- Production-hardened security

---

**Last Updated**: November 26, 2025
**Internal Planning Doc**: See `milestones/2025-11-26-goals.md` for detailed internal notes
