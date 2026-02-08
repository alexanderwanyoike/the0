# the0 - Project Context

## Multi-Agent Environment & Protocol

**IMPORTANT:** This repository is a multi-agent environment. Agents such as **Claude Code** and **Codex** may also be working on this codebase.

### Session Protocol
1.  **Context Update:** At the beginning of every session, before implementing any feature, **you must update this file (`GEMINI.md`)** with your current interpretation of the codebase.
2.  **Source of Truth:** While `AGENTS.md` and `CLAUDE.md` (if available) can be referred to for architectural guidance, the **codebase itself is the ultimate source of truth**. Always verify assumptions by inspecting the code.
3.  **Coordination:** Be mindful of changes potentially made by other agents.

## Project Overview

**the0** is an open-source algorithmic trading execution engine designed for deploying and managing trading bots across multiple markets. It features a microservices architecture that allows users to build strategies in various languages (Python, TypeScript, Rust, C++, C#, Scala, Haskell) and deploy them to a self-hosted environment.

### Key Components

*   **Frontend:** `frontend/` - Next.js 15 + React 19 dashboard for bot management and monitoring.
*   **API:** `api/` - NestJS backend providing REST APIs, authentication, and orchestration.
*   **CLI:** `cli/` - Go-based command-line tool for local bot development and management.
*   **Runtime:** `runtime/` - Go microservices (Bot Runner, Scheduler) for executing strategies.
*   **Infrastructure:** Local environment managed via `the0 local` CLI command (Postgres, Mongo, NATS, MinIO).
*   **SDKs:** `sdk/` - Client libraries for supported languages.

## Architecture

*   **Database:** PostgreSQL (User/Auth), MongoDB (Runtime state/logs).
*   **Storage:** MinIO (S3-compatible) for bot code and logs.
*   **Messaging:** NATS JetStream for event streaming.
*   **Execution:** Isolated bot execution environments via Docker/Containerization principles.

## Development Setup & Workflow

### Prerequisites
*   Docker 20.10+ & Docker Compose 2.0+
*   Node.js 20+ & Yarn
*   Go 1.21+
*   Python 3.11+ (for AI services)

### Quick Start (Full Platform)
To start the entire platform locally using Docker Compose:

```bash
the0 local up
```
*   **Frontend:** http://localhost:3001
*   **API:** http://localhost:3000
*   **MinIO Console:** http://localhost:9001 (Credentials: `admin`/`the0password`)

### Component Development

#### 1. CLI (`cli/`)
The CLI is essential for local bot development.

*   **Install:** `make install` (Installs to `~/bin/the0`)
*   **Build:** `make build`
*   **Test:** `make test`
*   **Lint:** `make lint`

#### 2. Frontend (`frontend/`)
*   **Install Dependencies:** `yarn install`
*   **Run Dev Server:** `yarn dev` (Starts on http://localhost:3001)
*   **Test:** `yarn test` or `yarn test:watch`
*   **Lint:** `yarn lint`
*   **Stack:** Next.js 15, React 19, Tailwind CSS, Zustand, Radix UI.

#### 3. API (`api/`)
*   **Install Dependencies:** `yarn install`
*   **Run Dev Server:** `yarn start:dev` (Starts on http://localhost:3000)
*   **Test:** `yarn test` (Unit) or `yarn test:e2e` (E2E)
*   **Database Migrations:** `yarn db:migrate`
*   **Stack:** NestJS, Drizzle ORM, Passport, Postgres.

#### 4. Runtime (`runtime/`)
*   **Build:** `make build`
*   **Test:** `make test`
*   **Docker Build:** `make docker-build`

## Testing Guidelines

*   **Frontend:** Uses Jest + React Testing Library. Mock API calls with MSW.
*   **API:** Uses Jest. E2E tests are located in `test/`.
*   **Go Services:** Use standard `go test ./...`.

## Conventions

*   **Commits:** Follow conventional commits (`feat`, `fix`, `docs`, `refactor`, `test`, `chore`).
*   **Code Style:**
    *   **TS/JS:** Prettier + ESLint.
    *   **Go:** `gofmt` + `go vet`.
    *   **Python:** Black + PEP 8.
*   **Branching:** Create feature branches from `main`.

## AI Integration

*   **MCP Server:** The platform includes a Model Context Protocol (MCP) server for integration with AI assistants (Claude Code, Gemini).
*   **AI-Assisted Code:** Accepted but must be reviewed and tested. Context engineering is preferred over "vibe coding".