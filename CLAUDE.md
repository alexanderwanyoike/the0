# the0 - Algorithmic Trading Platform

## Project Overview

**the0** is an open-source algorithmic trading platform that enables developers and traders to create, test, deploy, and manage trading bots across multiple markets. The platform provides a complete microservices architecture with support for Python and JavaScript/Node.js bot development.

**Status**: Beta - Active development, not production-ready
**License**: Apache 2.0
**Architecture**: Microservices with event-driven communication

## High-Level Architecture

### Services Overview

1. **Frontend** (`/frontend`) - Next.js 15 + React 19 web application
2. **API** (`/api`) - NestJS REST API and orchestration layer
3. **Runtime Services** (`/runtime`) - Go microservices for bot execution:
   - Bot Runner - Real-time bot execution
   - Backtest Runner - Historical strategy testing
   - Bot Scheduler - Cron-based scheduled execution
4. **CLI** (`/cli`) - Go command-line tool for local development
5. **Documentation** (`/docs`) - Next.js documentation site
6. **Site** (`/site`) - Marketing/landing page

### Data Architecture

- **PostgreSQL**: User accounts, bot definitions, authentication (Drizzle ORM)
- **MongoDB**: Runtime state, job queues, execution logs
- **MinIO**: S3-compatible storage for bot code, logs, backtest results
- **NATS JetStream**: Event streaming and service coordination
- **Redis** (optional): Caching layer

## Technology Stack

### Frontend Stack
- **Framework**: Next.js 15 (App Router), React 19
- **Language**: TypeScript 5
- **Styling**: Tailwind CSS with custom design system
- **UI Components**: shadcn/ui + Radix UI
- **State Management**: Zustand
- **Authentication**: Firebase Auth (may be replaced)
- **Testing**: Jest + React Testing Library

### Backend Stack
- **API**: NestJS (Node.js 20), TypeScript
- **Runtime Services**: Go 1.21+
- **Databases**: PostgreSQL 15, MongoDB 7
- **Message Broker**: NATS 2.10 with JetStream
- **Storage**: MinIO (S3-compatible)
- **ORM**: Drizzle (PostgreSQL), native drivers (MongoDB)

### Infrastructure
- **Containerization**: Docker + Docker Compose
- **Orchestration**: Kubernetes (experimental)
- **Deployment**: Local (Docker), Minikube (experimental)

## Project Structure

```
the0/
├── frontend/           # Next.js web application (main UI)
├── api/               # NestJS API server
├── runtime/           # Go microservices (bot-runner, backtest-runner, scheduler)
├── cli/               # Go CLI tool
├── docs/             # Documentation site (Next.js)
├── site/             # Landing page (Next.js)
├── k8s/              # Kubernetes manifests (experimental)
├── database/         # Database initialization scripts
└── notes/            # Development notes and feedback
```

## Development Workflow

### Prerequisites

- **Docker** 20.10+ and **Docker Compose** 2.0+
- **Node.js** 20+ and **yarn** (primary package manager)
- **Go** 1.21+ (for CLI and runtime services)
- At least 4GB RAM for containers

### Quick Start

```bash
# Install the CLI
curl -sSL https://install.the0.app | sh

# Start all services
the0 local start

# Access services:
# - Frontend: http://localhost:3001
# - API: http://localhost:3000
# - MinIO Console: http://localhost:9001 (admin/the0password)

# Stop services
the0 local stop
```

### Building Individual Services

```bash
# Frontend
cd frontend
yarn install
yarn dev              # Development server
yarn build            # Production build
yarn test             # Run tests

# API
cd api
yarn install
yarn start:dev        # Development server
yarn build            # Production build
yarn test             # Run tests
yarn db:migrate       # Run database migrations

# CLI
cd cli
make install          # Build and install to ~/bin/the0
the0 --help          # Verify installation

# Runtime services
cd runtime
go build -o bin/bot-runner ./cmd/bot-runner
go build -o bin/backtest-runner ./cmd/backtest-runner
go build -o bin/scheduler ./cmd/scheduler
```

### Testing Strategy

**Frontend**:
- Unit tests: Jest + React Testing Library
- Integration tests: MSW for API mocking
- E2E tests: Playwright (future)

**API**:
- Unit tests: Jest with NestJS testing utilities
- Integration tests: Supertest with test database
- E2E tests: Full Docker environment

**Runtime Services**:
- Unit tests: Go testing package
- Integration tests: Docker test containers
- Performance tests: Benchmarking suite

## Code Organization Principles

### Frontend
- **Feature-based organization**: Components grouped by business domain
- **Colocation**: Related files kept together (components, hooks, types)
- **Barrel exports**: Clean imports with index files
- **File naming**: kebab-case for files, PascalCase for components

### Backend (API)
- **Module-based architecture**: NestJS modules for feature separation
- **Service layer**: Business logic separated from controllers
- **Repository pattern**: Data access abstraction
- **DTOs**: Strong typing for API contracts

### Runtime Services (Go)
- **Package structure**: cmd/ for binaries, internal/ for implementation
- **Worker patterns**: Master-worker for concurrent execution
- **gRPC communication**: Inter-service communication
- **Error handling**: Explicit error returns and wrapping

## Common Development Tasks

### Adding a New Frontend Feature

1. Create feature directory in `frontend/src/components/[feature]`
2. Add page route in `frontend/src/app/[feature]`
3. Create custom hooks in `frontend/src/hooks/[feature]`
4. Add type definitions in `frontend/src/types/[feature].ts`
5. Update navigation/routing as needed

### Adding a New API Endpoint

1. Create or update module in `api/src/[module]`
2. Add controller endpoint with proper decorators
3. Implement service layer logic
4. Add DTOs for request/response validation
5. Update OpenAPI/Swagger documentation
6. Write tests (unit + integration)

### Modifying Bot Execution Flow

1. Update runtime service in `runtime/internal/[service]`
2. Modify NATS event handlers if needed
3. Update API orchestration layer
4. Test with Docker Compose environment
5. Update documentation

## Key Files and Locations

### Configuration Files
- **API Config**: `/api/src/config/` (environment-based)
- **Frontend Config**: `/frontend/.env.local` (not in repo)
- **Runtime Config**: `/runtime/.env` (not in repo)
- **Database Schema**: `/api/src/database/schema.ts` (Drizzle)

### Documentation
- **Main README**: `/README.md`
- **Kubernetes**: `/k8s/README.md`
- **Bot Development**: `/docs/src/content/docs/custom-bot-development/`
- **Frontend Guide**: `/frontend/CLAUDE.md`

### Entry Points
- **Frontend**: `/frontend/src/app/layout.tsx` (root layout)
- **API**: `/api/src/main.ts`
- **CLI**: `/cli/cmd/root.go`
- **Bot Runner**: `/runtime/cmd/bot-runner/main.go`

## Environment Variables

### Frontend
```bash
NEXT_PUBLIC_API_URL=http://localhost:3000
```

### API
```bash
DATABASE_URL=postgresql://the0:password@localhost:5432/the0_oss
NATS_URLS=nats://localhost:4222
MINIO_ENDPOINT=localhost:9000
MINIO_ACCESS_KEY=the0admin
MINIO_SECRET_KEY=the0password
JWT_SECRET=your-secret-key
```

### Runtime Services
```bash
NATS_URL=nats://localhost:4222
MONGO_URL=mongodb://root:password@localhost:27017
MINIO_ENDPOINT=localhost:9000
```

## Bot Development

### Supported Languages
- Python 3.11+ (any PyPI packages)
- JavaScript/Node.js 20+ (any npm packages)

### Bot Types
- **Scheduled Bots**: Run on cron schedules (daily, weekly, monthly)
- **Real-time Bots**: Continuous execution with live data feeds

### Bot Structure
```python
# Python bot example
from typing import Dict, Any

def main(id: str, config: Dict[str, Any]) -> Dict[str, Any]:
    """Bot entry point"""
    # Your trading logic here
    return {
        "status": "success",
        "message": "Trade executed"
    }
```

### Bot Configuration
- YAML configuration files
- JSON Schema validation
- Environment-specific settings
- Secret management for API keys

## Security Considerations

- **Code Analysis**: Automatic YARA rule scanning for uploaded bots
- **Sandboxing**: Isolated execution environments for each bot
- **API Keys**: Secure storage in database/environment
- **Authentication**: JWT tokens with refresh mechanism
- **Rate Limiting**: API endpoint protection
- **Input Validation**: Schema validation on all inputs

## Common Issues and Solutions

### Docker Issues
```bash
# Services not starting
the0 local logs [service-name]
the0 local stop && the0 local start

# Port conflicts
# Use the0 local config to adjust port mappings

# Volume permission issues
sudo chown -R $USER:$USER ./volumes
```

### Database Issues
```bash
# Reset database
cd api
yarn db:push  # Push schema changes
yarn db:seed  # Seed initial data

# Check PostgreSQL connection
docker exec -it the0-postgres psql -U the0 -d the0_oss
```

### Frontend Build Issues
```bash
# Clear Next.js cache
rm -rf .next
yarn build

# Node modules issues
rm -rf node_modules yarn.lock
yarn install
```

## Testing Commands

```bash
# Frontend tests
cd frontend
yarn test              # Run all tests
yarn test:watch        # Watch mode

# API tests
cd api
yarn test              # Unit tests
yarn test:e2e         # E2E tests
yarn test:cov         # Coverage report

# Runtime tests
cd runtime
go test ./...          # All tests
go test -v ./internal/bot-runner  # Specific package
```

## Deployment

### CLI Local (Recommended for Development)
```bash
the0 local start
```

### Kubernetes (Experimental)
```bash
cd k8s
make minikube-up
make setup-hosts
```

## Performance Optimization

### Frontend
- Code splitting via Next.js
- Image optimization with Next/Image
- React.memo for expensive components
- Lazy loading for routes

### Backend
- Database indexing on common queries
- Redis caching for frequently accessed data
- Connection pooling for databases
- Batch operations where possible

### Runtime
- Worker pools for concurrent bot execution
- Resource limits per bot
- Efficient event streaming with NATS
- Cleanup of completed jobs

## Debugging Tips

### Frontend Debugging
```bash
# Enable verbose logging
DEBUG=* yarn dev

# React DevTools
# Install browser extension

# Network inspection
# Use browser DevTools Network tab
```

### API Debugging
```bash
# NestJS debug mode
yarn start:debug

# View logs
docker compose logs -f the0-api

# Database queries
# Enable Drizzle query logging in config
```

### Runtime Debugging
```bash
# Go debugging with delve
dlv debug ./cmd/bot-runner

# View NATS messages
nats sub ">"

# MongoDB queries
docker exec -it the0-mongo mongosh
```

## Contributing Guidelines

1. **Branch Strategy**: Feature branches from `main`
2. **Commit Messages**: Conventional commits format
3. **Testing**: Write tests for new features
4. **Documentation**: Update docs with changes
5. **Code Review**: PR required before merge
6. **AI-Assisted Development**: Welcome! Ensure code is tested

## Resources

- **Discord**: https://discord.gg/g5mp57nK
- **Documentation**: https://docs.the0.app (when deployed)
- **GitHub Issues**: Report bugs and request features
- **Feedback**: `/notes/feedback.md`

## Future Roadmap Considerations (Based on Feedback)

The feedback in `/notes/feedback.md` suggests focusing on:

1. **User-Friendliness**: More beginner-friendly setup and AI assistance
2. **Research Infrastructure**: Enhanced backtesting, algorithm indexing, research pipelines
3. **Broker Integrations**: Easy drag-and-drop integrations with trading platforms
4. **Automation**: n8n-style drag-and-drop workflow automation
5. **Monetization**: Freemium model with premium features (risk management agents, orchestrator agents)
6. **Enterprise Features**: Risk management modules, research automation
7. **Completeness**: Balance dev, research, and execution pillars

### Three Pillars of Quant Model
- **Dev**: Bot development and deployment (currently strong)
- **Research**: Algorithm research and backtesting (needs enhancement)
- **Exec**: Trade execution and broker integration (needs enhancement)

## Build Verification

Always check the build before committing:

```bash
# Frontend
cd frontend && yarn build

# API
cd api && yarn build

# CLI
cd cli && make build

# Runtime
cd runtime && go build ./...
```

## Notes

- This is a large, complex system - don't try to analyze everything at once
- Focus on specific features or services when making changes
- Use `the0 local start` for development - it handles all service dependencies
- Check `/notes/feedback.md` for community input and feature priorities
- The project uses yarn as the primary package manager
- Always verify builds pass before submitting changes
