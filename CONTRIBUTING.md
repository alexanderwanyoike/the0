# Contributing to the0

Thank you for your interest in contributing to the0! This document provides guidelines and instructions for contributing to this project.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [Types of Contributions](#types-of-contributions)
- [Development Workflow](#development-workflow)
- [Reporting Bugs](#reporting-bugs)
- [Suggesting Enhancements](#suggesting-enhancements)
- [Code Contributions](#code-contributions)
- [Coding Conventions](#coding-conventions)
- [Testing Requirements](#testing-requirements)
- [Documentation Standards](#documentation-standards)
- [Pull Request Process](#pull-request-process)
- [Communication](#communication)
- [Recognition](#recognition)
- [License](#license)

## Code of Conduct

This project adheres to the [Contributor Covenant Code of Conduct](CODE_OF_CONDUCT.md). By participating, you are expected to uphold this code. Please report unacceptable behavior to the project maintainers.

## Getting Started

### Prerequisites

- **Docker** 20.10+ with **Compose** plugin (the CLI uses Docker Compose under the hood)
- **Node.js** 20+ and **yarn** (for frontend and API development)
- **Go** 1.21+ (for CLI and runtime services)
- **Python** 3.11+ (for AI services)
- **Git** for version control
- At least 4GB RAM available for containers

### Development Environment Setup

1. **Fork and clone the repository:**
   ```bash
   git fork https://github.com/alexanderwanyoike/the0.git
   cd the0
   ```

2. **Start all services:**
   ```bash
   the0 local up
   ```

3. **Access services:**
   - Frontend: http://localhost:3001
   - API: http://localhost:3000
   - MinIO Console: http://localhost:9001 (admin/the0password)

4. **Install development tools:**
   ```bash
   # Install CLI
   cd cli
   make install

   # Install frontend dependencies
   cd ../frontend
   yarn install

   # Install API dependencies
   cd ../api
   yarn install
   ```

## Types of Contributions

We welcome various types of contributions:

- **Bug Reports** - Help us identify and fix issues
- **Feature Requests** - Suggest new capabilities or improvements
- **Code Contributions** - Fix bugs, implement features, improve performance
- **Documentation** - Improve guides, add examples, fix typos
- **Testing** - Add test coverage, improve test infrastructure
- **Bot Templates** - Share trading strategies and patterns
- **Infrastructure** - Improve deployment, CI/CD, development tools

## Development Workflow

the0 uses a standard Git workflow:

1. **Create a feature branch** from `main`:
   ```bash
   git checkout -b feature/your-feature-name
   # or
   git checkout -b fix/bug-description
   ```

2. **Make your changes** in your branch

3. **Commit your changes** with clear commit messages:
   ```bash
   git add .
   git commit -m "type: brief description"
   ```

   Commit types: `feat`, `fix`, `docs`, `refactor`, `test`, `chore`

4. **Push to your fork:**
   ```bash
   git push origin feature/your-feature-name
   ```

5. **Create a Pull Request** on GitHub

## Reporting Bugs

Before creating a bug report:

1. **Check existing issues** to avoid duplicates
2. **Test on the latest version** to ensure the bug still exists
3. **Gather information** about your environment

Use the [Bug Report Template](.github/ISSUE_TEMPLATE/bug_report.yml) and include:

- Clear, descriptive title
- Steps to reproduce the issue
- Expected vs actual behavior
- Environment details (OS, Docker version, etc.)
- Relevant logs or error messages
- Screenshots if applicable

## Suggesting Enhancements

Use the [Feature Request Template](.github/ISSUE_TEMPLATE/feature_request.yml) and include:

- Clear description of the proposed feature
- Use case and problem it solves
- Potential implementation approach (optional)
- Alternatives you've considered

## Code Contributions

### Component-Specific Guidelines

**Frontend (Next.js + React):**
- Located in `/frontend`
- Uses TypeScript, React 19, Next.js 15
- State management: Zustand
- UI components: shadcn/ui + Radix UI
- Styling: Tailwind CSS
- Testing: Jest + React Testing Library

**API (NestJS):**
- Located in `/api`
- Uses TypeScript, NestJS
- Database: PostgreSQL (Drizzle ORM)
- Testing: Jest with NestJS testing utilities

**Runtime Services (Go):**
- Located in `/runtime`
- Uses Go 1.21+
- Services: bot-runner, scheduler
- Testing: Go testing package

**CLI (Go):**
- Located in `/cli`
- Uses Go + Cobra framework
- Install with `make install`

### AI-Assisted Development

We welcome AI-assisted development:

- **Tools Accepted**: Claude, ChatGPT, GitHub Copilot, Cursor, etc.
- **Quality Requirement**: All AI-generated code must be reviewed and tested
- **Context Engineering**: Prefer context engineering over "vibe coding"
- **Transparency**: You may mention AI tools used, but it's optional
- **Responsibility**: You are responsible for code quality regardless of how it was created

## Coding Conventions

### General

- Follow existing code style in each service
- Use meaningful variable and function names
- Keep functions small and focused
- Avoid over-engineering - simple solutions preferred

### TypeScript/JavaScript

- Use TypeScript for type safety
- Follow ESLint configuration
- Run `yarn lint` before committing
- Use functional components and hooks in React

### Go

- Follow standard Go formatting (`gofmt`)
- Run `go vet` before committing
- Use meaningful package names
- Document exported functions

### Python

- Follow PEP 8 style guide
- Use type hints where applicable
- Run `black` for formatting
- Use virtual environments

## Testing Requirements

**All code contributions must include tests.**

### Frontend Testing

```bash
cd frontend
yarn test              # Run tests
yarn test:watch        # Watch mode
```

- Unit tests for components, hooks, utilities
- Integration tests for page flows
- Use React Testing Library
- Mock API calls with MSW

### API Testing

```bash
cd api
yarn test              # Unit tests
yarn test:e2e         # End-to-end tests
```

- Unit tests for services and controllers
- Integration tests for database operations
- E2E tests for critical workflows

### Runtime/CLI Testing

```bash
cd runtime
go test ./...

cd cli
go test ./...
```

- Unit tests for core logic
- Integration tests with Docker test containers

## Documentation Standards

- Update README.md if changing user-facing features
- Document new configuration options
- Add JSDoc/GoDoc comments for public APIs
- Update CHANGELOG.md for significant changes
- Include examples for new features

## Pull Request Process

### Before Submitting

- [ ] Code follows project conventions
- [ ] Tests added and passing
- [ ] Documentation updated
- [ ] Commit messages are clear
- [ ] Branch is up to date with main
- [ ] No merge conflicts

### Submitting

1. **Fill out the PR template** completely
2. **Link related issues** using keywords (Fixes #123, Closes #456)
3. **Request review** from maintainers
4. **Respond to feedback** promptly
5. **Keep PR focused** - one feature/fix per PR

### Review Process

- Maintainers will review within 48-72 hours
- CI must pass (tests, linting, builds)
- At least one approval required
- Address all review comments
- Maintain professional, respectful communication

### Merge Strategy

- **Squash and merge** for feature branches
- **Merge commit** for release branches
- Maintainers will handle the merge

## Communication

- **Discord**: [Join our community](https://discord.gg/g5mp57nK) - `#contributors` channel
- **GitHub Issues**: For bugs and feature requests
- **GitHub Discussions**: For questions and general discussion
- **Pull Requests**: For code review and technical discussion

Response time expectations:
- Bug reports: 24-48 hours
- Feature requests: 1 week
- Pull requests: 48-72 hours
- Questions on Discord: Best effort, community-driven

## Recognition

Contributors are recognized in:

- **README.md** contributors section
- **Release notes** for significant contributions
- **GitHub insights** automatically tracks contributions

We deeply appreciate every contribution, large or small!

## License

By contributing to the0, you agree that your contributions will be licensed under the [Apache License 2.0](LICENSE).

---

**Thank you for contributing to the0!** Your efforts help build a better platform for algorithmic trading.
