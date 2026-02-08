# Repository Guidelines

## Project Structure & Module Organization
- `api/`: NestJS REST API, database migrations, and API tests (`*.spec.ts`).
- `frontend/`: Next.js + React dashboard UI, Tailwind styling, Jest/RTL tests.
- `runtime/`: Go bot runner/scheduler services.
- `cli/`: Go-based CLI tool.
- `sdk/`: Language SDKs (python/nodejs/rust/cpp/dotnet/scala/haskell/react).
- `docs/`: Documentation site content.
- `database/`, `services/`, `k8s/`, `example-bots/`: supporting services, deployment, and samples.

## Agent Coordination & Context
- This repo uses multiple agents (Claude and Gemini). Use `AGENTS.md` as the shared context baseline for this repo.
- Before implementing a feature, update `AGENTS.md` with your current understanding of the codebase and any relevant discoveries.
- Use `CLAUDE.md` and `GEMINI.md` for architecture guidance, but prioritize what the codebase currently does.

## Build, Test, and Development Commands
- `the0 local up`: Start the full stack locally.
- `the0 local dev`: Dev stack with hot reload for API/frontend/docs.
- `cd frontend && yarn install && yarn dev`: Run the web app locally.
- `cd api && yarn install && yarn start:dev`: Run the API in watch mode.
- `cd cli && make build` or `make install`: Build/install the CLI.
- `cd runtime && make build`: Build runtime services.

## Coding Style & Naming Conventions
- Follow existing style in each module; keep functions small and names explicit.
- TypeScript: ESLint + Prettier (`yarn lint`, `yarn format`).
- Go: `gofmt`, `go vet` (see `make fmt`, `make lint`).
- Python (SDKs/services): PEP 8 with type hints; use `black` where present.

## Testing Guidelines
- Tests are required for changes.
- API: Jest with `*.spec.ts` under `api/src` (`yarn test`, `yarn test:e2e`).
- Frontend: Jest + React Testing Library (`yarn test`, `yarn test:watch`).
- Go services/CLI: `go test ./...` or `make test`.

## Commit & Pull Request Guidelines
- Commit messages: `type: brief description` (types: `feat`, `fix`, `docs`, `refactor`, `test`, `chore`).
- Use the PR template (`.github/PULL_REQUEST_TEMPLATE.md`) and link issues (e.g., `Fixes #123`).
- Include tests run, update docs when user-facing changes exist, and add screenshots for UI changes.
- Keep PRs focused; maintainers prefer squash-and-merge for feature branches.

## Configuration & Secrets
- Frontend local env: `frontend/.env.local` (keep secrets out of Git).

## Runtime Notes (Codex)
- Runtime uses a unified daemon model (init + sync) for bot containers; Docker mode runs bot-runner and bot-scheduler as polling services while Kubernetes mode runs controllers that reconcile Pods/CronJobs.
- Bot code/state/logs are persisted via MinIO; Docker and K8s modes share storage helpers under `runtime/internal/runtime/storage`.
