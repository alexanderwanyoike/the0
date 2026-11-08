# Coding Standards

These are the rules every change to the0 is reviewed against. They come from what the codebase already does consistently, plus a few process rules the project holds itself to. Each rule is meant to be checkable in a diff.

## How to read this file

- **New code follows the rule, even where old code does not.** Where a section lists "existing exceptions", those are known debt, not precedent. Don't copy them, and don't fix them in an unrelated PR.
- **When the codebase has two patterns, the rule names the newer one.** If a case isn't covered here, follow the nearest recent code in the same module.
- **Tooling wins on formatting.** Anything Prettier, `gofmt` or a linter would change is not a review comment.
- `CONTRIBUTING.md` covers setup, issues and community. This file owns the rules for code, commits and pull requests.

## Workflow

### Branches and pull requests

- Branch from `dev` and open the PR into `dev`. Name the branch `<type>/<area>-<slug>`, for example `feat/api-mcp-errors` or `fix/cli-entrypoint-check`.
- A stacked PR targets its parent branch, so its diff shows only its own change.
- One PR addresses one issue or one unit of work, and is small enough to review in one sitting.
- The PR title is a conventional commit line. The description uses these headings, in order:

  ```
  ## Background
  ## Tasks
  ## Tested
  ## Tech Debt   (optional)
  ```

  `Tasks` is a checked list. `Tested` names the spec or test files and what each proves, plus the commands run (`yarn test`, `yarn build`, `go test ./...`). A PR that changes anything visual embeds screenshots of the rendered result under `Tested`. Host them on the `pr-screenshots` orphan branch under a folder named after the feature branch, and embed them with `https://github.com/alexanderwanyoike/the0/blob/pr-screenshots/<path>?raw=true` so they render for every reviewer.
- Run the service's build locally before pushing (`yarn build` for api, frontend and docs; `go build ./...` for runtime and cli), and list the command under `Tested`.
- Releases go from `dev` to `main` through a release PR opened by a maintainer, which bumps the chart `version` and `appVersion`. A `vX.Y.Z` tag on the release merge commit builds and publishes the images and SDKs.

### Commits

- Every commit does exactly one thing. A refactor, a rename or a formatting change never shares a commit with a behaviour change.
- Messages are conventional commits with the service as scope: `type(scope): summary`. Types are `feat`, `fix`, `test`, `refactor`, `docs`, `chore`, `style`, `ci`. Scopes are `api`, `cli`, `runtime`, `frontend`, `docs`, `k8s`, `sdk-<lang>`. A change to repo-root files (`CODING_STANDARDS.md`, `CONTRIBUTING.md`, `.github/`) has no scope: `docs: ...`, `ci: ...`.
- Write the summary as the behaviour, in the present tense: `fix(api): a storage failure answers a fixed message`.

### Test-driven changes

Code changes follow red, green, refactor:

1. Write a test that fails for the intended reason, run it, and commit it alone as `test(scope): ...`.
2. Make the smallest change that passes it, and commit that as `fix(scope)` or `feat(scope)`.
3. Refactor in later commits, with the tests still passing.

Red commits are deliberate, so hooks must not run the test suite on commit, and `git bisect` should `skip` a red commit rather than mark it bad. Documentation-only and formatting-only changes need no new tests.

## Tests

- Name tests after behaviour. A `describe` names a unit, a route or a group of behaviour (`"/bot routes"`, `"when the api cannot answer"`). An `it` is a sentence about what happens: `it("refuses a second deploy with a name the user already has")`. Don't start new titles with "should", and never name a `describe` after a review round or a bug report. Provenance goes in the commit message.
- Never assert after a fixed sleep. Wait on a condition (`waitFor` in TypeScript, `require.Eventually` or the `eventstest` helpers in Go).
- Prefer real collaborators at the boundary the change touches: PGlite for repositories, `frontDoor()` for HTTP behaviour, testcontainers for Mongo, MinIO and NATS.
- Existing exceptions: about 560 api and 250 frontend `it` titles start with "should"; 34 `time.Sleep` calls remain in runtime tests.

## Comments

- A comment explains a why the code can't express: a constraint, an invariant, a trade-off. It never narrates what the next line does.
- One why, one place. If the rationale already lives on a type, a command or a message doc, don't repeat it at the call site.
- Exported Go identifiers have doc comments. TypeScript commands and shared types get a one-line JSDoc that gives the reason they exist.
- No `TODO` or `FIXME` comments. Open an issue instead.
- Existing exceptions: what-comments in `api/src/main.ts`, `api/src/api-key/api-key.service.ts` and `frontend/src/lib/auth-fetch.ts`.

## Copy

This covers docs, CLI output, error messages and UI text.

- Never use the em dash character. Use a comma, a full stop or brackets.
- Colons introduce real lists and labels only. Don't use the "setup: reveal" pattern ("So the fix: stream it").
- Call the project "the0", lowercase, never "The0 OSS".
- Error messages say what happened and what to do next: `Bot instance named "x" already exists. Use "bot update" to modify it.` Never leak driver or storage messages to callers; the api uses the fixed `READ_FAILED`, `WRITE_FAILED` and `STORAGE_*` messages for that.

## TypeScript (api, frontend, SDKs)

- Use yarn. Never commit a `package-lock.json`.
- Don't add `any` in production code, and don't silence the rule with `eslint-disable` comments. Model the type, use `unknown` and narrow, or use a generic.
- No `@ts-ignore`, and no new suppression comments for type or lint rules.
- Never loosen `tsconfig.json` or the ESLint config. The target for every TypeScript package is `strict: true`, `@typescript-eslint/no-explicit-any` as an error, and the type-aware `no-unsafe-*` rules on; see Known gaps for where each package stands.
- Existing exceptions: bare `any` in api and frontend production code (worst in `frontend/src/components/custom-bots/markdown-components.tsx`), and four `no-explicit-any` suppressions in the api (`command-bus.ts`, `role.repository.ts`, `role-revision.repository.ts`, `database/connection.ts`).
- Files are kebab-case. Import across modules with the `@/` alias, and within a module with `./`.
- Double quotes and trailing commas (Prettier defaults); run `yarn format`.

## API (NestJS)

### Layout

- One folder per feature: `<feature>.module.ts`, `.controller.ts`, `.service.ts`, `.repository.ts`, plus `dto/`, `__tests__/` and, where needed, `commands/` and `<feature>.types.ts`. Role suffixes are `.handler.ts`, `.resolver.ts`, `.validator.ts`, `.mapper.ts`, `.guard.ts`, `.decorator.ts`.
- One class per file. Existing exceptions: `bot/commands/bot-commands.ts` and `mcp/mcp.arguments.ts` group several small classes.
- A module never imports another module's repository, and never exports its own. `__tests__/architecture.spec.ts` enforces this.

### Errors

- Services, repositories and handlers return `Result<T, AppError>` (`common/result.ts`, `common/app-error.ts`) and don't throw for expected failures. Build errors with `appError(code, message)`, where `code` is one of the `ErrorCode` values (`validation`, `forbidden`, `not_found`, `conflict`, `too_large`, `unprocessable`, `internal`, `unavailable`, `timeout`).
- Controllers translate a failure with `throw toHttpException(result.error)` and return `result.data`. The status map lives in `common/http-error.ts` and nowhere else.
- MCP tools translate a failure with `toMcpToolError` (`mcp/mcp-tool-result.ts`).
- Detect database errors with the helpers in `common/database-errors.ts` (`isUniqueConstraintError`, `isForeignKeyViolation`, `isConnectionError`), never by matching message text.
- Existing exceptions: `user/user.service.ts` throws Nest exceptions; `auth.service.ts` and `nats.service.ts` use `Result<_, string>`; some controllers throw `new XException` directly.

### Writes go through commands

- Every write to a bot is a `Command` dispatched on the `CommandBus` (`command-bus/`). A command is a class of readonly fields whose first field is `actor: Actor`, with a one-line JSDoc.
- A handler is `@Injectable`, implements `CommandHandler<X>` and `OnModuleInit`, registers itself in `onModuleInit`, and returns `Result<_, AppError>`.
- A handler that changes a bot writes the row and its outbox events in one transaction through the repository's `*WithEvents` methods, then calls `relay.publishCommitted(events)` only after the commit succeeds. Never publish to NATS from inside a transaction.
- REST and MCP build the same command. They never reimplement a write.
- `BotService` is read-only.

### Repositories and schema

- Every query is scoped by `userId` (`common/role.repository.ts`). A query that crosses users is an explicit admin path.
- A table is declared twice, `xTable` (Postgres) and `xTableSqlite`, and registered in `database/connection.ts`. The exception is an intentionally Postgres-only table, which says so in its doc comment (`bot-event-outbox.ts`).
- A migration is `database/migrations/NNNN_name.sql`, split with `--> statement-breakpoint`, and must be registered in `meta/_journal.json` or it never runs. The PGlite tests apply the journal, so an unregistered migration fails them.

### Controllers, validation and docs

- Request bodies are DTO classes validated with class-validator. The global `ValidationPipe` whitelists and forbids unknown fields, so every accepted field needs a decorator.
- Every controller has `@ApiTags`, and every route has `@ApiOperation({ summary })` and one of the auth decorators in `swagger/api-auth.decorators.ts` that matches its guard. `swagger/__tests__/swagger.spec.ts` checks the match.
- Read the caller with `@CurrentActor() actor: Actor`. Existing exceptions: `@CurrentUser()` in custom-bot, logs and user, and `@Request()` in api-key.

### Logging and config

- Inject `PinoLogger`. Log with the merge object first and the message second, and put errors under `err`: `this.logger.error({ err: error, botId }, "Failed to publish bot events")`.
- No `console.*` outside `main.ts`.
- Existing exceptions: Nest's `Logger` in `common/lock.repository.ts` and `user/admin-bootstrap.service.ts`, module-level `pino()` in some repositories, and `{ error }` instead of `{ err }` in `mcp.service.ts` and `nats.service.ts`.
- Read configuration through `ConfigService`. `process.env` belongs only in `config/`, `main.ts` and the logger setup. Existing exceptions: `common/password.ts`, `common/root-admin.ts`, `database/migrate.ts`, `database/seed.ts`.

### API tests

- Specs live in `__tests__/` next to the code and end in `.spec.ts`. HTTP behaviour goes in `*.endpoint.spec.ts` using `frontDoor()` (`test/front-door.ts`), which runs the real guard and pipe.
- Repository and command tests that need SQL use PGlite through `useTestDatabase()` (`test/test-database.ts`).
- Payloads shared by the api and the runtime are JSON examples in the repo-root `contracts/` folder, checked from `*-contracts.spec.ts` (through `src/test/contracts.ts`) and from the runtime's `contracts_test.go` files.
- Use the helpers in `src/test/` (`mock-actor`, `mock-bot`, `mock-config`, `mock-logger`, `bot-command-providers`) before writing new ones.

## Frontend (Next.js)

- Folders: `app/` for routes, `components/<feature>/`, `components/ui/` for shadcn primitives, `lib/`, `hooks/`, `contexts/`, `types/`. Files are kebab-case.
- Components are function components declared as `export function Name(props: NameProps)`, with `interface NameProps`. Use named exports; default exports are only for Next.js route files (`page`, `layout`, `error`, `loading`, `not-found`). Existing exceptions: about 30 arrow components and 10 uses of `React.FC`.
- Put `"use client"` at the top of any file that uses state, effects or browser APIs.
- Talk to the api through `apiRequest<T>` (`lib/auth-fetch.ts`), which returns `Result<T, ApiError>` (`lib/result.ts`). It sits on `authFetch`, which adds the token and handles a 401; use `authFetch` directly only for streams. Browser requests go through the `/api/[...path]` proxy. Don't call `fetch` directly from components. Existing exceptions: `lib/api/admin-users.ts`, three settings forms and `lib/auth/jwt-auth.service.ts`.
- Streams use `authFetch` with `readSSEEvents` (`lib/sse/sse-stream-reader.ts`) and an `AbortController`, not `EventSource`.
- Shared state lives in React Context: `createContext<T | null>(null)` with a `useXContext` hook that throws outside its provider. There is no global store library; don't add one. Existing exception: `contexts/auth-context.tsx` uses a default object.
- Hooks are `export function useX()` and return a typed `UseXReturn`.
- Show user-facing errors with `useToast`. No `console.log`.
- Style with Tailwind and `cn()` (`lib/utils.ts`). No CSS modules, no inline `style` for layout.
- Tests use Jest and React Testing Library, live in `__tests__/` with kebab-case names, use `it(...)`, and prefer `userEvent` over `fireEvent`.

## Go (runtime, cli)

### Shared

- Files are snake_case. Tests sit next to the code as `x_test.go`.
- `context.Context` is the first parameter of any function that does I/O or waits. Never `context.TODO()`. Timeouts use `context.WithTimeout`, with shared durations in constants.
- Wrap errors with `%w` and a lowercase message that says what failed: `fmt.Errorf("start mongo container: %w", err)`. Check them with `errors.Is` and `errors.As`, never by string.
- No `panic` for expected failures. `os.Exit` only in `main`.
- Define an interface where it is consumed, keep it small, and write the test double by hand. Don't add a mocking framework. Existing exception: `runtime/internal/query/server_test.go` uses `testify/mock`.
- Tests are named `TestThing_Behaviour` and are table-driven where cases share a shape. Integration tests are `*_integration_test.go` and skip under `testing.Short()`.
- Use `t.TempDir`, `t.Setenv` and `t.Cleanup` for isolation.
- Write `any` rather than `interface{}` in new code.

### Runtime

- One package per concern under `internal/`. Test-support packages end in `test` (`mongotest`, `s3test`, `eventstest`).
- Docker and Kubernetes modes don't import each other, and the shared packages (`model`, `util`, `constants`, `runtime`, `query`) import neither mode. `depguard` in `runtime/.golangci.yml` encodes this.
- Whether a bot or schedule should run is decided only by `desired.State` and `desired.IsEnabled`. Don't add a second enabled rule.
- Inject `util.Logger` and log with snake_case keys: `logger.Info("bot started", "bot_id", id)`. `util.LogMaster` stays confined to `cmd/app`, `internal/k8s/controller` and `internal/k8s/health`.
- Long-running loops stop through `ctx.Done()` or a stop channel, and every goroutine is tracked with a `sync.WaitGroup` that shutdown waits on.
- Bot event handlers return `botevents.Permanent(err)` to drop an event they can never process. Any other error is retried.
- `runtime/wrappers/` (`python_bot.py`, `node_bot.js`) is the contract with user bots and changes only with an explicit decision.

### CLI

- A command is built by `NewXCmd()` (exported for top-level groups, unexported `newXCmd()` inside a group), uses `RunE`, and delegates its body to a lowercase handler. Commands never call `os.Exit`; exit codes are set in `cmd/execute.go`.
- Commands that call the api use `internal.NewSessionClient()` after `GetAuthTokenWithRetry()`, and every request goes through `APIClient.send` or `sendJSON` (`internal/transport.go`). Api errors arrive as `*APIError`.
- Messages to the user go through `internal/logger`: `StartSpinner`, then `StopSpinnerWithError(failureTitle("X failed", err))` and `return err` on failure, or `StopSpinnerWithSuccess` on success. Data meant for stdout (JSON, raw log lines) is printed with `fmt.Println` so it can be piped. Existing exception: `cmd/auth.go` prints messages with `fmt.Println`.
- A command that can't be undone asks for confirmation (`Type 'yes' to confirm`) and takes `--yes/-y` to skip it. A reversible command doesn't prompt. Agents drive the CLI non-interactively, so a command must never block on input when `--yes` is given or when it doesn't prompt at all.
- CLI tests use the standard library (`t.Fatalf`, `t.Errorf`) and `httptest` servers, not testify.

## Configuration and deployment

A new api or runtime setting touches every place it's read or documented, in the same PR:

- `api/src/config/configuration.ts` (api), with a safe default
- the embedded compose files in `cli/internal/local/compose_files/`
- `k8s/values.yaml`, under the service's `env` map with a comment saying what it does, or under `extraEnv` if it's a secret
- `docs/deployment/` (`docker-compose.md`, `kubernetes.md`, and `production-checklist.md` when it matters in production)

In the Helm chart:

- Build every in-cluster name from the `the0.fullname` helper. Never hardcode `the0-<component>`; the prod release name is `the0-the0`. `k8s/tests/render-test.sh` renders under several release names to catch this.
- Bump `version` and `appVersion` together. Every `artifacthub.io/changes` description is double-quoted (`k8s/tests/check-annotations.py` checks it).

## Docs

- Pages live in `docs/` (VitePress), with frontmatter `title`, `description`, `tags` and `order`, and string values double-quoted.
- A new CLI command is documented on its group's page in `docs/the0-cli/`. A breaking change gets a page in `docs/migration-guides/`.
- The Copy rules above apply.

## SDKs

- Each SDK in `sdk/` follows its language's conventions and is checked against the shared contract in `sdk/contract/`.
- SDKs stay vendor-neutral: no third-party data or broker clients. Those belong in a separate package and an example bot.
- SDK changes are their own PRs, never a side effect of a platform change.

## Known gaps

These rules aren't enforced by tooling yet. Review catches them until it is:

- ESLint, `fallow` and golangci-lint run in CI but don't block a merge, and there is no separate typecheck or format check.
- The api's `tsconfig.json` is not `strict` (`strictNullChecks` is off), and `no-explicit-any` is a warning, not an error.
- The frontend lints with `next/core-web-vitals` and `next/typescript` only.
- `gofmt` currently reports `cli/internal/environments.go` and `cli/tests/zip_test.go`.
- `cli/tests/` declares `package internal` from a separate directory.
- `api/src/database/migrations/0003_simplify_custom_bot_status.sql` is not in the journal.
