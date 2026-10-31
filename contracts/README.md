# api and runtime contracts

The api (`api/`, TypeScript) and the runtime (`runtime/`, Go) talk over NATS, MinIO and HTTP. Each side writes its half of every agreement by hand, in its own language. The JSON files here are worked examples of those agreements. Both sides load them in their tests:
- the api's Jest suites assert that the api produces, and accepts, exactly these examples
- the runtime's Go tests assert the same for the runtime

When one side drifts, its tests fail against the shared example, not just against its own copy.

## The rule

Change a contract here first, then make both sides pass. A test that only one side runs belongs in that side's own tests, not here. Nothing is generated from these files, and no code outside tests reads them.

## Files

| File | Agreement |
|---|---|
| `events/streams.json` | The JetStream streams the api creates and the subjects each holds |
| `events/<subject>.json` | One bot event per subject: the subject and the payload the api publishes on it |
| `bot-ids.json` | Bot IDs both sides accept and use unchanged, and IDs the api refuses to read logs or state for |

## Where the tests are

Each side has one loader that finds this folder from wherever its tests run:
- api: `api/src/test/contracts.ts`
- runtime: `runtime/internal/contracts`

Search for the loader's callers to find every test that reads a file here.
