---
title: "Recipes"
description: "Complete, runnable bots that solve a real problem, contributed by the community"
order: 4
---

# Recipes

Recipes are complete bots you can deploy as they are, or copy and adapt. Each
one solves a concrete problem with real data, and the full source lives in the
[`example-bots/`](https://github.com/alexanderwanyoike/the0/tree/dev/example-bots)
directory of the repository.

The language quick starts teach the building blocks one at a time. Recipes show
those blocks working together: configuration schemas, metrics, persistent state
and query handlers in a single bot.

## Available Recipes

| Recipe | Type | Runtime | What it does | Contributed by |
|--------|------|---------|--------------|----------------|
| [Macro Event Guard](./macro-event-guard) | Scheduled | Python 3.11 | Flags upcoming CPI, payrolls and central-bank releases so you can stand aside around them | [Robert Tidball](https://github.com/roberttidball) |

## Contributing a Recipe

Recipes are how the0 works with third-party data providers. The SDKs stay
vendor-neutral, so they will not ship clients for external APIs. A recipe is
the right place for that integration instead: a self-contained bot that calls
the provider directly or depends on the provider's own package.

To contribute one, open a pull request against `dev` that adds a directory
under `example-bots/` with:

- `bot-config.yaml`, `bot-schema.json` and a sample `config.json`
- The bot entrypoint and, where it helps, a query entrypoint
- A `README.md` covering what the bot does, its configuration and the metrics it emits
- An entry in `example-bots/README.md`

Use [`python-macro-event-guard`](https://github.com/alexanderwanyoike/the0/tree/dev/example-bots/python-macro-event-guard)
as a reference for layout and tone. If the recipe needs an API key, keep it out
of URLs, logs and query responses. Once it is merged, the recipe gets a page
here with credit to you.
