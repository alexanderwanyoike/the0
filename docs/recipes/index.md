---
title: "Recipes"
description: "In-depth walkthroughs of complete bots, from first file to deployment"
order: 4
---

# Recipes

The language quick starts introduce each building block on its own. Recipes go
deeper: each one takes a complete, working bot and walks through how it is
built, file by file, explaining the design decisions along the way.

By the end of a recipe you will have seen configuration schemas, metrics,
persistent state and query handlers working together in a real bot, and you
will have a codebase you can deploy as it is or adapt into your own.

Every recipe is backed by a runnable example in the
[`example-bots/`](https://github.com/alexanderwanyoike/the0/tree/dev/example-bots)
directory.

## Available Recipes

| Recipe | You will learn | Language | Bot type |
|--------|----------------|----------|----------|
| [Macro Event Guard](./macro-event-guard) | Calling an external API safely, normalising untrusted data, emitting metrics, sharing state with query handlers | Python | Scheduled |

## Suggesting a Recipe

Built a bot that would make a good walkthrough? Open a pull request adding it
under `example-bots/`, or start a discussion on
[Discord](https://discord.gg/8xpMh3RR3).
