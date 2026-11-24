---
title: "Unique Bot Instance Names"
description: "Resolve existing duplicate bot names before applying the uniqueness migration"
tags: ["migration", "deployment", "bots"]
order: 3
---

# Unique Bot Instance Names

Migration `0009_unique_bot_names` requires each bot instance name to be unique
within its owner's account. Names remain case-sensitive, and different users
can use the same name. Both concurrent deploys and renames to a name already
in use now return a conflict. Updating a bot without changing its name still
works.

The migration does not rename or delete existing bots. If an account already
has duplicates, the constraint cannot be added and API startup stops at the
migration. Resolve those duplicates before upgrading.

## Before upgrading

1. Back up PostgreSQL and pause automated deploys and renames for the upgrade.
2. Run this read-only query as the database operator:

   ```sql
   SELECT user_id, name, count(*) AS instances, array_agg(id ORDER BY id) AS bot_ids
   FROM bots
   GROUP BY user_id, name
   HAVING count(*) > 1
   ORDER BY user_id, name;
   ```

3. For each group, have the owner decide which bot keeps the name. On the
   existing release, rename each remaining bot with
   `the0 bot update <bot-id> <config-file>`, using its current configuration with a distinct `name`.
   Update any automation that deploys that configuration too. Review each bot
   by ID so the intended instance is changed.
4. Run the query again. Upgrade only when it returns no rows, and keep writes
   paused until the migration finishes.

Bot IDs remain unchanged when a bot is renamed. Its logs, state and runtime
identity remain associated with that ID. If a duplicate was accidental and
should no longer run, the owner can explicitly disable or delete it instead.

## Verify the upgrade

The API applies the registered migration before starting. Confirm API readiness
and check that a duplicate deploy returns HTTP 409 with the existing-name
message. A rename to another bot's name should also return 409. Neither refusal
creates an execution event or changes the stored bot.

If the migration reports duplicate names, restore the previous API release,
resolve them there and retry the upgrade. Do not mark the migration complete
manually or drop bot records just to make startup succeed.
