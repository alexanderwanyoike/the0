---
title: "Admin Bootstrap"
description: "Create and recover the first the0 administrator"
tags: ["deployment", "auth", "admin"]
order: 3
---

# Admin Bootstrap

the0 does not allow public signup. The first screen is one of:

- `/setup` when no users exist
- `/login` when users already exist
- `/dashboard` when you are already authenticated

## Fresh Installs

On a new database, open the frontend and complete `/setup`. The setup form creates the first user with the `admin` role.

If `THE0_ADMIN_EMAIL` is set, setup only accepts that exact email address.

## Upgrades

Existing users are kept active and default to the `user` role. During startup, the API preserves any existing `metadata.role = "admin"` value by promoting that user to the new `admin` role.

If no active admin exists, set `THE0_ADMIN_EMAIL` to the email of one existing active user. On startup, the API promotes exactly that matching user. If the variable is unset or does not match an active user, normal login still works, but admin-only user management is unavailable and the API logs:

```text
No admin configured. Set THE0_ADMIN_EMAIL to an existing active user or see docs/deployment/admin-bootstrap.md
```

### Upgrade Checklist

Before upgrading an existing deployment to v1.14.0 or later:

1. Choose one existing active user who should become the first admin.
2. Set `THE0_ADMIN_EMAIL` to that exact email address in your deployment configuration.
3. Upgrade or restart the API so admin bootstrap runs at startup.
4. Log in as that user and confirm `/settings/users` is available.
5. Create or promote any additional admins from `/settings/users`.

After an admin role is stored in the database, `THE0_ADMIN_EMAIL` is no longer required for that deployment. Leaving it configured is safe; the API only uses it when no active admin exists.

## Docker Compose

For `the0 local`, set the admin email with:

```bash
the0 local admin set --email you@example.com
```

The command updates `~/.the0/compose/.env` idempotently and restarts `the0-api`.

Fresh local installs should use:

```text
http://localhost:3001/setup
```

## Kubernetes

Set the admin email in Helm values before upgrading:

```yaml
the0Api:
  env:
    THE0_ADMIN_EMAIL: "admin@example.com"
```

Apply the values through your normal Helm upgrade workflow. Avoid shell-based recovery flows inside running pods; admin bootstrap is controlled by configuration.

## Last Admin Protection

the0 prevents changes that would leave the platform without an active admin:

- An admin cannot demote their own account.
- An admin cannot deactivate their own account.
- The final active admin cannot be demoted, deactivated, or deleted.
- API keys cannot call admin user-management endpoints.

Use `/settings/users` as an admin to create users, assign roles, deactivate accounts, and reset passwords.
