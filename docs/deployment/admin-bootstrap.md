---
title: "Admin Bootstrap"
description: "Create the first the0 administrator"
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

If both `THE0_ADMIN_EMAIL` and `THE0_ADMIN_PASSWORD` are set before the API starts, the API creates the first admin automatically. The username is derived from the email local part. Remove `THE0_ADMIN_PASSWORD` after the admin has been created.

`THE0_ADMIN_PASSWORD` must satisfy the same password policy as normal setup and password reset flows. If the configured password is invalid, the API logs a warning and skips the bootstrap mutation. Bootstrap warnings are not exposed through the frontend.

## Upgrades

Existing users are kept active and default to the `user` role. During startup, the API preserves any existing `metadata.role = "admin"` value by promoting that user to the new `admin` role.

If no active admin exists, set both `THE0_ADMIN_EMAIL` and `THE0_ADMIN_PASSWORD`. On startup, the API promotes exactly that matching active user and sets the configured password. Email-only configuration warns and does not promote anyone.

Once an active admin exists, startup never changes an admin password from `THE0_ADMIN_PASSWORD`. This prevents a forgotten recovery variable from overwriting a password that an admin changed later in the UI. If `THE0_ADMIN_PASSWORD` is still configured after an active admin exists, the API logs a warning and ignores it.

If no admin can be created or promoted, normal login still works, but admin-only user management is unavailable and the API logs:

```text
No admin configured. Set THE0_ADMIN_EMAIL and THE0_ADMIN_PASSWORD for an existing active user or see docs/deployment/admin-bootstrap.md
```

## Behavior Matrix

| State | Configuration | Startup behavior |
|-------|---------------|------------------|
| Fresh install, no config | none | `/setup` creates the first admin |
| Fresh install, email/password | `THE0_ADMIN_EMAIL` and `THE0_ADMIN_PASSWORD` | Creates the first admin automatically |
| Upgrade, users exist, no admin | email/password for an active user | Promotes that user and sets the password |
| Upgrade, admin email only | `THE0_ADMIN_EMAIL` only | Warns and does not promote |
| Active admin already working | no password config | No startup mutation |
| Active admin already working | password config present | Warns and ignores the password config |

### Upgrade Checklist

Before upgrading an existing deployment to v1.14.0 or later:

1. Choose one existing active user who should become the first admin.
2. Set `THE0_ADMIN_EMAIL` to that exact email address in your deployment configuration.
3. Set `THE0_ADMIN_PASSWORD` to the temporary password to apply.
4. Upgrade or restart the API so admin bootstrap runs at startup.
5. Log in as that user and confirm `/settings/users` is available.
6. Create or promote any additional admins from `/settings/users`.
7. Remove `THE0_ADMIN_PASSWORD` from the deployment configuration and restart or redeploy.

After an admin role is stored in the database, `THE0_ADMIN_EMAIL` is usually no longer required. Leaving the email configured is safe, but do not leave `THE0_ADMIN_PASSWORD` configured after bootstrap.

## Docker Compose

For `the0 local`, set the admin email and password with:

```bash
the0 local admin set --email you@example.com
```

The command prompts for the password, updates `~/.the0/compose/.env` idempotently, and restarts `the0-api`. Automation can pass `--password`, but prompt mode avoids shell history exposure. The configured password is ignored when an active admin already exists.

The CLI only checks that the value can be written safely to `.env`; the API validates the password policy at startup. If the configured password is not applied, inspect the API logs with `the0 local logs api`.

Fresh local installs should use:

```text
http://localhost:3001/setup
```

## Kubernetes

Set the admin email as a normal environment variable and the password from a Secret before upgrading:

```yaml
the0Api:
  env:
    THE0_ADMIN_EMAIL: "admin@example.com"
  extraEnv:
    - name: THE0_ADMIN_PASSWORD
      valueFrom:
        secretKeyRef:
          name: the0-admin-bootstrap
          key: password
```

`extraEnv` accepts full Kubernetes `EnvVar` entries, so it works with Sealed Secrets or any controller that creates the referenced Secret. Do not put admin passwords in plaintext Helm values. Apply the values through your normal Helm upgrade workflow. Avoid shell-based recovery flows inside running pods; admin bootstrap is controlled by configuration.

## Last Admin Protection

the0 prevents changes that would leave the platform without an active admin:

- An admin cannot demote their own account.
- An admin cannot deactivate their own account.
- The final active admin cannot be demoted, deactivated, or deleted.
- API keys cannot call admin user-management endpoints.

Use `/settings/users` as an admin to create users, assign roles, deactivate accounts, and reset passwords.
