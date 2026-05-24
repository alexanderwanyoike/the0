---
title: "Secret Rotation"
description: "Rotate root admin and external service secrets safely"
tags: ["deployment", "secrets", "operations"]
order: 5
---

# Secret Rotation

Rotate secrets from the deployment layer, then restart or roll out the services that consume them. Do not change deployment-managed root admin credentials through the UI.

## Local Root Admin

Rotate only the password:

```bash
the0 local reset-admin-password new-password
```

Rotate email and password together:

```bash
the0 local admin set --email admin@example.com
```

The command updates `~/.the0/compose/.env` and recreates `the0-api`. Existing sessions are invalidated when the configured root admin password changes.

## Kubernetes Root Admin

Root admin password should be supplied from a Secret through `the0Api.extraEnv`:

```yaml
the0Api:
  env:
    THE0_ADMIN_EMAIL: "admin@example.com"
  extraEnv:
    - name: THE0_ADMIN_PASSWORD
      valueFrom:
        secretKeyRef:
          name: the0-root-admin
          key: password
```

Update the referenced Secret, then roll the API:

```bash
kubectl -n the0 rollout restart deployment/the0-api
kubectl -n the0 rollout status deployment/the0-api
```

## Sealed Secrets

When using Bitnami Sealed Secrets, rotate by sealing a new Secret value and applying it through GitOps:

```bash
kubectl -n the0 create secret generic the0-root-admin \
  --from-literal=password='new-password' \
  --dry-run=client \
  -o yaml > /tmp/the0-root-admin.yaml

kubeseal --format yaml < /tmp/the0-root-admin.yaml > clusters/prod/the0-root-admin.sealed.yaml
```

Commit the sealed manifest and let Flux apply it. After the Secret changes in the cluster, restart the API deployment so startup syncs the configured root admin password.

## External Service Secrets

Rotate database, object storage, NATS, Redis, and JWT secrets one at a time:

1. Create or seal the new secret value.
2. Apply it to the cluster or host.
3. Restart the consuming service.
4. Confirm health checks and login still work.
5. Remove the old value from the external provider after consumers have moved.

For `JWT_SECRET`, all existing API tokens become invalid after rotation. Plan a user re-login window and rotate API keys separately if required.

