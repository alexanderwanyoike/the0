---
title: "Production Checklist"
description: "Preflight checks before running the0 in production"
tags: ["deployment", "production", "security"]
order: 4
---

# Production Checklist

Use this checklist before exposing a the0 deployment to users or real broker credentials.

## Identity And Access

- Configure `THE0_ADMIN_EMAIL` and `THE0_ADMIN_PASSWORD`; public registration is disabled.
- Store the root admin password in protected deployment configuration, not plaintext Helm values or committed Compose files.
- Create named admin and user accounts after first login; avoid routine use of the deployment-managed root admin.
- Rotate API keys when users leave or automation ownership changes.

## Secrets

- Generate a strong `JWT_SECRET`; the API fails startup when it is missing.
- Replace default PostgreSQL, MongoDB, MinIO, and Redis credentials.
- Keep broker and exchange credentials outside bot config until runtime secret storage is available.
- Use Kubernetes Secrets, Sealed Secrets, External Secrets, or an equivalent secret manager for production clusters.

## Network And TLS

- Terminate HTTPS at the ingress, reverse proxy, or load balancer.
- Do not expose PostgreSQL, MongoDB, NATS, Redis, or MinIO API ports publicly.
- Restrict MinIO console access to operators.
- Set DNS records and ingress hosts before issuing certificates.

## Storage And Backups

- Back up PostgreSQL; it contains users, bot definitions, and platform configuration.
- Back up MongoDB if runtime state and execution coordination data must survive disaster recovery.
- Back up object storage buckets containing bot packages, logs, state archives, and artifacts.
- Test restore steps before relying on the backups.

## Runtime Safety

- Set bot memory and CPU limits for the deployment mode you use.
- Keep runtime workers on nodes where Docker or Kubernetes permissions are intentional and isolated.
- Monitor API, runtime, database, NATS, and object storage health.
- Confirm completed scheduled jobs and pods are cleaned up in Kubernetes deployments.

## Upgrades

- Read migration guides before upgrading across minor versions.
- For Kubernetes, confirm deployment pod templates include chart-version checksum annotations so Flux and Kubernetes roll pods after chart upgrades.
- Run upgrades first in a non-production namespace or host when possible.
- Keep a rollback path for the chart version, image tags, and database backup.

