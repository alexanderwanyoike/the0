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
- Rotate API keys when users leave or automation ownership changes, and see [API Keys And MCP](#api-keys-and-mcp) for public instances.

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

## Exposure

Login is the one place an unauthenticated visitor can try a password, and the0 does not rate-limit it. Keep it off the open internet, or rate-limit it in front of the0.

### Prefer A Private Network

Run the0 on a private network such as Tailscale or WireGuard, and don't publish the frontend or API hosts at all. Operators, the CLI and agents reach the instance over the private network, and nobody on the internet can try a password. If you also serve the docs publicly, give the docs host its own ingress rule and leave the frontend and API hosts off it.

### If The Instance Must Be Public

Rate-limit both login paths, and only those paths:

| Host | Path | Used by |
|---|---|---|
| Frontend, e.g. `the0.example.com` | `/api/auth/login` | The login form in the browser |
| API, e.g. `api.the0.example.com` | `/auth/login` | Direct API clients |

Both need a limit. The frontend forwards browser logins to the API from inside the deployment, so the API sees the frontend's address, not the visitor's, and a limit on the API host alone never sees browser logins. Leave the rest of the site alone, because the dashboard streams logs and makes many API calls that a site-wide limit would throttle.

#### nginx

For Docker Compose, or any nginx in front of the0, add a zone to the `http` block and a `location` for each login path next to your existing `location /`:

```nginx
# http block
limit_req_zone $binary_remote_addr zone=the0_login:10m rate=5r/m;
limit_req_status 429;

# frontend server block
location = /api/auth/login {
    limit_req zone=the0_login burst=4 nodelay;
    proxy_pass http://127.0.0.1:3001;  # same upstream and headers as location /
}

# API server block
location = /auth/login {
    limit_req zone=the0_login burst=4 nodelay;
    proxy_pass http://127.0.0.1:3000;  # same upstream and headers as location /
}
```

Each client IP gets five quick attempts across both paths, then one every 12 seconds, and nginx answers the rest with 429. If nginx sits behind a load balancer or CDN, set `set_real_ip_from` and `real_ip_header` so `$binary_remote_addr` is the visitor's address rather than the balancer's.

#### Kubernetes With Traefik

The chart puts every host on one Ingress, so a limit on that Ingress would apply to every page. Add a route for the two login paths instead. Traefik picks it over the chart's `/` rule because its match is more specific.

```yaml
apiVersion: traefik.io/v1alpha1
kind: Middleware
metadata:
  name: the0-login-rate-limit
  namespace: the0
spec:
  rateLimit:
    average: 5
    period: 1m
    burst: 5
---
apiVersion: traefik.io/v1alpha1
kind: IngressRoute
metadata:
  name: the0-login
  namespace: the0
spec:
  entryPoints:
    - websecure
  routes:
    - match: Host(`the0.example.com`) && Path(`/api/auth/login`)
      kind: Rule
      middlewares:
        - name: the0-login-rate-limit
      services:
        - name: the0-frontend
          port: 3000
    - match: Host(`api.the0.example.com`) && Path(`/auth/login`)
      kind: Rule
      middlewares:
        - name: the0-login-rate-limit
      services:
        - name: the0-api
          port: 3000
  tls:
    secretName: the0-tls  # the certificate your Ingress already uses
```

Service names follow the chart's full name. With the release name `the0` they are `the0-frontend` and `the0-api`, and `kubectl -n the0 get svc` shows yours. Traefik counts each route separately, so each login path allows five quick attempts per client IP before answering 429. Traefik counts by the address of the connection it receives, so behind a load balancer every visitor would share one count. Enable proxy protocol on the balancer and Traefik, or, if the balancer adds `X-Forwarded-For`, add `sourceCriterion: {ipStrategy: {depth: 1}}` under `rateLimit` so Traefik counts by the visitor's address.

#### Cloudflare

Add a rate limiting rule with the expression `(http.host eq "the0.example.com" and http.request.uri.path eq "/api/auth/login") or (http.host eq "api.the0.example.com" and http.request.uri.path eq "/auth/login")`.

### API Keys And MCP

API keys are 32 random bytes, so they can't be guessed, but a key can do everything its user can with bots, custom bots, logs and queries, over the REST API and the MCP endpoint. On a public instance, treat keys like passwords. Keep them out of shared configs and repositories, and when one may have leaked, delete it in the dashboard's API settings and create a new one. [Secret Rotation](./secret-rotation) covers the platform's own secrets.

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

