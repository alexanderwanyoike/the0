---
title: "Bundled Object Store: MinIO to Silo"
description: "The bundled S3-compatible store moves from pgsty/minio to pgsty/silo"
tags: ["migration", "deployment", "storage", "minio"]
order: 2
---

# Bundled Object Store: MinIO to Silo

the0 bundles an S3-compatible object store for bot code, logs and state. Upstream MinIO community edition is no longer maintained, and its images were removed from Docker Hub and quay.io in September 2026. The previous release moved the bundled store to `pgsty/minio`. No new images have been published under that name since `RELEASE.2026-08-04T00-00-00Z`: the project continues as [Silo](https://github.com/pgsty/silo) under `pgsty/silo`, and new releases, including security fixes, only ship there. The existing `pgsty/minio` tags can still be pulled.

The bundled store is now `pgsty/silo`, pinned to a release tag. Silo is the MinIO codebase under a new name, so it keeps the same S3 API, environment variables, ports, console and health endpoints, and reads existing data volumes in place.

## Who Must Act

You only need to act if you run the **bundled** store:

- Docker Compose installs created with `the0 local init`
- Kubernetes installs with `minio.enabled: true` (the chart default)

If your deployment uses external S3-compatible storage (`minio.enabled: false`, for example AWS S3, Cloudflare R2 or GCS), nothing changes.

## What Changes

| Before | After |
| --- | --- |
| `pgsty/minio:RELEASE.2026-08-04T00-00-00Z` | `pgsty/silo:RELEASE.2026-09-16T00-00-00Z` |
| Chart overrides the container `command` with `minio server ...` | Chart passes `args` and lets the image entrypoint pick the binary |
| Store Deployment uses a rolling update | Store Deployment uses `strategy: Recreate` |

The data volume, credentials, bucket names, ports 9000 and 9001, and the `/minio/health/live` and `/minio/health/ready` endpoints are unchanged.

The Silo image has no `minio` binary. That is why the chart now passes `args` instead of `command`: with the old template, the pod would fail with `exec: "minio": executable file not found`. The `args` form also keeps working if you override `minio.image` with a MinIO-based image of your own.

`Recreate` stops the old store pod before the new one starts. A rolling update would briefly run two servers against the same ReadWriteOnce volume, or hang when the volume cannot attach to a second node. Expect the store to be unavailable for the few seconds it takes to restart during the upgrade.

## Docker Compose

Update the CLI, then refresh the generated Compose files. `the0 local init` is what rewrites `~/.the0/compose/`, so an existing install keeps the old image until you run it.

For a source-mode local install:

```bash
the0 local init --source /path/to/the0 --email admin@example.com --password 'your-password'
the0 local start
```

For a prebuilt-image local install:

```bash
the0 local init --email admin@example.com --password 'your-password'
the0 local start
```

The existing `minio_data` volume is reused and read in place. Verify the stack:

```bash
the0 local status
```

## Kubernetes

Upgrade the release as usual:

```bash
helm repo update
helm upgrade the0 the0/the0 --namespace the0 -f values.yaml
```

If your values pin `minio.image`, update it to `pgsty/silo:RELEASE.2026-09-16T00-00-00Z` or remove the override to take the chart default.

Check that the store pod is ready and the API can reach it:

```bash
kubectl -n the0 get pods -l app.kubernetes.io/component=minio
kubectl -n the0 logs deploy/the0-api | grep -i minio
```

## Rolling Back

The on-disk format is shared, so setting `minio.image` (or the Compose image) back to `pgsty/minio:RELEASE.2026-08-04T00-00-00Z` reads the same volume.

That tag can still be pulled from Docker Hub as of this release, but the repository no longer changes, and upstream MinIO images were deleted without notice. If you want a rollback path that does not depend on it, keep a local copy before upgrading:

```bash
docker pull pgsty/minio:RELEASE.2026-08-04T00-00-00Z
docker save pgsty/minio:RELEASE.2026-08-04T00-00-00Z -o pgsty-minio-2026-08-04.tar
```

For Kubernetes, push that image to a registry your cluster can pull from. Taking a copy of your buckets before any storage upgrade is still good practice, for example with `mc mirror`.
