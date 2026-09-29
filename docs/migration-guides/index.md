---
title: "Migration Guides"
description: "Operator migration guides for the0 releases"
order: 6
---

# Migration Guides

Use these guides before upgrading deployments that already have data or users.
They focus on operator actions that are easy to miss from release notes alone.

## Guides

- [v1.14.0 Root Admin Migration](./v1-14-root-admin) - migrate from public-registration deployments to deployment-managed root admin credentials.
- [Bundled Object Store: MinIO to Silo](./bundled-object-store-silo) - the bundled object store image moves from `pgsty/minio` to `pgsty/silo`. Existing data is read in place.
