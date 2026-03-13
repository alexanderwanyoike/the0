import pytest

SAMPLE_AUDIT = """\
# Codebase Audit - Issues Shortlist

Scoured on 2026-03-11. Review and promote valid ones to GitHub Issues.

---

## CRITICAL / HIGH

### 1. [RESOLVED] Docker network name mismatch breaks bot execution
- **Where**: `cli/internal/local/compose_files/docker-compose.yml`
- Services connect to `the0-network` but `DOCKER_NETWORK` env var is set to `the0-oss-network`
- **Label**: bug

### 2. API Dockerfile health check starts entire app
- **Where**: `api/Dockerfile:45-46`
- Should use `wget --spider http://0.0.0.0:3000/`
- **Label**: bug

### 3. Hardcoded default JWT secret doesn't fail fast
- **Where**: `api/src/auth/jwt.strategy.ts:20`
- Falls back to `"the0-oss-jwt-secret-change-in-production"`
- **Label**: security

---

## MEDIUM

### 12. Unsafe `(request as any).user?.uid` pattern repeated everywhere
- **Where**: `api/src/custom-bot/custom-bot.controller.ts`
- Should create a `@CurrentUser()` decorator
- **Label**: refactor

### 13. [IN-PROGRESS: fix/audit-13-excessive-any] Excessive `any` types across API (~100+ instances)
- **Where**: Multiple API files
- Loss of compile-time type checking
- **Label**: refactor

### 14. Auth cookies set without Secure/SameSite flags
- **Where**: `frontend/src/contexts/auth-context.tsx`
- `document.cookie` set without `Secure`, `SameSite`, or `HttpOnly`
- **Label**: security

---

## LOW

### 24. Missing security contexts in K8s API and Frontend deployments
- **Where**: `k8s/templates/the0-api.yaml`
- Only bot-controller has `runAsNonRoot: true`
- **Label**: security

### 25. [ATTEMPTED: build-failure 2026-03-12] No liveness probe for frontend in K8s
- **Where**: `k8s/templates/the0-frontend.yaml`
- Has readiness probe but no liveness
- **Label**: enhancement

### 26. Debug log statements with wrong level in runtime
- **Where**: `runtime/internal/docker/bot-scheduler/subscriber/subscriber.go`
- Uses `s.logger.Info()` with "DEBUG:" prefix
- **Label**: cleanup
"""


@pytest.fixture
def sample_audit_text():
    return SAMPLE_AUDIT
