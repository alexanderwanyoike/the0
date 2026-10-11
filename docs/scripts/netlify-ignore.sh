#!/usr/bin/env bash
# Netlify's ignore command: exit 0 skips the build, anything else builds.
# Without a build cache Netlify sets CACHED_COMMIT_REF to COMMIT_REF, so its
# default check (a git diff between the two) always finds no change and
# cancels; a canceled build saves no cache, so production stopped rebuilding.
# Skip only when there is a real previous commit and nothing here changed.
if [ -z "${CACHED_COMMIT_REF:-}" ] || [ "$CACHED_COMMIT_REF" = "${COMMIT_REF:-}" ]; then
  exit 1
fi
git diff --quiet "$CACHED_COMMIT_REF" "$COMMIT_REF" -- .
