#!/usr/bin/env bash
set -euo pipefail

if [[ -z "${1:-}" ]]; then
  echo "Usage: $0 <release_blockers_count>" >&2
  exit 1
fi

if ! [[ "$1" =~ ^[0-9]+$ ]]; then
  echo "Error: release_blockers must be a non-negative integer, got: $1" >&2
  exit 1
fi

release_blockers="$1"

echo "Security scan found ${release_blockers} release-blocking findings."
exit 1
