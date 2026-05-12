#!/usr/bin/env bash
set -euo pipefail

release_blockers="$1"

echo "Security scan found ${release_blockers} release-blocking findings."
exit 1
