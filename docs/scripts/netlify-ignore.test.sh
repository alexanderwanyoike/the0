#!/usr/bin/env bash
# Runs netlify-ignore.sh against a throwaway repo. Netlify skips the build when
# the ignore command exits 0 and builds on any other exit code.
set -uo pipefail

SCRIPT="$(cd "$(dirname "$0")" && pwd)/netlify-ignore.sh"
repo=$(mktemp -d)
trap 'rm -rf "$repo"' EXIT
failures=0

git -C "$repo" init -q
git -C "$repo" config user.email test@example.com
git -C "$repo" config user.name test
mkdir -p "$repo/docs" "$repo/api"
commit() { git -C "$repo" add -A && git -C "$repo" commit -qm "$1" && git -C "$repo" rev-parse HEAD; }

echo one >"$repo/docs/page.md"; echo one >"$repo/api/app.ts"
first=$(commit first)
echo two >"$repo/api/app.ts"
api_only=$(commit "api only")
echo two >"$repo/docs/page.md"
docs_change=$(commit "docs change")

check() {
  local description=$1 expected=$2 cached=$3 current=$4 code
  (cd "$repo/docs" && CACHED_COMMIT_REF=$cached COMMIT_REF=$current bash "$SCRIPT" >/dev/null 2>&1)
  code=$?
  if [[ $expected == skip && $code -eq 0 ]] || [[ $expected == build && $code -ne 0 ]]; then
    echo "ok   $description"
  else
    echo "FAIL $description: expected $expected, exit code $code"
    failures=$((failures + 1))
  fi
}

check "builds when Netlify has no cache (cached ref equals the commit)" build "$docs_change" "$docs_change"
check "builds when there is no cached ref at all" build "" "$docs_change"
check "builds when the docs changed" build "$api_only" "$docs_change"
check "builds when the cached commit is unknown" build "0000000000000000000000000000000000000000" "$docs_change"
check "skips when nothing under the base directory changed" skip "$first" "$api_only"

if ((failures > 0)); then
  echo "$failures check(s) failed"
  exit 1
fi
