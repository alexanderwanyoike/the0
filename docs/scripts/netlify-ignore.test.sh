#!/usr/bin/env bash
# Runs the ignore command from docs/netlify.toml against a throwaway repo.
# Netlify skips a build when that command exits 0 and builds otherwise.
set -uo pipefail

DOCS_DIR="$(cd "$(dirname "$0")/.." && pwd)"
IGNORE=$(python3 -c 'import sys, tomllib; print(tomllib.load(open(sys.argv[1], "rb"))["build"].get("ignore", ""))' "$DOCS_DIR/netlify.toml")
repo=$(mktemp -d)
trap 'rm -rf "$repo"' EXIT
failures=0

git -C "$repo" init -q
git -C "$repo" config user.email test@example.com
git -C "$repo" config user.name test
mkdir -p "$repo/docs" "$repo/api"
# The ignore command may call scripts relative to the base directory.
cp -R "$DOCS_DIR/scripts" "$repo/docs/scripts"
commit() { git -C "$repo" add -A && git -C "$repo" commit -qm "$1" && git -C "$repo" rev-parse HEAD; }

echo one >"$repo/docs/page.md"; echo one >"$repo/api/app.ts"
preview_build=$(commit "dev head, built as a deploy preview")
echo two >"$repo/api/app.ts"
release_merge=$(commit "release merge on main, same docs")

check() {
  local description=$1 cached=$2 current=$3 code
  if [[ -z $IGNORE ]]; then
    echo "FAIL $description: netlify.toml sets no ignore command, so Netlify's own content check applies"
    failures=$((failures + 1))
    return
  fi
  (cd "$repo/docs" && CONTEXT=production CACHED_COMMIT_REF=$cached COMMIT_REF=$current bash -c "$IGNORE" >/dev/null 2>&1)
  code=$?
  if ((code != 0)); then
    echo "ok   $description"
  else
    echo "FAIL $description: the ignore command exited 0, so Netlify cancels the build"
    failures=$((failures + 1))
  fi
}

check "production builds after a deploy preview already built the same docs" "$preview_build" "$release_merge"
check "production builds without a build cache" "$release_merge" "$release_merge"

if ((failures > 0)); then
  echo "$failures check(s) failed"
  exit 1
fi
