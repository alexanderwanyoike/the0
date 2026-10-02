#!/usr/bin/env bash
# Renders the chart under several release names and checks that in-cluster
# URLs point at the Services the chart actually creates. Service names come
# from the release name, and Flux names releases "<namespace>-<name>", so a
# URL hardcoded for a release called "the0" breaks real installs.
set -euo pipefail

CHART_DIR="$(cd "$(dirname "$0")/.." && pwd)"
failures=0

bot_controller_service() {
  helm template "$1" "$CHART_DIR" --show-only templates/bot-controller.yaml \
    | awk '/^kind: Service$/ { svc = 1 } svc && /^  name: / { print $2; exit }'
}

api_env() {
  local release=$1 name=$2
  shift 2
  helm template "$release" "$CHART_DIR" --show-only templates/the0-api.yaml "$@" \
    | awk -v n="$name" '$0 ~ "- name: " n "$" { getline; sub(/^ *value: /, ""); gsub(/"/, ""); print }'
}

check() {
  if [[ "$2" == "$3" ]]; then
    echo "ok   $1"
  else
    echo "FAIL $1: expected '$2', got '$3'"
    failures=$((failures + 1))
  fi
}

for release in the0 the0-the0 trading; do
  service=$(bot_controller_service "$release")
  check "api RUNTIME_QUERY_URL reaches the bot-controller Service (release $release)" \
    "http://$service:9477" "$(api_env "$release" RUNTIME_QUERY_URL)"
done

override="http://query.example:9477"
check "api RUNTIME_QUERY_URL honours an explicit override" \
  "$override" "$(api_env the0 RUNTIME_QUERY_URL --set the0Api.env.RUNTIME_QUERY_URL="$override")"
check "api RUNTIME_QUERY_URL is set once when overridden" \
  "$override" "$(api_env the0 RUNTIME_QUERY_URL --set the0Api.env.RUNTIME_QUERY_URL="$override" | paste -sd ' ')"

if ((failures > 0)); then
  echo "$failures check(s) failed"
  exit 1
fi
