#!/usr/bin/env bash
# Renders the chart under several release names and checks that in-cluster
# URLs and Secret references point at the Services and Secrets the chart
# actually creates. Their names come from the release name, and Flux names
# releases "<namespace>-<name>", so a name hardcoded for a release called
# "the0" breaks real installs.
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

controller_arg() {
  local release=$1 flag=$2
  shift 2
  helm template "$release" "$CHART_DIR" --show-only templates/bot-controller.yaml "$@" \
    | awk -v f="--$flag=" 'index($0, f) { sub(/.*--[^=]*=/, ""); sub(/"$/, ""); print; exit }'
}

secret_name() {
  helm template "$1" "$CHART_DIR" --show-only templates/secrets.yaml \
    | awk '/^kind: Secret$/ { s = 1 } s && /^  name: / { print $2; exit }'
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

for release in the0 the0-the0 trading; do
  check "bot pods read storage credentials from the chart's Secret (release $release)" \
    "$(secret_name "$release")" "$(controller_arg "$release" storage-credentials-secret)"
done
check "bot pods read storage credentials from global.existingSecret" \
  "the0-secrets" "$(controller_arg the0-the0 storage-credentials-secret --set global.existingSecret=the0-secrets)"
check "bot pods read the access key from the Secret's minio-access-key" \
  "minio-access-key" "$(controller_arg the0 storage-credentials-access-key-field)"
check "bot pods read the secret key from the Secret's minio-secret-key" \
  "minio-secret-key" "$(controller_arg the0 storage-credentials-secret-key-field)"

override="http://query.example:9477"
check "api RUNTIME_QUERY_URL honours an explicit override" \
  "$override" "$(api_env the0 RUNTIME_QUERY_URL --set the0Api.env.RUNTIME_QUERY_URL="$override")"
check "api RUNTIME_QUERY_URL is set once when overridden" \
  "$override" "$(api_env the0 RUNTIME_QUERY_URL --set the0Api.env.RUNTIME_QUERY_URL="$override" | paste -sd ' ')"

check "Artifact Hub annotations are valid and quoted" \
  "ok" "$(python3 "$CHART_DIR/tests/check-annotations.py" "$CHART_DIR/Chart.yaml")"

if ((failures > 0)); then
  echo "$failures check(s) failed"
  exit 1
fi
