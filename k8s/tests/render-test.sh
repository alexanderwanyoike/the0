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

# The api and the runtime read the same settings under different names, and
# the MinIO endpoint in a different shape. Each case below sets the values a
# self-hoster would, and every container must still agree with the api.
in_template='/^# Source: / { in_t = ($3 == t) }'

rendered_env() {
  awk -v t="the0/templates/$2" -v n="$3" "$in_template"'
    in_t && $0 ~ "^ *- name: " n "$" { getline; sub(/^ *value: /, ""); gsub(/"/, ""); print }
  ' "$1"
}

rendered_arg() {
  awk -v t="the0/templates/$2" -v f="--$3=" "$in_template"'
    in_t && index($0, f) { sub(/.*--[^=]*=/, ""); sub(/"$/, ""); print; exit }
  ' "$1"
}

# An empty reference value means the setting vanished from both sides, which
# check alone would accept.
check_agrees() {
  if [[ -z "$2" ]]; then
    echo "FAIL $1: the reference value is empty"
    failures=$((failures + 1))
  else
    check "$@"
  fi
}

rendering=$(mktemp)
trap 'rm -f "$rendering"' EXIT

shared_value_cases=(
  "defaults|"
  "in-cluster ports|--set nats.port=4333 --set minio.port=9100"
  "external NATS URL and S3|--set global.existingSecret=s --set nats.enabled=false --set nats.external.url=nats://nats.example:4222 --set minio.enabled=false --set minio.external.endpoint=s3.example.com --set minio.external.port=8443 --set minio.external.useSSL=false"
  "external NATS host and port|--set global.existingSecret=s --set nats.enabled=false --set nats.external.host=nats.example --set nats.external.port=4333"
  "custom logs bucket|--set the0Api.env.LOG_BUCKET=team-logs"
)

for release in the0 the0-the0 trading; do
  for shared_case in "${shared_value_cases[@]}"; do
    label="${shared_case%%|*} (release $release)"
    read -ra overrides <<< "${shared_case#*|}"
    # bash before 4.4 treats an empty "${overrides[@]}" as unbound under set -u.
    helm template "$release" "$CHART_DIR" ${overrides[@]+"${overrides[@]}"} > "$rendering"

    api_nats=$(rendered_env "$rendering" the0-api.yaml NATS_URLS)
    check_agrees "controller NATS_URL matches api NATS_URLS: $label" \
      "$api_nats" "$(rendered_env "$rendering" bot-controller.yaml NATS_URL)"

    api_minio_host=$(rendered_env "$rendering" the0-api.yaml MINIO_ENDPOINT)
    api_minio_port=$(rendered_env "$rendering" the0-api.yaml MINIO_PORT)
    api_minio=""
    if [[ -n "$api_minio_host" && -n "$api_minio_port" ]]; then
      api_minio="$api_minio_host:$api_minio_port"
    fi
    check_agrees "controller MINIO_ENDPOINT matches api MINIO_ENDPOINT:MINIO_PORT: $label" \
      "$api_minio" "$(rendered_env "$rendering" bot-controller.yaml MINIO_ENDPOINT)"
    check_agrees "controller --minio-endpoint matches api MINIO_ENDPOINT:MINIO_PORT: $label" \
      "$api_minio" "$(rendered_arg "$rendering" bot-controller.yaml minio-endpoint)"
    check_agrees "gc MINIO_ENDPOINT matches api MINIO_ENDPOINT:MINIO_PORT: $label" \
      "$api_minio" "$(rendered_env "$rendering" gc-cronjob.yaml MINIO_ENDPOINT)"

    api_ssl=$(rendered_env "$rendering" the0-api.yaml MINIO_USE_SSL)
    check_agrees "controller MINIO_USE_SSL matches the api: $label" \
      "$api_ssl" "$(rendered_env "$rendering" bot-controller.yaml MINIO_USE_SSL)"
    check_agrees "gc MINIO_USE_SSL matches the api: $label" \
      "$api_ssl" "$(rendered_env "$rendering" gc-cronjob.yaml MINIO_USE_SSL)"

    api_logs=$(rendered_env "$rendering" the0-api.yaml LOG_BUCKET | paste -sd ' ')
    check_agrees "gc MINIO_LOGS_BUCKET matches api LOG_BUCKET: $label" \
      "$api_logs" "$(rendered_env "$rendering" gc-cronjob.yaml MINIO_LOGS_BUCKET)"
  done
done

for release in the0 the0-the0 trading; do
  helm template "$release" "$CHART_DIR" --show-only templates/bot-controller.yaml > "$rendering"
  container_port=$(awk '/- name: query$/ { getline; if ($1 == "containerPort:") { print $2; exit } }' "$rendering")
  service_port=$(awk '/^kind: Service$/ { svc = 1 } svc && /- port: / { port = $3 } svc && /targetPort: query$/ { print port; exit }' "$rendering")
  query_url=$(api_env "$release" RUNTIME_QUERY_URL)
  check_agrees "bot-controller Service port matches the query containerPort (release $release)" \
    "$container_port" "$service_port"
  check_agrees "api RUNTIME_QUERY_URL dials the query containerPort (release $release)" \
    "$container_port" "${query_url##*:}"
done

check "Artifact Hub annotations are valid and quoted" \
  "ok" "$(python3 "$CHART_DIR/tests/check-annotations.py" "$CHART_DIR/Chart.yaml")"

if ((failures > 0)); then
  echo "$failures check(s) failed"
  exit 1
fi
