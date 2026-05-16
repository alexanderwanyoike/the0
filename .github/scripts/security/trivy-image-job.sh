#!/usr/bin/env bash
set -euo pipefail

if [[ -z "${1:-}" || -z "${2:-}" || -z "${3:-}" || -z "${4:-}" ]]; then
  echo "Usage: $0 <service> <image> <dockerfile> <context>" >&2
  exit 1
fi

service="$1"
image="$2"
dockerfile="$3"
context="$4"

mkdir -p reports summaries

record_failure() {
  local stage="$1"
  local exit_code="$2"
  local message="$3"
  local report="reports/scan-error-trivy-image-${service}.json"
  local summary="summaries/scan-error-trivy-image-${service}.md"

  jq -n \
    --arg scanner "Trivy image" \
    --arg target "${service}" \
    --arg stage "${stage}" \
    --arg message "${message}" \
    --argjson exit_code "${exit_code}" \
    '{
      scanner: $scanner,
      target: $target,
      stage: $stage,
      exit_code: $exit_code,
      message: $message
    }' > "${report}"

  {
    echo "### Trivy image: ${service}"
    echo
    echo "Scan status: failed before findings could be produced."
    echo
    echo "| Field | Value |"
    echo "| --- | --- |"
    echo "| Stage | ${stage} |"
    echo "| Exit code | ${exit_code} |"
    echo "| Message | ${message} |"
  } > "${summary}"
}

run_or_record_failure() {
  local stage="$1"
  local message="$2"
  shift 2

  set +e
  "$@"
  local status=$?
  set -e

  if [[ "${status}" -ne 0 ]]; then
    record_failure "${stage}" "${status}" "${message}"
    return "${status}"
  fi
}

if ! run_or_record_failure \
  "build" \
  "Docker image build failed before Trivy could scan the image." \
  bash .github/scripts/security/docker-build-retry.sh "${image}" "${dockerfile}" "${context}"; then
  exit 0
fi

if ! run_or_record_failure \
  "install" \
  "Trivy installation failed before the image could be scanned." \
  bash .github/scripts/security/install-trivy.sh; then
  exit 0
fi

if ! run_or_record_failure \
  "scan" \
  "Trivy failed while scanning the built image." \
  bash .github/scripts/security/trivy-image-scan.sh "${service}" "${image}"; then
  exit 0
fi
