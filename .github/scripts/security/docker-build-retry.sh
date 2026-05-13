#!/usr/bin/env bash
set -euo pipefail

if [[ -z "${1:-}" || -z "${2:-}" || -z "${3:-}" ]]; then
  echo "Usage: $0 <image> <dockerfile> <context>" >&2
  exit 1
fi

image="$1"
dockerfile="$2"
context="$3"
attempts="${DOCKER_BUILD_ATTEMPTS:-3}"
delay_seconds="${DOCKER_BUILD_RETRY_DELAY_SECONDS:-30}"

for attempt in $(seq 1 "${attempts}"); do
  echo "Docker build attempt ${attempt}/${attempts}: ${image}"
  if docker build -t "${image}" -f "${dockerfile}" "${context}"; then
    exit 0
  fi

  if [[ "${attempt}" -lt "${attempts}" ]]; then
    echo "Docker build failed; retrying in ${delay_seconds}s..." >&2
    sleep "${delay_seconds}"
  fi
done

echo "Docker build failed after ${attempts} attempts: ${image}" >&2
exit 1
