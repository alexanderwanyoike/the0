#!/usr/bin/env bash
set -euo pipefail

if [[ -z "${1:-}" || -z "${2:-}" ]]; then
  echo "Usage: $0 <service> <image>" >&2
  exit 1
fi

service="$1"
image="$2"

mkdir -p reports summaries

if ! trivy image \
    --format json \
    --output "reports/trivy-image-${service}.json" \
    --severity UNKNOWN,LOW,MEDIUM,HIGH,CRITICAL \
    --ignore-unfixed=false \
    "${image}"; then
  echo "Trivy image scan failed for ${service}" >&2
  exit 1
fi

jq -r --arg title "Trivy image: ${service}" '
  def count_sev($sev):
    [ .Results[]? | .Vulnerabilities[]? | select(.Severity == $sev) ] | length;
  {
    critical: count_sev("CRITICAL"),
    high: count_sev("HIGH"),
    medium: count_sev("MEDIUM"),
    low: count_sev("LOW"),
    unknown: count_sev("UNKNOWN")
  }
  | "### \($title)\n\n" +
    "| Severity | Count |\n| --- | ---: |\n" +
    "| CRITICAL | \(.critical) |\n" +
    "| HIGH | \(.high) |\n" +
    "| MEDIUM | \(.medium) |\n" +
    "| LOW | \(.low) |\n" +
    "| UNKNOWN | \(.unknown) |\n"
' "reports/trivy-image-${service}.json" > "summaries/trivy-image-${service}.md"
