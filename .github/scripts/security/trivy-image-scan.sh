#!/usr/bin/env bash
set -euo pipefail

service="$1"
image="$2"

mkdir -p reports summaries

trivy image \
  --format json \
  --output "reports/trivy-image-${service}.json" \
  --severity UNKNOWN,LOW,MEDIUM,HIGH,CRITICAL \
  --ignore-unfixed=false \
  "${image}"

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
