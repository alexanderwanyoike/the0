#!/usr/bin/env bash
set -euo pipefail

if [[ -z "${1:-}" ]]; then
  echo "Usage: $0 <package_name>" >&2
  exit 1
fi

package_name="$1"
workspace="${GITHUB_WORKSPACE:-$(pwd)}"

mkdir -p "${workspace}/reports" "${workspace}/summaries"

yarn audit --json > "${workspace}/reports/yarn-audit-${package_name}.jsonl" || true

jq -R -s -r --arg title "Yarn audit: ${package_name}" '
  split("\n")
  | map(select(length > 0) | fromjson?)
  | map(select(.type == "auditAdvisory") | .data.advisory)
  | {
      critical: ([.[] | select(.severity == "critical") | (.github_advisory_id // (.id | tostring))] | unique | length),
      high: ([.[] | select(.severity == "high") | (.github_advisory_id // (.id | tostring))] | unique | length),
      moderate: ([.[] | select(.severity == "moderate") | (.github_advisory_id // (.id | tostring))] | unique | length),
      low: ([.[] | select(.severity == "low") | (.github_advisory_id // (.id | tostring))] | unique | length),
      info: ([.[] | select(.severity == "info") | (.github_advisory_id // (.id | tostring))] | unique | length)
    }
  | "### \($title)\n\n" +
    "| Severity | Unique advisories |\n| --- | ---: |\n" +
    "| CRITICAL | \(.critical) |\n" +
    "| HIGH | \(.high) |\n" +
    "| MODERATE | \(.moderate) |\n" +
    "| LOW | \(.low) |\n" +
    "| INFO | \(.info) |\n"
' "${workspace}/reports/yarn-audit-${package_name}.jsonl" \
  > "${workspace}/summaries/yarn-audit-${package_name}.md"
