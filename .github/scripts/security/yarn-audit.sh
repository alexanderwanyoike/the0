#!/usr/bin/env bash
set -euo pipefail

package_name="$1"
workspace="${GITHUB_WORKSPACE:-$(pwd)}"

mkdir -p "${workspace}/reports" "${workspace}/summaries"

yarn audit --json > "${workspace}/reports/yarn-audit-${package_name}.jsonl" || true

jq -R -s -r --arg title "Yarn audit: ${package_name}" '
  split("\n")
  | map(select(length > 0) | fromjson?)
  | {
      critical: ([.[] | select(.type == "auditAdvisory" and .data.advisory.severity == "critical")] | length),
      high: ([.[] | select(.type == "auditAdvisory" and .data.advisory.severity == "high")] | length),
      moderate: ([.[] | select(.type == "auditAdvisory" and .data.advisory.severity == "moderate")] | length),
      low: ([.[] | select(.type == "auditAdvisory" and .data.advisory.severity == "low")] | length),
      info: ([.[] | select(.type == "auditAdvisory" and .data.advisory.severity == "info")] | length)
    }
  | "### \($title)\n\n" +
    "| Severity | Count |\n| --- | ---: |\n" +
    "| CRITICAL | \(.critical) |\n" +
    "| HIGH | \(.high) |\n" +
    "| MODERATE | \(.moderate) |\n" +
    "| LOW | \(.low) |\n" +
    "| INFO | \(.info) |\n"
' "${workspace}/reports/yarn-audit-${package_name}.jsonl" \
  > "${workspace}/summaries/yarn-audit-${package_name}.md"
