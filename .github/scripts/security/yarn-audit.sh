#!/usr/bin/env bash
set -euo pipefail

if [[ -z "${1:-}" ]]; then
  echo "Usage: $0 <package_name>" >&2
  exit 1
fi

package_name="$1"
workspace="${GITHUB_WORKSPACE:-$(pwd)}"
report="${workspace}/reports/yarn-audit-${package_name}.jsonl"
metadata="${workspace}/reports/scan-metadata-yarn-audit-${package_name}.json"
summary="${workspace}/summaries/yarn-audit-${package_name}.md"

mkdir -p "${workspace}/reports" "${workspace}/summaries"

yarn audit --json > "${report}" || true

critical="$(jq -R -s '[split("\n")[] | select(length > 0) | fromjson? | select(.type == "auditAdvisory" and .data.advisory.severity == "critical") | (.data.advisory.github_advisory_id // (.data.advisory.id | tostring))] | unique | length' "${report}")"
high="$(jq -R -s '[split("\n")[] | select(length > 0) | fromjson? | select(.type == "auditAdvisory" and .data.advisory.severity == "high") | (.data.advisory.github_advisory_id // (.data.advisory.id | tostring))] | unique | length' "${report}")"
moderate="$(jq -R -s '[split("\n")[] | select(length > 0) | fromjson? | select(.type == "auditAdvisory" and .data.advisory.severity == "moderate") | (.data.advisory.github_advisory_id // (.data.advisory.id | tostring))] | unique | length' "${report}")"
low="$(jq -R -s '[split("\n")[] | select(length > 0) | fromjson? | select(.type == "auditAdvisory" and .data.advisory.severity == "low") | (.data.advisory.github_advisory_id // (.data.advisory.id | tostring))] | unique | length' "${report}")"
info="$(jq -R -s '[split("\n")[] | select(length > 0) | fromjson? | select(.type == "auditAdvisory" and .data.advisory.severity == "info") | (.data.advisory.github_advisory_id // (.data.advisory.id | tostring))] | unique | length' "${report}")"
total_dependencies="$(jq -R -s '[split("\n")[] | select(length > 0) | fromjson? | select(.type == "auditSummary") | .data.totalDependencies] | last // 0' "${report}")"
production_dependencies="$(jq -R -s '[split("\n")[] | select(length > 0) | fromjson? | select(.type == "auditSummary") | .data.dependencies] | last // 0' "${report}")"
dev_dependencies="$(jq -R -s '[split("\n")[] | select(length > 0) | fromjson? | select(.type == "auditSummary") | .data.devDependencies] | last // 0' "${report}")"
optional_dependencies="$(jq -R -s '[split("\n")[] | select(length > 0) | fromjson? | select(.type == "auditSummary") | .data.optionalDependencies] | last // 0' "${report}")"
declared_dependencies="$(jq '[
  (.dependencies // {} | keys[]?),
  (.devDependencies // {} | keys[]?),
  (.optionalDependencies // {} | keys[]?),
  (.peerDependencies // {} | keys[]?)
] | unique | length' package.json)"
tool_version="$(yarn --version)"

jq -n \
  --arg scanner "Yarn audit" \
  --arg package "${package_name}" \
  --arg tool_version "${tool_version}" \
  --arg report "reports/yarn-audit-${package_name}.jsonl" \
  --argjson total_dependencies "${total_dependencies}" \
  --argjson production_dependencies "${production_dependencies}" \
  --argjson dev_dependencies "${dev_dependencies}" \
  --argjson optional_dependencies "${optional_dependencies}" \
  --argjson declared_dependencies "${declared_dependencies}" \
  --argjson critical "${critical}" \
  --argjson high "${high}" \
  --argjson moderate "${moderate}" \
  --argjson low "${low}" \
  --argjson info "${info}" \
  '{
    scanner: $scanner,
    target: $package,
    status: "completed",
    tool_version: $tool_version,
    reports: [$report],
    scanned: {
      total_dependencies: $total_dependencies,
      production_dependencies: $production_dependencies,
      dev_dependencies: $dev_dependencies,
      optional_dependencies: $optional_dependencies,
      declared_dependencies: $declared_dependencies
    },
    findings: {
      critical: $critical,
      high: $high,
      moderate: $moderate,
      low: $low,
      info: $info
    },
    diagnostics: {
      problems: 0
    }
  }' > "${metadata}"

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
' "${report}" > "${summary}"
