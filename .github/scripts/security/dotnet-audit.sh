#!/usr/bin/env bash
set -euo pipefail

if [[ -z "${1:-}" ]]; then
  echo "Usage: $0 <package_name>" >&2
  exit 1
fi

package_name="$1"
workspace="${GITHUB_WORKSPACE:-$(pwd)}"
report="${workspace}/reports/dotnet-audit-${package_name}.json"
all_packages_report="${workspace}/reports/dotnet-packages-${package_name}.json"
metadata="${workspace}/reports/scan-metadata-dotnet-audit-${package_name}.json"
summary="${workspace}/summaries/dotnet-audit-${package_name}.md"

command -v dotnet >/dev/null 2>&1 || {
  echo "dotnet is required for .NET dependency auditing. Install: https://dotnet.microsoft.com/download" >&2
  exit 1
}
command -v jq >/dev/null 2>&1 || {
  echo "jq is required to summarize .NET audit results. Install jq and retry." >&2
  exit 1
}

mkdir -p "${workspace}/reports" "${workspace}/summaries"

dotnet list package --vulnerable --include-transitive --format json > "${report}" || true
dotnet list package --include-transitive --format json > "${all_packages_report}" || true

critical="$(jq '[.. | objects | select(has("severity")) | select(.severity == 3 or (.severity | tostring | ascii_downcase) == "critical")] | length' "${report}")"
high="$(jq '[.. | objects | select(has("severity")) | select(.severity == 2 or (.severity | tostring | ascii_downcase) == "high")] | length' "${report}")"
moderate="$(jq '[.. | objects | select(has("severity")) | select(.severity == 1 or (.severity | tostring | ascii_downcase) == "moderate")] | length' "${report}")"
low="$(jq '[.. | objects | select(has("severity")) | select(.severity == 0 or (.severity | tostring | ascii_downcase) == "low")] | length' "${report}")"
project_count="$(jq '.projects // [] | length' "${report}")"
if jq empty "${all_packages_report}" >/dev/null 2>&1; then
  package_count="$(jq '[.projects[]?.frameworks[]? | (.topLevelPackages[]?, .transitivePackages[]?) | (.id // .name // empty)] | unique | length' "${all_packages_report}")"
else
  package_count=0
fi
problem_count="$(jq '.problems // [] | length' "${report}")"
tool_version="$(dotnet --version)"

jq -n \
  --arg scanner ".NET audit" \
  --arg package "${package_name}" \
  --arg tool_version "${tool_version}" \
  --arg report "reports/dotnet-audit-${package_name}.json" \
  --arg all_packages_report "reports/dotnet-packages-${package_name}.json" \
  --argjson project_count "${project_count}" \
  --argjson package_count "${package_count}" \
  --argjson problem_count "${problem_count}" \
  --argjson critical "${critical}" \
  --argjson high "${high}" \
  --argjson moderate "${moderate}" \
  --argjson low "${low}" \
  '{
    scanner: $scanner,
    target: $package,
    status: "completed",
    tool_version: $tool_version,
    reports: [$report, $all_packages_report],
    scanned: {
      projects: $project_count,
      packages: $package_count
    },
    findings: {
      critical: $critical,
      high: $high,
      moderate: $moderate,
      low: $low
    },
    diagnostics: {
      problems: $problem_count
    }
  }' > "${metadata}"

{
  echo "### .NET audit: ${package_name}"
  echo
  echo "| Scan coverage | Count |"
  echo "| --- | ---: |"
  echo "| Projects scanned | ${project_count} |"
  echo "| Packages resolved | ${package_count} |"
  echo "| Scanner problems | ${problem_count} |"
  echo
  echo "| Severity | Count |"
  echo "| --- | ---: |"
  echo "| CRITICAL | ${critical} |"
  echo "| HIGH | ${high} |"
  echo "| MODERATE | ${moderate} |"
  echo "| LOW | ${low} |"
} > "${summary}"

if [[ "${SECURITY_AUDIT_STRICT:-0}" == "1" && $((critical + high)) -gt 0 ]]; then
  echo "::error::.NET audit found ${critical} CRITICAL and ${high} HIGH vulnerabilities."
  exit 1
fi
