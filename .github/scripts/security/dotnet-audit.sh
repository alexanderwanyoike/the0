#!/usr/bin/env bash
set -euo pipefail

if [[ -z "${1:-}" ]]; then
  echo "Usage: $0 <package_name>" >&2
  exit 1
fi

package_name="$1"
workspace="${GITHUB_WORKSPACE:-$(pwd)}"
report="${workspace}/reports/dotnet-audit-${package_name}.json"
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

critical="$(jq '[.. | objects | select(has("severity")) | select(.severity == 3 or (.severity | tostring | ascii_downcase) == "critical")] | length' "${report}")"
high="$(jq '[.. | objects | select(has("severity")) | select(.severity == 2 or (.severity | tostring | ascii_downcase) == "high")] | length' "${report}")"
moderate="$(jq '[.. | objects | select(has("severity")) | select(.severity == 1 or (.severity | tostring | ascii_downcase) == "moderate")] | length' "${report}")"
low="$(jq '[.. | objects | select(has("severity")) | select(.severity == 0 or (.severity | tostring | ascii_downcase) == "low")] | length' "${report}")"

{
  echo "### .NET audit: ${package_name}"
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
