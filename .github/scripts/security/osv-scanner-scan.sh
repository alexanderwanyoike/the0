#!/usr/bin/env bash
set -euo pipefail

if [[ -z "${1:-}" || -z "${2:-}" ]]; then
  echo "Usage: $0 <package_name> <lockfile_or_manifest>" >&2
  exit 1
fi

package_name="$1"
scan_file="$2"
workspace="${GITHUB_WORKSPACE:-$(pwd)}"
report="${workspace}/reports/osv-scanner-${package_name}.json"
summary="${workspace}/summaries/osv-scanner-${package_name}.md"

command -v osv-scanner >/dev/null 2>&1 || {
  echo "osv-scanner is required for dependency auditing. Install: go install github.com/google/osv-scanner/v2/cmd/osv-scanner@latest" >&2
  exit 1
}
command -v jq >/dev/null 2>&1 || {
  echo "jq is required to summarize OSV scan results. Install jq and retry." >&2
  exit 1
}

if [[ ! -f "${scan_file}" ]]; then
  echo "OSV scan input not found: ${scan_file}" >&2
  exit 1
fi

mkdir -p "${workspace}/reports" "${workspace}/summaries"

osv-scanner scan source --lockfile "${scan_file}" --format json --output "${report}" || true

critical="$(jq '[.results[]?.packages[]?.vulnerabilities[]? | select((.database_specific.severity // .severity[0]?.score // "" | tostring | ascii_upcase) == "CRITICAL")] | length' "${report}")"
high="$(jq '[.results[]?.packages[]?.vulnerabilities[]? | select((.database_specific.severity // .severity[0]?.score // "" | tostring | ascii_upcase) == "HIGH")] | length' "${report}")"
other="$(jq '[.results[]?.packages[]?.vulnerabilities[]? | select((.database_specific.severity // .severity[0]?.score // "" | tostring | ascii_upcase) != "CRITICAL" and (.database_specific.severity // .severity[0]?.score // "" | tostring | ascii_upcase) != "HIGH")] | length' "${report}")"

{
  echo "### OSV-Scanner: ${package_name}"
  echo
  echo "| Severity | Count |"
  echo "| --- | ---: |"
  echo "| CRITICAL | ${critical} |"
  echo "| HIGH | ${high} |"
  echo "| OTHER/UNKNOWN | ${other} |"
} > "${summary}"

if [[ "${SECURITY_AUDIT_STRICT:-0}" == "1" && $((critical + high)) -gt 0 ]]; then
  echo "::error::OSV-Scanner found ${critical} CRITICAL and ${high} HIGH vulnerabilities."
  exit 1
fi
