#!/usr/bin/env bash
set -euo pipefail

if [[ -z "${1:-}" || -z "${2:-}" ]]; then
  echo "Usage: $0 <package_name> <source_path>" >&2
  exit 1
fi

package_name="$1"
scan_path="$2"
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

if [[ ! -e "${scan_path}" ]]; then
  echo "OSV scan input not found: ${scan_path}" >&2
  exit 1
fi

mkdir -p "${workspace}/reports" "${workspace}/summaries"

osv-scanner scan source --recursive --no-ignore --format json --output "${report}" "${scan_path}" || true

severity_filter='
  def osv_severity:
    ((.database_specific.severity? | strings | ascii_upcase) // "") as $label
    | if $label == "CRITICAL" or $label == "HIGH" or $label == "MODERATE" or $label == "LOW" then $label
      else ([.severity[]?.score? | tonumber?] | max // -1) as $score
      | if $score >= 9 then "CRITICAL" elif $score >= 7 then "HIGH" else "OTHER" end
      end;
'

critical="$(jq "${severity_filter}"'[.results[]?.packages[]?.vulnerabilities[]? | select(osv_severity == "CRITICAL")] | length' "${report}")"
high="$(jq "${severity_filter}"'[.results[]?.packages[]?.vulnerabilities[]? | select(osv_severity == "HIGH")] | length' "${report}")"
other="$(jq "${severity_filter}"'[.results[]?.packages[]?.vulnerabilities[]? | select(osv_severity != "CRITICAL" and osv_severity != "HIGH")] | length' "${report}")"

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
