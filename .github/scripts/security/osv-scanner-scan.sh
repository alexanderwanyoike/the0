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
metadata="${workspace}/reports/scan-metadata-osv-scanner-${package_name}.json"
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

osv-scanner scan source --recursive --no-ignore --all-packages --format json --output "${report}" "${scan_path}" || true

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
reported_package_count="$(jq '[.results[]?.packages[]? | .package.name? // empty] | unique | length' "${report}")"
vulnerable_package_count="$(jq '[.results[]?.packages[]? | select((.vulnerabilities // []) | length > 0) | .package.name? // empty] | unique | length' "${report}")"
package_count="${reported_package_count}"
if [[ "${package_count}" -eq 0 && -n "${OSV_SCAN_PACKAGE_COUNT:-}" ]]; then
  package_count="${OSV_SCAN_PACKAGE_COUNT}"
fi
source_file="${OSV_SCAN_SOURCE_FILE:-${scan_path}}"
tool_version="$(osv-scanner --version | awk '{print $NF}')"

jq -n \
  --arg scanner "OSV-Scanner" \
  --arg package "${package_name}" \
  --arg tool_version "${tool_version}" \
  --arg report "reports/osv-scanner-${package_name}.json" \
  --arg source_file "${source_file}" \
  --argjson package_count "${package_count}" \
  --argjson vulnerable_package_count "${vulnerable_package_count}" \
  --argjson critical "${critical}" \
  --argjson high "${high}" \
  --argjson other "${other}" \
  '{
    scanner: $scanner,
    target: $package,
    status: "completed",
    tool_version: $tool_version,
    reports: [$report],
    source: $source_file,
    scanned: {
      packages: $package_count,
      packages_with_vulnerabilities: $vulnerable_package_count
    },
    findings: {
      critical: $critical,
      high: $high,
      other: $other
    },
    diagnostics: {
      problems: 0
    }
  }' > "${metadata}"

{
  echo "### OSV-Scanner: ${package_name}"
  echo
  echo "| Scan coverage | Count |"
  echo "| --- | ---: |"
  echo "| Packages scanned | ${package_count} |"
  echo "| Packages with vulnerabilities | ${vulnerable_package_count} |"
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
