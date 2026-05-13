#!/usr/bin/env bash
set -euo pipefail

if [[ -z "${1:-}" ]]; then
  echo "Usage: $0 <module_name>" >&2
  exit 1
fi

module_name="$1"
workspace="${GITHUB_WORKSPACE:-$(pwd)}"
report="${workspace}/reports/govulncheck-${module_name}.json"
metadata="${workspace}/reports/scan-metadata-govulncheck-${module_name}.json"
summary="${workspace}/summaries/govulncheck-${module_name}.md"

command -v go >/dev/null 2>&1 || {
  echo "go is required for Go vulnerability scanning. Install Go and retry." >&2
  exit 1
}
command -v govulncheck >/dev/null 2>&1 || {
  echo "govulncheck is required for Go vulnerability scanning. Install: go install golang.org/x/vuln/cmd/govulncheck@latest" >&2
  exit 1
}
command -v jq >/dev/null 2>&1 || {
  echo "jq is required to summarize govulncheck results. Install jq and retry." >&2
  exit 1
}

mkdir -p "${workspace}/reports" "${workspace}/summaries"

set +e
govulncheck -json ./... > "${report}"
status=$?
set -e

if [[ "${status}" -ne 0 && "${status}" -ne 3 ]]; then
  echo "govulncheck failed for ${module_name} with exit code ${status}" >&2
  exit "${status}"
fi

findings="$(jq -s '[.[] | select(.finding?.osv? != null) | select((.finding.trace[0]?.function // "") != "") | .finding.osv] | unique | length' "${report}")"
package_count="$(go list ./... | wc -l | tr -d ' ')"
module_count="$(go list -m all | wc -l | tr -d ' ')"
tool_version="$(govulncheck -version 2>&1 | awk 'NR == 1 {print $NF}')"

jq -n \
  --arg scanner "Govulncheck" \
  --arg module "${module_name}" \
  --arg tool_version "${tool_version}" \
  --arg report "reports/govulncheck-${module_name}.json" \
  --argjson package_count "${package_count}" \
  --argjson module_count "${module_count}" \
  --argjson findings "${findings}" \
  --argjson exit_code "${status}" \
  '{
    scanner: $scanner,
    target: $module,
    status: "completed",
    tool_version: $tool_version,
    reports: [$report],
    scanned: {
      go_packages: $package_count,
      go_modules: $module_count
    },
    findings: {
      reachable_vulnerabilities: $findings
    },
    diagnostics: {
      exit_code: $exit_code
    }
  }' > "${metadata}"

{
  echo "### Govulncheck: ${module_name}"
  echo
  echo "| Scan coverage | Count |"
  echo "| --- | ---: |"
  echo "| Go packages scanned | ${package_count} |"
  echo "| Go modules resolved | ${module_count} |"
  echo
  echo "| Finding type | Count |"
  echo "| --- | ---: |"
  echo "| Reachable distinct Go vulnerabilities | ${findings} |"
  echo
  echo "Govulncheck does not provide HIGH/CRITICAL severity labels; reachable findings are treated as release-blocking in the policy job."
} > "${summary}"
