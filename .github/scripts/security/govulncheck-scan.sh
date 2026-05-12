#!/usr/bin/env bash
set -euo pipefail

if [[ -z "${1:-}" ]]; then
  echo "Usage: $0 <module_name>" >&2
  exit 1
fi

module_name="$1"
workspace="${GITHUB_WORKSPACE:-$(pwd)}"

mkdir -p "${workspace}/reports" "${workspace}/summaries"

set +e
govulncheck -json ./... > "${workspace}/reports/govulncheck-${module_name}.json"
status=$?
set -e

if [[ "${status}" -ne 0 && "${status}" -ne 3 ]]; then
  echo "govulncheck failed for ${module_name} with exit code ${status}" >&2
  exit "${status}"
fi

findings="$(jq -s '[.[] | select(.finding?.osv? != null) | select((.finding.trace[0]?.function // "") != "") | .finding.osv] | unique | length' "${workspace}/reports/govulncheck-${module_name}.json")"

{
  echo "### Govulncheck: ${module_name}"
  echo
  echo "| Finding type | Count |"
  echo "| --- | ---: |"
  echo "| Reachable distinct Go vulnerabilities | ${findings} |"
  echo
  echo "Govulncheck does not provide HIGH/CRITICAL severity labels; reachable findings are treated as release-blocking in the policy job."
} > "${workspace}/summaries/govulncheck-${module_name}.md"
