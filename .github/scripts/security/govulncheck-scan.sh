#!/usr/bin/env bash
set -euo pipefail

module_name="$1"
workspace="${GITHUB_WORKSPACE:-$(pwd)}"

mkdir -p "${workspace}/reports" "${workspace}/summaries"

govulncheck -json ./... > "${workspace}/reports/govulncheck-${module_name}.json" || true

findings="$(jq -s '[.[] | select(.finding? != null and .finding.osv? != null)] | length' "${workspace}/reports/govulncheck-${module_name}.json")"

{
  echo "### Govulncheck: ${module_name}"
  echo
  echo "| Finding type | Count |"
  echo "| --- | ---: |"
  echo "| Reachable Go vulnerabilities | ${findings} |"
  echo
  echo "Govulncheck does not provide HIGH/CRITICAL severity labels; reachable findings are treated as release-blocking in the policy job."
} > "${workspace}/summaries/govulncheck-${module_name}.md"
