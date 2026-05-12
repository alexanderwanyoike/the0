#!/usr/bin/env bash
set -euo pipefail

event_name="$1"
base_ref="$2"
git_ref="$3"
artifacts_dir="${4:-security-artifacts}"
summary_file="${5:-security-summary.md}"

is_release_bound=false
if [[ "${event_name}" == "pull_request" && "${base_ref}" == "main" ]]; then
  is_release_bound=true
elif [[ "${event_name}" == "push" && "${git_ref}" == "refs/heads/main" ]]; then
  is_release_bound=true
elif [[ "${event_name}" == "push" && "${git_ref}" == refs/tags/v* ]]; then
  is_release_bound=true
fi

trivy_critical=0
trivy_high=0
yarn_critical=0
yarn_high=0
govuln_findings=0

shopt -s nullglob
for file in "${artifacts_dir}"/reports/trivy-*.json; do
  critical="$(jq '[.Results[]? | (.Vulnerabilities[]?, .Misconfigurations[]?, .Secrets[]?) | select((.Severity // "UNKNOWN") == "CRITICAL")] | length' "${file}")"
  high="$(jq '[.Results[]? | (.Vulnerabilities[]?, .Misconfigurations[]?, .Secrets[]?) | select((.Severity // "UNKNOWN") == "HIGH")] | length' "${file}")"
  trivy_critical=$((trivy_critical + critical))
  trivy_high=$((trivy_high + high))
done

for file in "${artifacts_dir}"/reports/yarn-audit-*.jsonl; do
  critical="$(jq -R -s '[split("\n")[] | select(length > 0) | fromjson? | select(.type == "auditAdvisory" and .data.advisory.severity == "critical")] | length' "${file}")"
  high="$(jq -R -s '[split("\n")[] | select(length > 0) | fromjson? | select(.type == "auditAdvisory" and .data.advisory.severity == "high")] | length' "${file}")"
  yarn_critical=$((yarn_critical + critical))
  yarn_high=$((yarn_high + high))
done

for file in "${artifacts_dir}"/reports/govulncheck-*.json; do
  findings="$(jq -s '[.[] | select(.finding? != null and .finding.osv? != null)] | length' "${file}")"
  govuln_findings=$((govuln_findings + findings))
done

release_blockers=$((trivy_critical + trivy_high + yarn_critical + yarn_high + govuln_findings))

{
  echo "<!-- the0-security-summary -->"
  echo "## Security scan summary"
  echo
  if [[ "${is_release_bound}" == "true" ]]; then
    echo "**Policy:** release-bound target; HIGH/CRITICAL findings and reachable Go vulnerabilities block this workflow."
  else
    echo "**Policy:** visibility-only target; findings are reported here and would block release if this targeted \`main\` or a release tag."
  fi
  echo
  echo "| Scanner | Release-blocking findings |"
  echo "| --- | ---: |"
  echo "| Trivy CRITICAL | ${trivy_critical} |"
  echo "| Trivy HIGH | ${trivy_high} |"
  echo "| Yarn audit CRITICAL | ${yarn_critical} |"
  echo "| Yarn audit HIGH | ${yarn_high} |"
  echo "| Govulncheck reachable findings | ${govuln_findings} |"
  echo "| **Total release blockers** | **${release_blockers}** |"
  echo
  echo "Full JSON reports and compact per-scan summaries are attached as workflow artifacts."
  echo
  if [[ "${release_blockers}" -gt 0 ]]; then
    if [[ "${is_release_bound}" == "true" ]]; then
      echo "**Result:** release blocked."
    else
      echo "**Result:** would block release, but this workflow remains green for this target."
    fi
  else
    echo "**Result:** no release-blocking findings detected."
  fi
} > "${summary_file}"

if [[ -n "${GITHUB_STEP_SUMMARY:-}" ]]; then
  cat "${summary_file}" >> "${GITHUB_STEP_SUMMARY}"
fi

if [[ -n "${GITHUB_OUTPUT:-}" ]]; then
  {
    echo "is_release_bound=${is_release_bound}"
    echo "release_blockers=${release_blockers}"
  } >> "${GITHUB_OUTPUT}"
fi
