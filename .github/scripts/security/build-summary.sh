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
dotnet_critical=0
dotnet_high=0
python_findings=0
cargo_critical=0
cargo_high=0
osv_critical=0
osv_high=0
scan_coverage_rows=""

shopt -s nullglob

trivy_files=("${artifacts_dir}"/reports/trivy-*.json)
if [[ "${#trivy_files[@]}" -gt 0 ]]; then
  trivy_critical="$(jq -s '[.[] | .Results[]? | (.Vulnerabilities[]?, .Misconfigurations[]?, .Secrets[]?) | select((.Severity // "UNKNOWN") == "CRITICAL") | (.VulnerabilityID // .ID // .RuleID // .Title // "unknown")] | unique | length' "${trivy_files[@]}")"
  trivy_high="$(jq -s '[.[] | .Results[]? | (.Vulnerabilities[]?, .Misconfigurations[]?, .Secrets[]?) | select((.Severity // "UNKNOWN") == "HIGH") | (.VulnerabilityID // .ID // .RuleID // .Title // "unknown")] | unique | length' "${trivy_files[@]}")"
fi

yarn_files=("${artifacts_dir}"/reports/yarn-audit-*.jsonl)
if [[ "${#yarn_files[@]}" -gt 0 ]]; then
  yarn_critical="$(jq -R -s '[split("\n")[] | select(length > 0) | fromjson? | select(.type == "auditAdvisory" and .data.advisory.severity == "critical") | (.data.advisory.github_advisory_id // (.data.advisory.id | tostring))] | unique | length' "${yarn_files[@]}")"
  yarn_high="$(jq -R -s '[split("\n")[] | select(length > 0) | fromjson? | select(.type == "auditAdvisory" and .data.advisory.severity == "high") | (.data.advisory.github_advisory_id // (.data.advisory.id | tostring))] | unique | length' "${yarn_files[@]}")"
fi

govuln_files=("${artifacts_dir}"/reports/govulncheck-*.json)
if [[ "${#govuln_files[@]}" -gt 0 ]]; then
  govuln_findings="$(jq -s '[.[] | select(.finding?.osv? != null) | select((.finding.trace[0]?.function // "") != "") | .finding.osv] | unique | length' "${govuln_files[@]}")"
fi

dotnet_files=("${artifacts_dir}"/reports/dotnet-audit-*.json)
if [[ "${#dotnet_files[@]}" -gt 0 ]]; then
  dotnet_critical="$(jq -s '[.[] | .. | objects | select(has("severity")) | select(.severity == 3 or (.severity | tostring | ascii_downcase) == "critical") | (.id // .name // .package // tostring)] | unique | length' "${dotnet_files[@]}")"
  dotnet_high="$(jq -s '[.[] | .. | objects | select(has("severity")) | select(.severity == 2 or (.severity | tostring | ascii_downcase) == "high") | (.id // .name // .package // tostring)] | unique | length' "${dotnet_files[@]}")"
fi

python_files=("${artifacts_dir}"/reports/pip-audit-*.json)
if [[ "${#python_files[@]}" -gt 0 ]]; then
  python_findings="$(jq -s '[.[] | .dependencies[]?.vulns[]? | (.id // .name // tostring)] | unique | length' "${python_files[@]}")"
fi

cargo_files=("${artifacts_dir}"/reports/cargo-audit-*.json)
if [[ "${#cargo_files[@]}" -gt 0 ]]; then
  cargo_critical="$(jq -s '[.[] | .vulnerabilities.list[]? | (.advisory.cvss // 0) as $score | select($score >= 9) | (.advisory.id // .advisory.package // tostring)] | unique | length' "${cargo_files[@]}")"
  cargo_high="$(jq -s '[.[] | .vulnerabilities.list[]? | (.advisory.cvss // 0) as $score | select($score >= 7 and $score < 9) | (.advisory.id // .advisory.package // tostring)] | unique | length' "${cargo_files[@]}")"
fi

osv_files=("${artifacts_dir}"/reports/osv-scanner-*.json)
if [[ "${#osv_files[@]}" -gt 0 ]]; then
  osv_critical="$(jq -s '
    def osv_severity:
      ((.database_specific.severity? | strings | ascii_upcase) // "") as $label
      | if $label == "CRITICAL" or $label == "HIGH" or $label == "MODERATE" or $label == "LOW" then $label
        else ([.severity[]?.score? | tonumber?] | max // -1) as $score
        | if $score >= 9 then "CRITICAL" elif $score >= 7 then "HIGH" else "OTHER" end
        end;
    [.[] | .results[]?.packages[]?.vulnerabilities[]? | select(osv_severity == "CRITICAL") | (.id // .aliases[]? // tostring)] | unique | length
  ' "${osv_files[@]}")"
  osv_high="$(jq -s '
    def osv_severity:
      ((.database_specific.severity? | strings | ascii_upcase) // "") as $label
      | if $label == "CRITICAL" or $label == "HIGH" or $label == "MODERATE" or $label == "LOW" then $label
        else ([.severity[]?.score? | tonumber?] | max // -1) as $score
        | if $score >= 9 then "CRITICAL" elif $score >= 7 then "HIGH" else "OTHER" end
        end;
    [.[] | .results[]?.packages[]?.vulnerabilities[]? | select(osv_severity == "HIGH") | (.id // .aliases[]? // tostring)] | unique | length
  ' "${osv_files[@]}")"
fi

metadata_files=("${artifacts_dir}"/reports/scan-metadata-*.json)
if [[ "${#metadata_files[@]}" -gt 0 ]]; then
  scan_coverage_rows="$(jq -s -r '
    def field_label:
      gsub("_"; " ");
    def kv_rows:
      to_entries
      | map("\(.key | field_label): \(.value)")
      | join("<br>");
    sort_by(.scanner, .target)
    | .[]
    | "| \(.scanner): \(.target) | \((.scanned // {}) | kv_rows) | \((.diagnostics // {}) | kv_rows) |"
  ' "${metadata_files[@]}")"
fi

release_blockers=$((trivy_critical + trivy_high + yarn_critical + yarn_high + govuln_findings + dotnet_critical + dotnet_high + python_findings + cargo_critical + cargo_high + osv_critical + osv_high))

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
  echo "| Scanner | Unique release-blocking findings |"
  echo "| --- | ---: |"
  echo "| Trivy CRITICAL | ${trivy_critical} |"
  echo "| Trivy HIGH | ${trivy_high} |"
  echo "| Yarn audit CRITICAL | ${yarn_critical} |"
  echo "| Yarn audit HIGH | ${yarn_high} |"
  echo "| Govulncheck reachable distinct vulnerabilities | ${govuln_findings} |"
  echo "| .NET audit CRITICAL | ${dotnet_critical} |"
  echo "| .NET audit HIGH | ${dotnet_high} |"
  echo "| pip-audit known vulnerabilities | ${python_findings} |"
  echo "| cargo-audit CRITICAL | ${cargo_critical} |"
  echo "| cargo-audit HIGH | ${cargo_high} |"
  echo "| OSV-Scanner CRITICAL | ${osv_critical} |"
  echo "| OSV-Scanner HIGH | ${osv_high} |"
  echo "| **Total release blockers** | **${release_blockers}** |"
  echo
  if [[ -n "${scan_coverage_rows}" ]]; then
    echo "### Scan coverage"
    echo
    echo "| Scanner | Coverage | Diagnostics |"
    echo "| --- | --- | --- |"
    echo "${scan_coverage_rows}"
    echo
  fi
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
