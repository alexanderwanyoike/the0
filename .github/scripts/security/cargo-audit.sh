#!/usr/bin/env bash
set -euo pipefail

if [[ -z "${1:-}" ]]; then
  echo "Usage: $0 <package_name>" >&2
  exit 1
fi

package_name="$1"
workspace="${GITHUB_WORKSPACE:-$(pwd)}"
report="${workspace}/reports/cargo-audit-${package_name}.json"
metadata="${workspace}/reports/scan-metadata-cargo-audit-${package_name}.json"
summary="${workspace}/summaries/cargo-audit-${package_name}.md"

command -v cargo >/dev/null 2>&1 || {
  echo "cargo is required for Rust dependency auditing. Install Rust: https://rustup.rs/" >&2
  exit 1
}
command -v cargo-audit >/dev/null 2>&1 || {
  echo "cargo-audit is required for Rust dependency auditing. Install: cargo install cargo-audit --locked" >&2
  exit 1
}
command -v jq >/dev/null 2>&1 || {
  echo "jq is required to summarize Rust audit results. Install jq and retry." >&2
  exit 1
}

mkdir -p "${workspace}/reports" "${workspace}/summaries"

if [[ ! -f Cargo.lock ]]; then
  cargo generate-lockfile
fi

cargo audit --json > "${report}" || true

critical="$(jq '[.vulnerabilities.list[]? | (.advisory.cvss // 0) as $score | select($score >= 9)] | length' "${report}")"
high="$(jq '[.vulnerabilities.list[]? | (.advisory.cvss // 0) as $score | select($score >= 7 and $score < 9)] | length' "${report}")"
other="$(jq '[.vulnerabilities.list[]? | (.advisory.cvss // 0) as $score | select($score < 7)] | length' "${report}")"
dependency_count="$(jq '.lockfile["dependency-count"] // 0' "${report}")"
warning_count="$(jq '[.warnings | objects | to_entries[]?] | length' "${report}")"
tool_version="$(cargo audit --version | awk '{print $2}')"

jq -n \
  --arg scanner "cargo-audit" \
  --arg package "${package_name}" \
  --arg tool_version "${tool_version}" \
  --arg report "reports/cargo-audit-${package_name}.json" \
  --argjson dependency_count "${dependency_count}" \
  --argjson warning_count "${warning_count}" \
  --argjson critical "${critical}" \
  --argjson high "${high}" \
  --argjson other "${other}" \
  '{
    scanner: $scanner,
    target: $package,
    status: "completed",
    tool_version: $tool_version,
    reports: [$report],
    scanned: {
      dependencies: $dependency_count
    },
    findings: {
      critical: $critical,
      high: $high,
      other: $other
    },
    diagnostics: {
      warnings: $warning_count
    }
  }' > "${metadata}"

{
  echo "### cargo-audit: ${package_name}"
  echo
  echo "| Scan coverage | Count |"
  echo "| --- | ---: |"
  echo "| Dependencies scanned | ${dependency_count} |"
  echo "| Scanner warnings | ${warning_count} |"
  echo
  echo "| Severity | Count |"
  echo "| --- | ---: |"
  echo "| CRITICAL | ${critical} |"
  echo "| HIGH | ${high} |"
  echo "| OTHER | ${other} |"
} > "${summary}"

if [[ "${SECURITY_AUDIT_STRICT:-0}" == "1" && $((critical + high)) -gt 0 ]]; then
  echo "::error::cargo-audit found ${critical} CRITICAL and ${high} HIGH vulnerabilities."
  exit 1
fi
