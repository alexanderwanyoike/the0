#!/usr/bin/env bash
set -euo pipefail

if [[ -z "${1:-}" ]]; then
  echo "Usage: $0 <package_name>" >&2
  exit 1
fi

package_name="$1"
workspace="${GITHUB_WORKSPACE:-$(pwd)}"
report="${workspace}/reports/cargo-audit-${package_name}.json"
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

{
  echo "### cargo-audit: ${package_name}"
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
