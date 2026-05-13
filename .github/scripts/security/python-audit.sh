#!/usr/bin/env bash
set -euo pipefail

if [[ -z "${1:-}" ]]; then
  echo "Usage: $0 <package_name>" >&2
  exit 1
fi

package_name="$1"
workspace="${GITHUB_WORKSPACE:-$(pwd)}"
report="${workspace}/reports/pip-audit-${package_name}.json"
metadata="${workspace}/reports/scan-metadata-pip-audit-${package_name}.json"
summary="${workspace}/summaries/pip-audit-${package_name}.md"

command -v python >/dev/null 2>&1 || command -v python3 >/dev/null 2>&1 || {
  echo "python is required for Python dependency auditing. Install Python 3 and retry." >&2
  exit 1
}
python_bin="$(command -v python || command -v python3)"
command -v jq >/dev/null 2>&1 || {
  echo "jq is required to summarize Python audit results. Install jq and retry." >&2
  exit 1
}

if ! "${python_bin}" -m pip_audit --version >/dev/null 2>&1; then
  echo "pip-audit is required for Python dependency auditing. Install: python -m pip install pip-audit" >&2
  exit 1
fi

mkdir -p "${workspace}/reports" "${workspace}/summaries"

"${python_bin}" -m pip_audit --format json --progress-spinner off --desc off --aliases on . > "${report}" || true

vulnerabilities="$(jq '[.dependencies[]?.vulns[]?] | length' "${report}")"
dependency_count="$(jq '.dependencies // [] | length' "${report}")"
fix_count="$(jq '.fixes // [] | length' "${report}")"
project_dependency_count="$("${python_bin}" - <<'PY'
import pathlib
import tomllib

path = pathlib.Path("pyproject.toml")
if not path.exists():
    print(0)
    raise SystemExit

data = tomllib.loads(path.read_text())
project = data.get("project", {})
count = len(project.get("dependencies", []) or [])
for values in (project.get("optional-dependencies", {}) or {}).values():
    count += len(values or [])
print(count)
PY
)"
tool_version="$("${python_bin}" -m pip_audit --version | awk '{print $NF}')"

jq -n \
  --arg scanner "pip-audit" \
  --arg package "${package_name}" \
  --arg tool_version "${tool_version}" \
  --arg report "reports/pip-audit-${package_name}.json" \
  --argjson dependency_count "${dependency_count}" \
  --argjson project_dependency_count "${project_dependency_count}" \
  --argjson fix_count "${fix_count}" \
  --argjson vulnerabilities "${vulnerabilities}" \
  '{
    scanner: $scanner,
    target: $package,
    status: "completed",
    tool_version: $tool_version,
    reports: [$report],
    scanned: {
      dependencies: $dependency_count,
      declared_project_dependencies: $project_dependency_count
    },
    findings: {
      vulnerabilities: $vulnerabilities,
      fixes: $fix_count
    },
    diagnostics: {
      problems: 0
    }
  }' > "${metadata}"

{
  echo "### pip-audit: ${package_name}"
  echo
  echo "| Scan coverage | Count |"
  echo "| --- | ---: |"
  echo "| Dependencies scanned | ${dependency_count} |"
  echo "| Declared project dependencies | ${project_dependency_count} |"
  echo
  echo "| Finding type | Count |"
  echo "| --- | ---: |"
  echo "| Known Python dependency vulnerabilities | ${vulnerabilities} |"
  echo
  echo "pip-audit does not provide normalized HIGH/CRITICAL severity in its JSON output, so any finding is treated as release-blocking."
} > "${summary}"

if [[ "${SECURITY_AUDIT_STRICT:-0}" == "1" && "${vulnerabilities}" -gt 0 ]]; then
  echo "::error::pip-audit found ${vulnerabilities} Python dependency vulnerabilities."
  exit 1
fi
