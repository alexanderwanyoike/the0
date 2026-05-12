#!/usr/bin/env bash
set -euo pipefail

event_name="${GITHUB_EVENT_NAME:-}"
git_ref="${GITHUB_REF:-}"
before_sha="${GITHUB_EVENT_BEFORE:-}"
base_sha="${GITHUB_EVENT_PULL_REQUEST_BASE_SHA:-}"
head_sha="${GITHUB_SHA:-}"

if [[ -z "${GITHUB_OUTPUT:-}" ]]; then
  echo "GITHUB_OUTPUT is required." >&2
  exit 1
fi

set_all=false
changed_files=""

if [[ "${event_name}" == "workflow_dispatch" || "${git_ref}" == refs/tags/v* ]]; then
  set_all=true
elif [[ "${event_name}" == "pull_request" ]]; then
  if [[ -z "${base_sha}" || -z "${head_sha}" ]]; then
    echo "Pull request base/head SHAs are required for change detection." >&2
    exit 1
  fi
  changed_files="$(git diff --name-only "${base_sha}" "${head_sha}")"
elif [[ "${event_name}" == "push" ]]; then
  if [[ -n "${before_sha}" && "${before_sha}" != "0000000000000000000000000000000000000000" ]]; then
    changed_files="$(git diff --name-only "${before_sha}" "${head_sha}")"
  else
    changed_files="$(git diff-tree --no-commit-id --name-only -r "${head_sha}")"
  fi
else
  changed_files="$(git diff-tree --no-commit-id --name-only -r "${head_sha}")"
fi

if [[ "${set_all}" != "true" ]] && grep -Eq '^(\.github/workflows/security\.yml|\.github/scripts/security/)' <<< "${changed_files}"; then
  set_all=true
fi

changed_any=false
if [[ -n "${changed_files}" ]]; then
  changed_any=true
fi

matches_any() {
  local regex="$1"
  [[ "${set_all}" == "true" ]] || grep -Eq "${regex}" <<< "${changed_files}"
}

write_bool() {
  local name="$1"
  local value="$2"
  printf '%s=%s\n' "${name}" "${value}" >> "${GITHUB_OUTPUT}"
}

target_bool() {
  local name="$1"
  local regex="$2"
  if matches_any "${regex}"; then
    write_bool "${name}" true
  else
    write_bool "${name}" false
  fi
}

write_bool all "${set_all}"
write_bool any_changed "${changed_any}"
write_bool trivy_repo true

target_bool api '^api/'
target_bool frontend '^frontend/'
target_bool runtime '^runtime/'
target_bool docs '^docs/'
target_bool cli '^cli/'
target_bool sdk_nodejs '^sdk/nodejs/'
target_bool sdk_react '^sdk/react/'
target_bool sdk_dotnet '^sdk/dotnet/'
target_bool sdk_python '^sdk/python/'
target_bool sdk_rust '^sdk/rust/'
target_bool sdk_scala '^sdk/scala/'

{
  echo "changed_files<<EOF"
  echo "${changed_files}"
  echo "EOF"
} >> "${GITHUB_OUTPUT}"
