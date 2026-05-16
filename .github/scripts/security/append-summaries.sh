#!/usr/bin/env bash
set -euo pipefail

artifacts_dir="${1:-security-artifacts}"

if [[ -z "${GITHUB_STEP_SUMMARY:-}" ]]; then
  exit 0
fi

shopt -s nullglob
for file in "${artifacts_dir}"/summaries/*.md; do
  {
    echo
    cat "${file}"
  } >> "${GITHUB_STEP_SUMMARY}"
done
