#!/usr/bin/env bash
set -euo pipefail

if [[ -z "${1:-}" ]]; then
  echo "Usage: $0 <package_name>" >&2
  exit 1
fi

package_name="$1"
script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

command -v sbt >/dev/null 2>&1 || {
  echo "sbt is required for Scala dependency auditing. Install: https://www.scala-sbt.org/download/" >&2
  exit 1
}

sbt makePom
pom_file="$(find target -name "*.pom" -type f | head -n 1)"
if [[ -z "${pom_file}" ]]; then
  echo "sbt makePom did not generate a pom.xml file under target/." >&2
  exit 1
fi

bash "${script_dir}/osv-scanner-scan.sh" "${package_name}" "${pom_file}"
