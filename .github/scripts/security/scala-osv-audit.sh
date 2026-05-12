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
command -v python3 >/dev/null 2>&1 || {
  echo "python3 is required to count Scala dependencies before OSV scanning. Install Python 3 and retry." >&2
  exit 1
}

sbt makePom
pom_file="$(find target -name "*.pom" -type f | head -n 1)"
if [[ -z "${pom_file}" ]]; then
  echo "sbt makePom did not generate a pom.xml file under target/." >&2
  exit 1
fi

scan_dir="target/security-osv"
mkdir -p "${scan_dir}"
cp "${pom_file}" "${scan_dir}/pom.xml"

dependency_count="$(python3 - "${scan_dir}/pom.xml" <<'PY'
import sys
import xml.etree.ElementTree as ET

path = sys.argv[1]
root = ET.parse(path).getroot()

def local_name(tag):
    return tag.rsplit("}", 1)[-1]

count = 0
for element in root.iter():
    if local_name(element.tag) != "dependency":
        continue
    if any(local_name(child.tag) == "groupId" for child in element):
        count += 1

print(count)
PY
)"
export OSV_SCAN_PACKAGE_COUNT="${dependency_count}"
export OSV_SCAN_SOURCE_FILE="${scan_dir}/pom.xml"

bash "${script_dir}/osv-scanner-scan.sh" "${package_name}" "${scan_dir}"
