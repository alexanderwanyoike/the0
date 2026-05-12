#!/usr/bin/env bash
set -euo pipefail

version="${TRIVY_VERSION:-0.58.1}"

curl -sfL https://raw.githubusercontent.com/aquasecurity/trivy/main/contrib/install.sh \
  | sh -s -- -b /usr/local/bin "v${version}"
