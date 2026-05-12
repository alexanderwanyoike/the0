#!/usr/bin/env bash
set -euo pipefail

version="${TRIVY_VERSION:-0.70.0}"
install_dir="${TRIVY_INSTALL_DIR:-/usr/local/bin}"
os="$(uname -s)"
arch="$(uname -m)"
tmp_dir="$(mktemp -d)"

case "${os}" in
  Linux) os=Linux ;;
  Darwin) os=macOS ;;
  *) echo "Unsupported OS for Trivy install: ${os}" >&2; exit 1 ;;
esac

case "${arch}" in
  x86_64 | amd64) arch=64bit ;;
  aarch64 | arm64) arch=ARM64 ;;
  *) echo "Unsupported architecture for Trivy install: ${arch}" >&2; exit 1 ;;
esac

trap 'rm -rf "${tmp_dir}"' EXIT

curl -fsSL \
  "https://github.com/aquasecurity/trivy/releases/download/v${version}/trivy_${version}_${os}-${arch}.tar.gz" \
  -o "${tmp_dir}/trivy.tar.gz"
tar -xzf "${tmp_dir}/trivy.tar.gz" -C "${tmp_dir}" trivy
install -m 0755 "${tmp_dir}/trivy" "${install_dir}/trivy"
