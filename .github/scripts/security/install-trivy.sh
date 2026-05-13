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
mkdir -p "${install_dir}"

checksum_file="trivy_${version}_checksums.txt"
archive_file="trivy_${version}_${os}-${arch}.tar.gz"

if command -v sha256sum >/dev/null 2>&1; then
  hash_cmd=(sha256sum)
elif command -v shasum >/dev/null 2>&1; then
  hash_cmd=(shasum -a 256)
else
  echo "sha256sum or shasum is required to verify Trivy downloads" >&2
  exit 1
fi

curl -fsSL \
  "https://github.com/aquasecurity/trivy/releases/download/v${version}/${archive_file}" \
  -o "${tmp_dir}/${archive_file}"
curl -fsSL \
  "https://github.com/aquasecurity/trivy/releases/download/v${version}/${checksum_file}" \
  -o "${tmp_dir}/${checksum_file}"

grep " ${archive_file}$" "${tmp_dir}/${checksum_file}" > "${tmp_dir}/trivy.sha256"
(cd "${tmp_dir}" && "${hash_cmd[@]}" -c trivy.sha256)

tar -xzf "${tmp_dir}/${archive_file}" -C "${tmp_dir}" trivy
install -m 0755 "${tmp_dir}/trivy" "${install_dir}/trivy"
