#!/bin/sh
# the0 CLI installer
# Usage: curl -sSL https://install.the0.dev | sh
set -eu

GITHUB_REPO="alexanderwanyoike/the0"
INSTALL_DIR="$HOME/.the0/bin"
BINARY_NAME="the0"
WORK_DIR=""

cleanup() {
    if [ -n "$WORK_DIR" ] && [ -d "$WORK_DIR" ]; then
        rm -rf "$WORK_DIR"
    fi
    if [ -n "${TMPFILE:-}" ] && [ -f "$TMPFILE" ]; then
        rm -f "$TMPFILE"
    fi
}

trap cleanup EXIT INT TERM

# Colors (only when outputting to a terminal)
setup_colors() {
    if [ -t 1 ]; then
        RED='\033[0;31m'
        GREEN='\033[0;32m'
        YELLOW='\033[0;33m'
        BLUE='\033[0;34m'
        BOLD='\033[1m'
        RESET='\033[0m'
    else
        RED=''
        GREEN=''
        YELLOW=''
        BLUE=''
        BOLD=''
        RESET=''
    fi
}

info() {
    printf "${BLUE}==>${RESET} ${BOLD}%s${RESET}\n" "$1"
}

success() {
    printf "${GREEN}==>${RESET} ${BOLD}%s${RESET}\n" "$1"
}

warn() {
    printf "${YELLOW}warning:${RESET} %s\n" "$1"
}

error() {
    printf "${RED}error:${RESET} %s\n" "$1" >&2
    exit 1
}

check_dependencies() {
    # Check for curl or wget
    if command -v curl >/dev/null 2>&1; then
        DOWNLOADER="curl"
    elif command -v wget >/dev/null 2>&1; then
        DOWNLOADER="wget"
    else
        error "curl or wget is required but neither was found"
    fi

    # Check for sha256sum or shasum
    if command -v sha256sum >/dev/null 2>&1; then
        SHA_CMD="sha256sum"
    elif command -v shasum >/dev/null 2>&1; then
        SHA_CMD="shasum -a 256"
    else
        error "sha256sum or shasum is required but neither was found"
    fi
}

detect_platform() {
    OS="$(uname -s)"
    ARCH="$(uname -m)"

    case "$OS" in
        Linux)  OS="linux" ;;
        Darwin) OS="darwin" ;;
        *)      error "Unsupported operating system: $OS" ;;
    esac

    case "$ARCH" in
        x86_64|amd64)   ARCH="amd64" ;;
        aarch64|arm64)  ARCH="arm64" ;;
        *)              error "Unsupported architecture: $ARCH" ;;
    esac

    info "Detected platform: ${OS}-${ARCH}"
}

download() {
    url="$1"
    output="$2"

    if [ "$DOWNLOADER" = "curl" ]; then
        curl -fsSL -o "$output" "$url"
    else
        wget -q -O "$output" "$url"
    fi
}

get_latest_version() {
    info "Fetching latest release..."

    TMPFILE="$(mktemp)"
    url="https://api.github.com/repos/${GITHUB_REPO}/releases/latest"

    if ! download "$url" "$TMPFILE"; then
        rm -f "$TMPFILE"
        error "Failed to fetch latest release from GitHub"
    fi

    # Extract tag_name without requiring jq
    TAG="$(sed -n 's/.*"tag_name"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p' "$TMPFILE" | head -1)"
    rm -f "$TMPFILE"

    if [ -z "$TAG" ]; then
        error "Could not determine latest release version"
    fi

    info "Latest version: ${TAG}"
}

download_binary() {
    WORK_DIR="$(mktemp -d)"
    BINARY_URL="https://github.com/${GITHUB_REPO}/releases/download/${TAG}/${BINARY_NAME}-${OS}-${ARCH}"
    CHECKSUMS_URL="https://github.com/${GITHUB_REPO}/releases/download/${TAG}/checksums.txt"

    info "Downloading ${BINARY_NAME}-${OS}-${ARCH}..."
    if ! download "$BINARY_URL" "${WORK_DIR}/${BINARY_NAME}"; then
        error "Failed to download binary from ${BINARY_URL}"
    fi

    info "Downloading checksums..."
    if ! download "$CHECKSUMS_URL" "${WORK_DIR}/checksums.txt"; then
        error "Failed to download checksums from ${CHECKSUMS_URL}"
    fi
}

verify_checksum() {
    info "Verifying checksum..."

    EXPECTED="$(grep "  ${BINARY_NAME}-${OS}-${ARCH}$" "${WORK_DIR}/checksums.txt" | awk '{print $1}')"
    if [ -z "$EXPECTED" ]; then
        error "No checksum found for ${BINARY_NAME}-${OS}-${ARCH} in checksums.txt"
    fi

    ACTUAL="$(cd "$WORK_DIR" && $SHA_CMD "$BINARY_NAME" | awk '{print $1}')"

    if [ "$EXPECTED" != "$ACTUAL" ]; then
        error "Checksum verification failed (expected: ${EXPECTED}, got: ${ACTUAL})"
    fi

    success "Checksum verified"
}

install_binary() {
    info "Installing to ${INSTALL_DIR}/${BINARY_NAME}..."

    mkdir -p "$INSTALL_DIR"
    mv "${WORK_DIR}/${BINARY_NAME}" "${INSTALL_DIR}/${BINARY_NAME}"
    chmod +x "${INSTALL_DIR}/${BINARY_NAME}"

    success "Installed ${BINARY_NAME} to ${INSTALL_DIR}/${BINARY_NAME}"
}

setup_path() {
    # Check if already in PATH
    case ":$PATH:" in
        *":${INSTALL_DIR}:"*)
            return
            ;;
    esac

    EXPORT_LINE="export PATH=\"\$HOME/.the0/bin:\$PATH\""
    MODIFIED_FILES=""
    ALREADY_CONFIGURED=""

    append_to_file() {
        file="$1"
        if [ -f "$file" ]; then
            # Check if already present
            if ! grep -qF '.the0/bin' "$file" 2>/dev/null; then
                printf '\n# the0 CLI\n%s\n' "$EXPORT_LINE" >> "$file"
                MODIFIED_FILES="${MODIFIED_FILES} ${file}"
            else
                ALREADY_CONFIGURED="yes"
            fi
        fi
    }

    append_to_file "$HOME/.bashrc"
    append_to_file "$HOME/.zshrc"

    # Fallback to .profile if neither .bashrc nor .zshrc were modified or already configured
    if [ -z "$MODIFIED_FILES" ] && [ -z "$ALREADY_CONFIGURED" ]; then
        if [ -f "$HOME/.profile" ]; then
            if ! grep -qF '.the0/bin' "$HOME/.profile" 2>/dev/null; then
                printf '\n# the0 CLI\n%s\n' "$EXPORT_LINE" >> "$HOME/.profile"
                MODIFIED_FILES=" $HOME/.profile"
            fi
        fi
    fi

    if [ -n "$MODIFIED_FILES" ]; then
        info "Added ${INSTALL_DIR} to PATH in:${MODIFIED_FILES}"
        printf "  Run %ssource <file>%s or open a new terminal to use the0.\n" "$BOLD" "$RESET"
    else
        info "Could not find a shell config file to update PATH."
        printf "  Add the following to your shell profile manually:\n"
        printf "    %s\n" "$EXPORT_LINE"
    fi
}

main() {
    setup_colors

    printf "\n%sthe0 CLI Installer%s\n\n" "$BOLD" "$RESET"

    check_dependencies
    detect_platform
    get_latest_version
    download_binary
    verify_checksum
    install_binary
    setup_path

    printf "\n"
    success "the0 ${TAG} installed successfully!"
    printf "  Run %sthe0 --help%s to get started.\n\n" "$BOLD" "$RESET"
}

main
