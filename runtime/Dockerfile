# Universal the0 runtime image with all language runtimes
# Used for bot execution in both Docker and K8s modes
#
# Build: docker build -t the0/runtime:latest runtime/
#
# Base: Ubuntu 24.04 (GCC 13 / GLIBCXX_3.4.32+ for C++ binary compatibility)
#
# Supported runtimes:
#   - python3.11: Python 3.11 (from deadsnakes PPA)
#   - nodejs20: Node.js 20 LTS
#   - dotnet8: .NET 8 runtime
#   - scala3: Scala 3 (via Java 17 JRE)
#   - rust-stable: Pre-compiled Rust binaries
#   - gcc13: Pre-compiled C++ binaries (requires GCC 13+ libstdc++)
#   - ghc96: Pre-compiled Haskell binaries

# Stage 1: Build the runtime binary from Go source
FROM golang:1.24-alpine AS runtime-builder
RUN apk --no-cache add make git
WORKDIR /build
COPY go.mod go.sum ./
RUN go mod download
COPY . .
RUN CGO_ENABLED=0 go build -ldflags="-s -w" -o runtime ./cmd/app

# Stage 2: Universal runtime image with all language support
# Ubuntu 24.04 provides GCC 13 / libstdc++ with GLIBCXX_3.4.32+ for C++ binaries
FROM ubuntu:24.04

# Prevent interactive prompts during package installation
ENV DEBIAN_FRONTEND=noninteractive

# Install base dependencies and utilities
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    ca-certificates \
    gnupg \
    && rm -rf /var/lib/apt/lists/*

# Python 3.11 from deadsnakes PPA (required for ABI compatibility with vendored packages)
RUN apt-get update && apt-get install -y --no-install-recommends \
    software-properties-common \
    && add-apt-repository -y ppa:deadsnakes/ppa \
    && apt-get update && apt-get install -y --no-install-recommends \
    python3.11 \
    python3.11-venv \
    python3.11-distutils \
    && rm -rf /var/lib/apt/lists/* \
    && ln -sf /usr/bin/python3.11 /usr/bin/python3 \
    && ln -sf /usr/bin/python3.11 /usr/bin/python

# Node.js 20 LTS
RUN curl -fsSL https://deb.nodesource.com/setup_20.x | bash - \
    && apt-get install -y --no-install-recommends nodejs \
    && rm -rf /var/lib/apt/lists/* \
    && npm install -g npm@latest

# .NET 8 Runtime
RUN apt-get update && apt-get install -y --no-install-recommends \
    dotnet-runtime-8.0 \
    && rm -rf /var/lib/apt/lists/*

# Java 17 JRE (for Scala JARs)
RUN apt-get update && apt-get install -y --no-install-recommends \
    openjdk-17-jre-headless \
    && rm -rf /var/lib/apt/lists/*

# Runtime libraries for compiled languages (Rust, C++, Haskell)
# These are needed to run pre-compiled binaries
# Ubuntu 24.04 includes GCC 13's libstdc++ with GLIBCXX_3.4.32+
RUN apt-get update && apt-get install -y --no-install-recommends \
    libstdc++6 \
    libgmp10 \
    libffi8 \
    zlib1g \
    && rm -rf /var/lib/apt/lists/*

# Copy the runtime binary from builder
COPY --from=runtime-builder /build/runtime /app/runtime
RUN chmod 755 /app/runtime

# Copy wrapper scripts for Python and Node.js
# These handle signal management, config parsing, and result writing
COPY wrappers/ /app/wrappers/
RUN chmod 755 /app/wrappers/*.py /app/wrappers/*.js

# Create required directories
# /bot - Bot code (downloaded from MinIO)
# /state - Bot persistent state (synced to MinIO)
# /query - Query results (for ephemeral queries)
# /var/the0/logs - Bot logs (synced to MinIO)
RUN mkdir -p /bot /state /query /var/the0/logs

# Set working directory to bot code location
WORKDIR /bot

# The execute command handles everything:
# - Downloads code from MinIO
# - Downloads state from MinIO
# - Starts sync subprocess (Docker mode)
# - Starts query server subprocess (if realtime bot)
# - Executes the bot
# - Handles cleanup and signal forwarding
CMD ["/app/runtime", "execute"]
