# Universal the0 runtime image with all language runtimes
# Used for bot execution in both Docker and K8s modes
#
# Build: docker build -t the0/runtime:latest runtime/
#
# Base: Ubuntu 24.04 (GCC 13 / GLIBCXX_3.4.32+ for C++ binary compatibility)
#
# Supported runtimes:
#   - python3.11: Python 3.11 (from deadsnakes PPA)
#   - nodejs22: Node.js 22 LTS
#   - nodejs20: Node.js 20 (end of life, kept for already-deployed bots)
#   - dotnet8: .NET 8 runtime
#   - scala3: Scala 3 (via Java 17 JRE)
#   - rust-stable: Pre-compiled Rust binaries
#   - gcc13: Pre-compiled C++ binaries (requires GCC 13+ libstdc++)
#   - ghc96: Pre-compiled Haskell binaries

# Stage 1: Build the runtime binary from Go source
FROM dhi.io/golang:1.25-alpine3.22-dev AS runtime-builder
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

# Node.js 22 LTS is the default `node` on PATH (runtime: nodejs22).
# npm and corepack are removed from both Node installs: the runtime only ever
# executes `node` (dependencies are vendored by the CLI before upload), and
# npm's bundled deps are a recurring source of HIGH advisories in image scans.
RUN curl -fsSL https://deb.nodesource.com/setup_22.x | bash - \
    && apt-get install -y --no-install-recommends nodejs \
    && rm -rf /var/lib/apt/lists/* /usr/lib/node_modules \
       /usr/bin/npm /usr/bin/npx /usr/bin/corepack

# Node.js 20 (runtime: nodejs20) is end of life and only stays so that bots
# already deployed on it keep running until their authors move to nodejs22
# (issue #331). Pinned to the final 20.x release.
ARG NODE20_VERSION=20.20.2
RUN case "$(dpkg --print-architecture)" in \
      amd64) NODE_ARCH=x64;   NODE_SHA=19e56f0825510207dd904f087fe52faa0a4eb6b2aab5f0ea7a33830d04888b8b ;; \
      arm64) NODE_ARCH=arm64; NODE_SHA=47ef73d543ecf6eb19435f6c03a0ac4809b3bf0dd6b26c7c571efc2a6572a74d ;; \
      *) echo "unsupported architecture for Node 20" >&2; exit 1 ;; \
    esac \
    && curl -fsSLo /tmp/node20.tar.gz "https://nodejs.org/dist/v${NODE20_VERSION}/node-v${NODE20_VERSION}-linux-${NODE_ARCH}.tar.gz" \
    && echo "${NODE_SHA}  /tmp/node20.tar.gz" | sha256sum -c - \
    && mkdir -p /opt/node20 \
    && tar -xzf /tmp/node20.tar.gz -C /opt/node20 --strip-components=1 \
    && rm -rf /tmp/node20.tar.gz /opt/node20/lib/node_modules \
       /opt/node20/bin/npm /opt/node20/bin/npx /opt/node20/bin/corepack

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
RUN mkdir -p /bot /state /query /var/the0/logs \
    && chown -R 1000:1000 /app /bot /state /query /var/the0

# Set working directory to bot code location
WORKDIR /bot

USER 1000:1000

# The execute command handles everything:
# - Downloads code from MinIO
# - Downloads state from MinIO
# - Starts sync subprocess (Docker mode)
# - Starts query server subprocess (if realtime bot)
# - Executes the bot
# - Handles cleanup and signal forwarding
CMD ["/app/runtime", "execute"]
