#!/bin/sh
set -xe -o pipefail

# Check if docker socket is already available (Docker-in-Docker case)
if [ -S /var/run/docker.sock ]; then
    echo "Docker socket already available, skipping dockerd startup"
else
    echo "Starting dockerd..."
    # Start dockerd in background
    /usr/bin/dockerd --iptables=false --ip6tables=false --bridge=none -D &
    # Wait for Docker daemon to be ready
    sleep 5
fi

# Verify Docker is working
docker info || exit 1

# Start the application as worker
exec ./runtime -worker