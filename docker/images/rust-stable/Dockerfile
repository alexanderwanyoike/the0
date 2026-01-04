# Rust runtime with the0 daemon binary
# Used for Rust bot execution in both Docker and K8s modes
# Bots are pre-compiled; this image runs the compiled binary

# Stage 1: Build the runtime binary
FROM golang:1.24-alpine AS runtime-builder
RUN apk --no-cache add make git
WORKDIR /build
COPY runtime/go.mod runtime/go.sum ./
RUN go mod download
COPY runtime/ .
RUN CGO_ENABLED=0 go build -ldflags="-s -w" -o runtime ./cmd/app

# Stage 2: Final image (slim, just needs to run binaries)
FROM debian:bookworm-slim

# Install minimal dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    bash \
    && rm -rf /var/lib/apt/lists/*

# Copy runtime binary
COPY --from=runtime-builder /build/runtime /app/runtime
RUN chmod 755 /app/runtime

# Create required directories
RUN mkdir -p /bot /state /var/the0/logs

# Bootstrap script
RUN cat > /app/bootstrap.sh << 'BOOTSTRAP'
#!/bin/bash
set -e

/app/runtime daemon init \
  --bot-id "$BOT_ID" \
  --code-path /bot \
  --state-path /state \
  --code-file "$CODE_FILE" \
  --runtime "$RUNTIME" \
  --entrypoint "$ENTRYPOINT"

SYNC_FLAGS="--bot-id $BOT_ID --state-path /state --logs-path /var/the0/logs"
if [ "$IS_SCHEDULED" = "true" ]; then
  SYNC_FLAGS="$SYNC_FLAGS --watch-done /var/the0/done"
fi

/app/runtime daemon sync $SYNC_FLAGS &
SYNC_PID=$!

cleanup() {
  EXIT_CODE=$?
  if [ "$IS_SCHEDULED" = "true" ]; then
    echo $EXIT_CODE > /var/the0/done
  fi
  sleep 3
  kill $SYNC_PID 2>/dev/null || true
  wait $SYNC_PID 2>/dev/null || true
  exit $EXIT_CODE
}
trap cleanup EXIT INT TERM

cd /bot
# Don't use exec - we need the trap to fire on exit
/bin/bash /bot/entrypoint.sh
BOOTSTRAP
RUN chmod 755 /app/bootstrap.sh

WORKDIR /bot
CMD ["/app/bootstrap.sh"]
