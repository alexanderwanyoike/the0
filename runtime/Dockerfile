# Build stage
FROM golang:1.24-alpine AS builder

RUN apk --no-cache add make

WORKDIR /app

COPY go.mod go.sum ./
RUN go mod download

COPY . .

RUN make build

# Runtime stage - includes Docker client for container management
FROM alpine:latest

# Install Docker client and networking tools
RUN apk update && \
    apk add --no-cache \
    docker-cli \
    ca-certificates \
    iptables-legacy \
    iproute2

# Create necessary directories
RUN mkdir -p /app /tmp/runtime

WORKDIR /app

# Health check port
EXPOSE 8080

# Copy the built binary from the builder stage
COPY --from=builder /app/build/runtime .

# Ensure the binary is executable
RUN chmod +x ./runtime

# Default command
CMD ["./runtime", "--help"]
