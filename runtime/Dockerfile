# Use the official Golang image as the base image
FROM golang:tip-alpine as builder

RUN apk --no-cache add make

RUN apk --no-cache add protobuf protobuf-dev

WORKDIR /app

COPY go.mod go.sum ./

RUN go mod download

COPY . .

RUN make go-build

# Use Docker-in-Docker image which includes both Docker daemon and client
FROM alpine:latest

# Update package repository and install Docker
RUN apk update && \
    apk add --no-cache \
    docker \
    docker-compose \
    docker-cli-compose \
    iptables-legacy \
    iproute2

# Install necessary packages for the application
RUN apk --no-cache add ca-certificates

# Create a non-root user with docker group access
RUN addgroup -g 1000 appuser && \
    adduser -D -u 1000 -G appuser appuser && \
    addgroup appuser docker

# Create necessary directories with proper permissions
RUN mkdir -p /app /tmp/runtime && \
    chown -R appuser:appuser /app /tmp/runtime

# Set the working directory inside the container
WORKDIR /app

EXPOSE 8080

# Expose gRPC port
EXPOSE 50051

# Copy the built binary from the builder stage
COPY --from=builder /app/build/runtime .

# Ensure the binary is executable
RUN chmod +x ./runtime

# Note: Running as root is required for dockerd to function properly
# The Kubernetes security context provides the necessary isolation
USER root

# Set entrypoint to handle Docker daemon startup if needed
ENTRYPOINT ["/bin/sh", "-c", "if [ ! -S /var/run/docker.sock ]; then /usr/bin/dockerd --iptables=false --ip6tables=false --bridge=none -D & sleep 5 && docker info; fi && exec \"$@\"", "--"]

# Default command (can be overridden in docker-compose)
CMD ["./runtime", "--help"]