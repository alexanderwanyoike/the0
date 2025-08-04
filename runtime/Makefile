# Variables
BINARY_NAME=runtime
BUILD_DIR=./build
DOCKER_TAG=latest

# Go related variables
GOBIN=$(shell go env GOBIN)

.PHONY: install-proto-tools build-proto go-build clean docker-build up down test test-unit test-integration test-all fmt

# Protocol Buffer related targets
install-proto-tools:
	@echo "Installing Protocol Buffer and gRPC tools..."
	go install google.golang.org/protobuf/cmd/protoc-gen-go@latest
	go install google.golang.org/grpc/cmd/protoc-gen-go-grpc@latest
	@echo "Done! Make sure $(GOBIN) is in your PATH."
	@echo "Current PATH: $(PATH)"

build-proto:
	@echo "Building Protocol Buffers..."
	protoc --go_out=. --go-grpc_out=. pb/worker.proto
	@echo "Done! Generated files are in ./pkg/proto."

# Build related targets
go-build: install-proto-tools build-proto
	@echo "Building the project..."
	go build -o $(BUILD_DIR)/$(BINARY_NAME) ./cmd/app
	@echo "Done! Executable is in the build directory."

clean:
	@echo "Cleaning up..."
	rm -rf $(BUILD_DIR)/$(BINARY_NAME)
	@echo "Done! Cleaned up the build files."

# Docker related targets
docker-build:
	@echo "Building Docker image..."
	docker build -t $(BINARY_NAME):$(DOCKER_TAG) .
	@echo "Done! Docker image is built."

# Test related targets
test:
	@echo "Running all tests..."
	go test -v ./...
	@echo "Done! All tests completed."

fmt:
	@echo "Formatting code..."
	go fmt ./...
	@echo "Done! Code is formatted."