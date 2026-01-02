# Variables
BINARY_NAME=runtime
BUILD_DIR=./build

.PHONY: build clean docker-build test fmt

# Build the Go binary
build:
	@echo "Building the project..."
	@mkdir -p $(BUILD_DIR)
	go build -o $(BUILD_DIR)/$(BINARY_NAME) ./cmd/app
	@echo "Done! Executable is in $(BUILD_DIR)/$(BINARY_NAME)"

clean:
	@echo "Cleaning up..."
	rm -rf $(BUILD_DIR)/$(BINARY_NAME)
	@echo "Done!"

# Docker related targets
docker-build:
	@echo "Building Docker image..."
	docker build -t $(BINARY_NAME):latest .
	@echo "Done!"

# Test related targets
test:
	@echo "Running all tests..."
	go test -v ./...
	@echo "Done!"

test-short:
	@echo "Running short tests..."
	go test -short ./...
	@echo "Done!"

fmt:
	@echo "Formatting code..."
	go fmt ./...
	@echo "Done!"

# Alias for backwards compatibility
go-build: build
