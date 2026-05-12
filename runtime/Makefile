# Variables
BINARY_NAME=runtime
BUILD_DIR=./build

.PHONY: build clean docker-build test fmt security-audit security-image security-scan

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

security-audit:
	@command -v govulncheck >/dev/null 2>&1 || { echo "govulncheck is required for Go vulnerability scanning. Install: go install golang.org/x/vuln/cmd/govulncheck@latest"; exit 1; }
	govulncheck ./...

security-image:
	@command -v docker >/dev/null 2>&1 || { echo "docker is required to build the image. Install: https://docs.docker.com/get-docker/"; exit 1; }
	@command -v trivy >/dev/null 2>&1 || { echo "trivy is required to scan the image. Install: https://aquasecurity.github.io/trivy/latest/getting-started/installation/"; exit 1; }
	docker build -t $(BINARY_NAME):security .
	trivy image --exit-code 1 --severity HIGH,CRITICAL $(BINARY_NAME):security

security-scan: security-audit security-image

# Alias for backwards compatibility
go-build: build
