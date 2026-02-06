# Makefile for the0 CLI

.PHONY: build test clean install lint

# Build the CLI binary
build:
	go build -o bin/the0 .

# Run tests
test:
	go test ./... -v

# Run tests with coverage
test-coverage:
	go test ./... -v -coverprofile=coverage.out
	go tool cover -html=coverage.out -o coverage.html

# Clean build artifacts
clean:
	rm -rf bin/
	rm -f coverage.out coverage.html
	rm -f bot_*.zip

# Install dependencies
deps:
	go mod tidy
	go mod download

# Lint the code
lint:
	golangci-lint run

# Install the CLI locally
install: build
	cp bin/the0 ~/bin/the0

# Run the CLI in development mode
dev: build
	./bin/the0

# Format code
fmt:
	go fmt ./...

# Verify everything is ready for release
verify: fmt lint test build
	@echo "✅ All checks passed!"

# Release build with optimizations
release:
	CGO_ENABLED=0 GOOS=linux GOARCH=amd64 go build -ldflags="-w -s" -o bin/the0-linux-amd64 .
	CGO_ENABLED=0 GOOS=linux GOARCH=arm64 go build -ldflags="-w -s" -o bin/the0-linux-arm64 .
	CGO_ENABLED=0 GOOS=darwin GOARCH=amd64 go build -ldflags="-w -s" -o bin/the0-darwin-amd64 .
	CGO_ENABLED=0 GOOS=darwin GOARCH=arm64 go build -ldflags="-w -s" -o bin/the0-darwin-arm64 .
	CGO_ENABLED=0 GOOS=windows GOARCH=amd64 go build -ldflags="-w -s" -o bin/the0-windows-amd64.exe .
	cd bin && sha256sum the0-* > checksums.txt

