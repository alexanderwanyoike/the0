# the0-ai Desktop Application Makefile
# Provides commands for development, building, and running the application

.PHONY: help install install-deps dev dev-browser build clean test

# Default target
help:
	@echo "the0-ai Desktop Application Commands:"
	@echo ""
	@echo "Development:"
	@echo "  make setup        - Set up development environment and install dependencies"
	@echo "  make install-deps - Install Python and Node.js dependencies"
	@echo "  make dev          - Run application in desktop mode"
	@echo "  make dev-browser  - Run application in browser mode"
	@echo "  make dev-staging  - Run application with staging updates"
	@echo ""
	@echo "Building:"
	@echo "  make build        - Build standalone executable"
	@echo "  make build-frontend - Build React frontend only"
	@echo "  make install      - Install executable to ~/bin/the0-ai"
	@echo ""
	@echo "Maintenance:"
	@echo "  make clean        - Clean build artifacts and cache"
	@echo "  make test         - Run tests (when available)"

# Set up development environment
setup: install-deps
	@echo "✅ Development environment setup complete!"

# Install all dependencies
install-deps:
	@echo "📦 Installing Python dependencies..."
	pip install -r requirements.txt
	@echo "📦 Installing Node.js dependencies..."
	cd frontend && npm install
	@echo "✅ All dependencies installed!"

# Run application in desktop mode
dev:
	@echo "🚀 Starting the0-ai in desktop mode..."
	python3 the0_ai.py

# Run application in browser mode (for debugging)
dev-browser:
	@echo "🌐 Starting the0-ai in browser mode..."
	python3 the0_ai.py --browser

# Run application with staging updates  
dev-staging:
	@echo "🧪 Starting the0-ai with staging update stream..."
	THE0_AI_UPDATE_STREAM=staging python3 the0_ai.py

# Build standalone executable
build: build-frontend
	@echo "🔨 Building standalone executable..."
	python3 build.py
	@echo "✅ Build complete! Executable created in dist/"

# Build React frontend only
build-frontend:
	@echo "🏗️ Building React frontend..."
	cd frontend && npm run build
	@echo "✅ Frontend build complete!"

# Install executable to user's bin directory  
install: build
	@echo "📦 Installing the0-ai executable..."
	python3 install.py
	@echo "✅ Installation complete!"

# Clean build artifacts and cache
clean:
	@echo "🧹 Cleaning build artifacts..."
	rm -rf dist/
	rm -rf build/
	rm -rf *.spec
	rm -rf frontend/dist/
	rm -rf frontend/node_modules/.cache/
	rm -rf __pycache__/
	rm -rf **/__pycache__/
	rm -rf .pytest_cache/
	@echo "✅ Clean complete!"

# Run tests (placeholder for when tests are added)
test:
	@echo "🧪 Running tests..."
	@echo "⚠️  No tests configured yet"