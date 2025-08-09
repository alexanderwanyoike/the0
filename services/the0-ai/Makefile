# the0-ai API Makefile
# Provides commands for development, testing, formatting, and running the AI agent API

.PHONY: help install-deps dev test format lint clean clean-deps run-agent setup

# Default target
help:
	@echo "the0-ai API Commands:"
	@echo ""
	@echo "Setup:"
	@echo "  make setup        - Set up virtual environment and install dependencies"
	@echo "  make install-deps - Install Python dependencies"
	@echo ""
	@echo "Development:"
	@echo "  make dev          - Run AI agent API in development mode"
	@echo "  make run-agent    - Run AI agent API server"
	@echo ""
	@echo "Code Quality:"
	@echo "  make format       - Format code with black"
	@echo "  make lint         - Lint code with flake8"
	@echo "  make test         - Run all tests with pytest"
	@echo "  make test-watch   - Run tests in watch mode"
	@echo ""
	@echo "Maintenance:"
	@echo "  make clean        - Clean cache and artifacts"
	@echo "  make clean-deps   - Clean up unused dependencies"

# Set up virtual environment and install dependencies
setup:
	@echo "🐍 Setting up Python virtual environment..."
	python3 -m venv venv
	@echo "📦 Installing dependencies..."
	. venv/bin/activate && pip install -r requirements.txt
	@echo "✅ Development environment setup complete!"

# Install Python dependencies
install-deps:
	@echo "📦 Installing Python dependencies..."
	. venv/bin/activate && pip install -r requirements.txt
	@echo "✅ Dependencies installed!"

# Run AI agent API in development mode
dev:
	@echo "🚀 Starting AI agent API server..."
	. venv/bin/activate && uvicorn api.main:app --reload --host 0.0.0.0 --port 8000

# Run AI agent API server
run-agent:
	@echo "🚀 Starting AI agent API server..."
	. venv/bin/activate && python -m uvicorn api.main:app --host 0.0.0.0 --port 8000

# Format code with black
format:
	@echo "🎨 Formatting code with black..."
	. venv/bin/activate && black api/ the0/ tests/ --line-length 88
	@echo "✅ Code formatted!"

# Lint code with flake8
lint:
	@echo "🔍 Linting code with flake8..."
	. venv/bin/activate && flake8 api/ the0/ tests/ --max-line-length=88 --ignore=E203,W503
	@echo "✅ Code linted!"

# Run all tests with pytest
test:
	@echo "🧪 Running all tests..."
	. venv/bin/activate && python -m pytest tests/ -v --tb=short
	@echo "✅ Tests complete!"

# Run tests in watch mode
test-watch:
	@echo "🧪 Running tests in watch mode..."
	. venv/bin/activate && python -m pytest tests/ -v --tb=short -f

# Clean cache and build artifacts
clean:
	@echo "🧹 Cleaning cache and artifacts..."
	rm -rf __pycache__/
	rm -rf **/__pycache__/
	rm -rf .pytest_cache/
	rm -rf *.egg-info/
	rm -rf build/
	rm -rf dist/
	find . -name "*.pyc" -delete
	find . -name "*.pyo" -delete
	@echo "✅ Clean complete!"

# Clean up unused dependencies
clean-deps:
	@echo "🧹 Cleaning up unused dependencies..."
	. venv/bin/activate && pip-autoremove -y || echo "Install pip-autoremove with: pip install pip-autoremove"
	@echo "✅ Dependencies cleaned!"