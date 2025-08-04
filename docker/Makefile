# the0 Docker Management
# Simple Makefile for managing the0 Docker environment

.PHONY: help up down restart dev-up dev-down logs
# Default target
help: ## Show this help message
	@echo "the0 Docker Management"
	@echo "========================="
	@echo ""
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)

# Production environment
up: ## Start production environment (rebuilds images if needed)
	@echo "🚀 Starting the0 production environment..."
	@echo "📦 Building latest images..."
	docker compose build
	@echo "🚀 Starting services..."
	docker compose up -d
	@echo ""
	@echo "🎉 the0 is running!"
	@echo "Frontend:      http://localhost:3001"
	@echo "API:           http://localhost:3000"
	@echo "Runtime:       http://localhost:8080"
	@echo "MinIO Console: http://localhost:9001"

down: ## Stop all services
	docker compose down

restart: ## Restart all services with latest code
	@echo "🔄 Restarting the0 with latest code..."
	@echo "🛑 Stopping services..."
	docker compose down
	@echo "📦 Building latest images..."
	docker compose build
	@echo "🚀 Starting services..."
	docker compose up -d
	@echo ""
	@echo "🎉 the0 restarted with latest code!"
	@echo "Frontend:      http://localhost:3001"
	@echo "API:           http://localhost:3000"
	@echo "Runtime:       http://localhost:8080"
	@echo "MinIO Console: http://localhost:9001"

# Development environment  
dev-up: ## Start development environment with hot reload
	@echo "🚀 Starting the0 development environment..."
	@echo "📊 Step 1/3: Infrastructure services"
	@docker compose up -d postgres mongo nats minio
	@echo "⏳ Waiting for infrastructure..."
	@sleep 5
	@echo "📊 Step 2/3: Runtime services"
	@docker compose up -d bot-runner-master bot-runner-worker backtest-runner-master backtest-runner-worker bot-scheduler-master bot-scheduler-worker the0-analyzer
	@echo "📊 Step 3/3: Frontend & API with hot reload"
	@docker compose -f docker-compose.yml -f docker-compose.dev.yml up -d the0-frontend the0-api
	@echo ""
	@echo "✅ Development environment ready!"
	@echo "Frontend (dev): http://localhost:3001 (hot reload enabled)"
	@echo "API (dev):      http://localhost:3000 (hot reload enabled)"

dev-down: ## Stop development services
	docker compose -f docker-compose.yml -f docker-compose.dev.yml down

# View logs
logs: ## View logs (use 'make logs service=api' for specific service)
ifdef service
	docker compose logs -f $(service)
else
	docker compose logs -f
endif