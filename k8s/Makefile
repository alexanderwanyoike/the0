# Makefile for the0 Kubernetes Deployment

.PHONY: help
help: ## Show this help message
	@echo "🚀 the0 Platform - Kubernetes Deployment"
	@echo ""
	@awk 'BEGIN {FS = ":.*##"; printf "Usage:\n  make \033[36m<target>\033[0m\n\nTargets:\n"} /^[a-zA-Z_-]+:.*?##/ { printf "  \033[36m%-20s\033[0m %s\n", $$1, $$2 }' $(MAKEFILE_LIST)

# Variables
RELEASE_NAME ?= the0
NAMESPACE ?= the0
CHART_PATH ?= .

# Prerequisites check
.PHONY: check-deps
check-deps: ## Check if required tools are installed
	@command -v helm >/dev/null 2>&1 || { echo "❌ Helm is required. Install: https://helm.sh/docs/intro/install/"; exit 1; }
	@command -v kubectl >/dev/null 2>&1 || { echo "❌ kubectl is required. Install: https://kubernetes.io/docs/tasks/tools/"; exit 1; }
	@kubectl cluster-info >/dev/null 2>&1 || { echo "❌ kubectl not configured or cluster unreachable"; exit 1; }
	@echo "✅ Prerequisites satisfied"

# Minikube lifecycle management
.PHONY: minikube-up
minikube-up: check-deps ## Build, deploy and show service URLs (simplified)
	@echo "🏗️  Starting minikube deployment..."
	@minikube start --memory=4096 --cpus=4 --disk-size=20g --driver=docker 2>/dev/null || echo "Minikube already running"
	@echo "🔨 Building application images in minikube environment..."
	@eval $$(minikube docker-env) && \
		docker build -t the0-api:latest ../api && \
		docker build --build-arg NEXT_PUBLIC_API_URL=http://api.the0.local:30000 -t the0-frontend:latest ../frontend && \
		docker build -t runtime:latest ../runtime && \
		docker build -t the0-analyzer:latest ../services/0vers33r
	@echo "🚀 Deploying to minikube..."
	@kubectl create namespace $(NAMESPACE) --dry-run=client -o yaml | kubectl apply -f -
	@helm upgrade --install $(RELEASE_NAME) $(CHART_PATH) \
		--namespace $(NAMESPACE) \
		--set global.imagePullPolicy=IfNotPresent \
		--set the0Api.imagePullPolicy=Never \
		--set the0Frontend.imagePullPolicy=Never \
		--set botRunner.imagePullPolicy=Never \
		--set backtestRunner.imagePullPolicy=Never \
		--set botScheduler.imagePullPolicy=Never \
		--set the0Analyzer.imagePullPolicy=Never \
		--set minikube.enabled=true \
		--set externalServices.enabled=true
	@echo ""
	@echo "✅ Deployment completed! Services will be available once pods are ready."
	@echo ""
	@echo "🌐 NodePort URLs (no tunnel required):"
	@echo "   Frontend:  http://the0.local:30001"
	@echo "   API:       http://api.the0.local:30000"
	@echo "   MinIO:     http://minio.the0.local:30002"
	@echo ""
	@echo "⚠️  To access services:"
	@echo "   1. Run: make setup-hosts"
	@echo "   2. That's it! No tunnel needed."
	@echo ""
	@echo "💡 Use 'make status' to check pod readiness"
	@echo "💡 Use 'make logs' to view service logs"

.PHONY: minikube-down
minikube-down: check-deps ## Stop and remove deployment
	@echo "🛑 Stopping minikube deployment..."
	@helm uninstall $(RELEASE_NAME) -n $(NAMESPACE) 2>/dev/null || echo "Release not found"
	@kubectl delete namespace $(NAMESPACE) --ignore-not-found=true
	@echo "✅ Minikube deployment stopped and cleaned up"

.PHONY: minikube-restart
minikube-restart: minikube-down minikube-up ## Quick restart (rebuild and redeploy)

.PHONY: minikube
minikube: minikube-up ## Alias for minikube-up (backward compatibility)

.PHONY: setup-hosts
setup-hosts: ## Add the0.local hostnames to /etc/hosts
	@echo "🔧 Setting up /etc/hosts entries..."
	@MINIKUBE_IP=$$(minikube ip) && \
	if grep -q "the0.local" /etc/hosts; then \
		echo "⚠️  Updating existing the0.local entries in /etc/hosts"; \
		sudo sed -i '/the0\.local/d' /etc/hosts; \
	fi && \
	echo "$$MINIKUBE_IP  the0.local api.the0.local minio.the0.local" | sudo tee -a /etc/hosts > /dev/null && \
	echo "✅ Added the0.local entries to /etc/hosts ($$MINIKUBE_IP)"

.PHONY: tunnel
tunnel: ## Start minikube tunnel for LoadBalancer services
	@echo "🔄 Starting minikube tunnel..."
	@minikube tunnel

.PHONY: remove-hosts
remove-hosts: ## Remove the0.local hostnames from /etc/hosts
	@echo "🔧 Removing /etc/hosts entries..."
	@sudo sed -i '/the0\.local/d' /etc/hosts
	@echo "✅ Removed the0.local entries from /etc/hosts"

# Production deployment (assumes external infrastructure)
.PHONY: deploy
deploy: check-deps ## Deploy to production cluster with external infrastructure
	@echo "🚀 Deploying to production cluster..."
	@kubectl create namespace $(NAMESPACE) --dry-run=client -o yaml | kubectl apply -f -
	@helm upgrade --install $(RELEASE_NAME) $(CHART_PATH) \
		--namespace $(NAMESPACE) \
		--set postgresql.enabled=false \
		--set mongodb.enabled=false \
		--set nats.enabled=false \
		--set minio.enabled=false \
		--set minikube.enabled=false
	@echo "✅ Production deployment completed!"

# Status and logs
.PHONY: status
status: check-deps ## Show deployment status
	@helm status $(RELEASE_NAME) -n $(NAMESPACE)
	@echo ""
	@kubectl get pods -n $(NAMESPACE)

.PHONY: logs
logs: check-deps ## Show recent logs from all services
	@kubectl logs -n $(NAMESPACE) -l app.kubernetes.io/instance=$(RELEASE_NAME) --tail=50 --all-containers=true

# Show minikube service URLs
define show_services
	@echo "✅ Services available via minikube:"
	@echo ""
	@minikube service list -n $(NAMESPACE)
	@echo ""
	@echo "🎯 LoadBalancer services accessible on localhost via minikube tunnel"
endef

# Minikube service access
.PHONY: urls
urls: check-deps ## Show minikube service URLs
	@echo "📍 Minikube service URLs:"
	@minikube service list -n $(NAMESPACE) | grep $(RELEASE_NAME)-.*-external || echo "No external services found"

.PHONY: services 
services: check-deps ## Show minikube service URLs
	$(call show_services)


# Cleanup
.PHONY: uninstall
uninstall: check-deps ## Remove the deployment
	@helm uninstall $(RELEASE_NAME) -n $(NAMESPACE)
	@echo "✅ Deployment removed"

.PHONY: clean
clean: uninstall ## Remove deployment and namespace
	@kubectl delete namespace $(NAMESPACE) --ignore-not-found=true
	@echo "✅ Namespace removed"

# Default target
.DEFAULT_GOAL := help