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
VALUES ?=

# Prerequisites check for general use
.PHONY: check-deps
check-deps: ## Check if required tools are installed and cluster is accessible
	@command -v helm >/dev/null 2>&1 || { echo "❌ Helm is required. Install: https://helm.sh/docs/intro/install/"; exit 1; }
	@command -v kubectl >/dev/null 2>&1 || { echo "❌ kubectl is required. Install: https://kubernetes.io/docs/tasks/tools/"; exit 1; }
	@kubectl cluster-info >/dev/null 2>&1 || { echo "❌ kubectl not configured or cluster unreachable"; exit 1; }
	@echo "✅ Prerequisites satisfied"

# Prerequisites check for minikube (doesn't require running cluster)
.PHONY: check-minikube-deps
check-minikube-deps: ## Check if required tools are installed for minikube
	@command -v minikube >/dev/null 2>&1 || { echo "❌ Minikube is required. Install: https://minikube.sigs.k8s.io/docs/start/"; exit 1; }
	@command -v helm >/dev/null 2>&1 || { echo "❌ Helm is required. Install: https://helm.sh/docs/intro/install/"; exit 1; }
	@command -v kubectl >/dev/null 2>&1 || { echo "❌ kubectl is required. Install: https://kubernetes.io/docs/tasks/tools/"; exit 1; }
	@command -v docker >/dev/null 2>&1 || { echo "❌ Docker is required. Install: https://docs.docker.com/get-docker/"; exit 1; }
	@echo "✅ Minikube prerequisites satisfied"


# Minikube lifecycle management
.PHONY: minikube-up
minikube-up: check-minikube-deps ## Build, deploy and show service URLs (simplified)
	@echo "🏗️  Starting minikube deployment..."
	@minikube start --memory=4096 --cpus=4 --disk-size=20g --driver=docker 2>/dev/null || echo "Minikube already running"
	@echo "⏳ Waiting for minikube cluster to be ready..."
	@kubectl cluster-info >/dev/null 2>&1 || { echo "❌ Failed to connect to minikube cluster"; exit 1; }
	@echo "✅ Minikube cluster is ready"
	@echo "🔨 Building application images in minikube environment..."
	@echo "⚠️  Note: This requires internet connectivity to pull base images from Docker Hub"
	@eval $$(minikube docker-env) && \
		docker build -t the0-api:latest ../api && \
		docker build --build-arg NEXT_PUBLIC_API_URL=http://api.the0.local:30000 --build-arg NEXT_PUBLIC_DOCS_URL=http://docs.the0.local:30004 -t the0-frontend:latest ../frontend && \
		docker build -t the0-docs:latest ../docs && \
		docker build -t runtime:latest ../runtime
	@echo "🚀 Deploying to minikube..."
	@kubectl create namespace $(NAMESPACE) --dry-run=client -o yaml | kubectl apply -f -
	@helm upgrade --install $(RELEASE_NAME) $(CHART_PATH) \
		--namespace $(NAMESPACE) \
		--set global.imagePullPolicy=IfNotPresent \
		--set the0Api.image.tag=latest \
		--set the0Api.imagePullPolicy=Never \
		--set the0Frontend.image.tag=latest \
		--set the0Frontend.imagePullPolicy=Never \
		--set the0Docs.image.tag=latest \
		--set the0Docs.imagePullPolicy=Never \
		--set botRunner.imagePullPolicy=Never \
		--set botScheduler.imagePullPolicy=Never \
		--set botController.image.tag=latest \
		--set botController.runtimeImage=runtime:latest \
		--set minikube.enabled=true \
		--set externalServices.enabled=true
	@echo ""
	@echo "✅ Deployment completed! Services will be available once pods are ready."
	@echo ""
	@echo "🌐 NodePort URLs (no tunnel required):"
	@echo "   Frontend:  http://the0.local:30001"
	@echo "   API:       http://api.the0.local:30000"
	@echo "   MinIO:     http://minio.the0.local:30002"
	@echo "   Docs:      http://docs.the0.local:30004"
	@echo ""
	@echo "⚠️  To access services:"
	@echo "   1. Run: make setup-hosts"
	@echo "   2. That's it! No tunnel needed."
	@echo ""
	@echo "💡 Use 'make status' to check pod readiness"
	@echo "💡 Use 'make logs' to view service logs"

.PHONY: minikube-down
minikube-down: ## Stop and remove deployment
	@echo "🛑 Stopping minikube deployment..."
	@if minikube status >/dev/null 2>&1; then \
		helm uninstall $(RELEASE_NAME) -n $(NAMESPACE) 2>/dev/null || echo "Release not found"; \
		kubectl delete namespace $(NAMESPACE) --ignore-not-found=true; \
		echo "✅ Minikube deployment stopped and cleaned up"; \
	else \
		echo "⚠️  Minikube is not running"; \
	fi

.PHONY: minikube-pause
minikube-pause: ## Pause minikube (saves resources, preserves environment)
	@echo "⏸️  Pausing minikube..."
	@minikube pause
	@echo "✅ Minikube paused. Use 'make minikube-resume' to resume."

.PHONY: minikube-resume
minikube-resume: ## Resume paused minikube
	@echo "▶️  Resuming minikube..."
	@minikube unpause
	@echo "✅ Minikube resumed."

.PHONY: minikube-stop
minikube-stop: ## Stop minikube (preserves cluster, saves more resources)
	@echo "🛑 Stopping minikube..."
	@minikube stop
	@echo "✅ Minikube stopped. Use 'make minikube-start' to restart."

.PHONY: minikube-start
minikube-start: ## Start stopped minikube
	@echo "🚀 Starting minikube..."
	@minikube start
	@echo "✅ Minikube started."

.PHONY: minikube-restart
minikube-restart: minikube-down minikube-up ## Quick restart (rebuild and redeploy)


.PHONY: minikube
minikube: minikube-up ## Alias for minikube-up (backward compatibility)

# Kubernetes-Native Controller Mode
# Uses K8s Pods and CronJobs instead of Docker-in-Docker

.PHONY: minikube-controller
minikube-controller: check-minikube-deps ## Deploy in K8s-native controller mode (no Docker-in-Docker)
	@echo "🏗️  Starting minikube deployment in controller mode..."
	@minikube start --memory=4096 --cpus=4 --disk-size=20g --driver=docker 2>/dev/null || echo "Minikube already running"
	@echo "⏳ Enabling registry addon for bot images..."
	@minikube addons enable registry
	@echo "⏳ Waiting for minikube cluster to be ready..."
	@kubectl cluster-info >/dev/null 2>&1 || { echo "❌ Failed to connect to minikube cluster"; exit 1; }
	@echo "✅ Minikube cluster is ready with registry"
	@echo "🔨 Building application images in minikube environment..."
	@eval $$(minikube docker-env) && \
		docker build -t the0-api:latest ../api && \
		docker build --build-arg NEXT_PUBLIC_API_URL=http://api.the0.local:30000 --build-arg NEXT_PUBLIC_DOCS_URL=http://docs.the0.local:30004 -t the0-frontend:latest ../frontend && \
		docker build -t the0-docs:latest ../docs && \
		docker build -t runtime:latest ../runtime
	@echo "🚀 Deploying to minikube in controller mode..."
	@kubectl create namespace $(NAMESPACE) --dry-run=client -o yaml | kubectl apply -f -
	@helm upgrade --install $(RELEASE_NAME) $(CHART_PATH) \
		--namespace $(NAMESPACE) \
		--set global.imagePullPolicy=IfNotPresent \
		--set the0Api.image.tag=latest \
		--set the0Api.imagePullPolicy=Never \
		--set the0Frontend.image.tag=latest \
		--set the0Frontend.imagePullPolicy=Never \
		--set the0Docs.image.tag=latest \
		--set the0Docs.imagePullPolicy=Never \
		--set botRunner.enabled=false \
		--set botScheduler.enabled=false \
		--set botController.enabled=true \
		--set botController.image.tag=latest \
		--set botController.imagePullPolicy=Never \
		--set botController.runtimeImage=runtime:latest \
		--set botController.imageBuilder.registry=localhost:5000 \
		--set minikube.enabled=true \
		--set externalServices.enabled=true
	@echo ""
	@echo "✅ Controller mode deployment completed!"
	@echo ""
	@echo "🔧 Controller mode benefits:"
	@echo "   - No Docker-in-Docker (no privileged pods)"
	@echo "   - Each bot runs as its own K8s Pod"
	@echo "   - Scheduled bots use K8s CronJobs"
	@echo "   - Kaniko builds bot images automatically"
	@echo ""
	@echo "🌐 NodePort URLs (no tunnel required):"
	@echo "   Frontend:  http://the0.local:30001"
	@echo "   API:       http://api.the0.local:30000"
	@echo "   MinIO:     http://minio.the0.local:30002"
	@echo "   Docs:      http://docs.the0.local:30004"
	@echo ""
	@echo "⚠️  To access services:"
	@echo "   1. Run: make setup-hosts"
	@echo "   2. That's it! No tunnel needed."
	@echo ""
	@echo "💡 Use 'make status' to check pod readiness"
	@echo "💡 Use 'make logs-controller' to view controller logs"

.PHONY: enable-registry
enable-registry: ## Enable minikube registry addon for bot images
	@echo "📦 Enabling minikube registry addon..."
	@minikube addons enable registry
	@echo "✅ Registry enabled at localhost:5000"
	@echo "💡 Images can be pushed to localhost:5000/image:tag"

# Runtime image management
.PHONY: build-runtime
build-runtime: ## Build runtime image in minikube environment
	@echo "🔨 Building runtime image..."
	@eval $$(minikube docker-env) && docker build -t runtime:latest ../runtime
	@echo "✅ Runtime image built: runtime:latest"

.PHONY: update-runtime
update-runtime: build-runtime ## Build runtime image in minikube (alias for build-runtime)
	@echo "✅ Runtime image updated in minikube"

.PHONY: restart-controller
restart-controller: ## Restart the bot controller deployment
	@echo "🔄 Restarting bot controller..."
	@kubectl rollout restart deployment $(RELEASE_NAME)-bot-controller -n $(NAMESPACE)
	@kubectl rollout status deployment $(RELEASE_NAME)-bot-controller -n $(NAMESPACE) --timeout=120s
	@echo "✅ Bot controller restarted"

.PHONY: reload-controller
reload-controller: update-runtime restart-controller ## Rebuild runtime, load to minikube, and restart controller

.PHONY: upgrade
upgrade: ## Upgrade helm release with current values
	@echo "🚀 Upgrading helm release..."
	@helm upgrade $(RELEASE_NAME) $(CHART_PATH) -n $(NAMESPACE) --reuse-values
	@echo "✅ Helm release upgraded"

.PHONY: logs-controller
logs-controller: check-deps ## Show controller logs
	@kubectl logs -n $(NAMESPACE) -l app.kubernetes.io/component=bot-controller --tail=100 -f

.PHONY: setup-hosts
setup-hosts: ## Add the0.local hostnames to /etc/hosts
	@echo "🔧 Setting up /etc/hosts entries..."
	@MINIKUBE_IP=$$(minikube ip) && \
	if grep -q "the0.local" /etc/hosts; then \
		echo "⚠️  Updating existing the0.local entries in /etc/hosts"; \
		sudo sed -i '/the0\.local/d' /etc/hosts; \
	fi && \
	echo "$$MINIKUBE_IP  the0.local api.the0.local minio.the0.local docs.the0.local" | sudo tee -a /etc/hosts > /dev/null && \
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
deploy: check-deps ## Deploy to production cluster (use VALUES=examples/aws-production.yaml)
ifndef VALUES
	$(error VALUES is required for production deploy. Usage: make deploy VALUES=examples/aws-production.yaml)
endif
	@echo "🚀 Deploying to production cluster..."
	@kubectl create namespace $(NAMESPACE) --dry-run=client -o yaml | kubectl apply -f -
	@helm upgrade --install $(RELEASE_NAME) $(CHART_PATH) \
		--namespace $(NAMESPACE) --create-namespace \
		-f "$(VALUES)" \
		--wait --timeout 5m
	@echo "✅ Production deployment completed!"

.PHONY: template
template: ## Render templates locally (use VALUES=examples/aws-production.yaml)
	@helm template $(RELEASE_NAME) $(CHART_PATH) $(if $(VALUES),-f "$(VALUES)",)

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