# Build the0 runtime image
# The universal 'runtime' image contains all language runtimes

REGISTRY ?= the0
VERSION ?= latest

# Build context is the runtime directory (self-contained with Dockerfile)
BUILD_CONTEXT = ../../runtime

.PHONY: all build push clean list test minikube

all: build

# Build the universal runtime image
build:
	@echo "Building $(REGISTRY)/runtime:$(VERSION)..."
	docker build -t $(REGISTRY)/runtime:$(VERSION) $(BUILD_CONTEXT)

# Alias for build
runtime: build

# Push runtime image to registry
push: build
	@echo "Pushing $(REGISTRY)/runtime:$(VERSION)..."
	docker push $(REGISTRY)/runtime:$(VERSION)

# Verify image exists
list:
	@echo "$(REGISTRY)/runtime:$(VERSION)"

# Test that runtime binary works in the image
test: build
	@echo "Testing $(REGISTRY)/runtime:$(VERSION)..."
	docker run --rm $(REGISTRY)/runtime:$(VERSION) /app/runtime version

clean:
	docker rmi $(REGISTRY)/runtime:$(VERSION) 2>/dev/null || true

# Build runtime image in minikube
minikube:
	@echo "Building $(REGISTRY)/runtime:$(VERSION) in minikube..."
	minikube image build -t $(REGISTRY)/runtime:$(VERSION) $(BUILD_CONTEXT)
