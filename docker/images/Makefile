# Build all the0 runtime images
# Each image includes the daemon binary for state/log synchronization

REGISTRY ?= the0
VERSION ?= latest

# All runtime images to build
RUNTIMES = python311 nodejs20 rust-stable dotnet8 gcc13 scala3 ghc96

# Build context is the repository root (to access runtime/ source)
BUILD_CONTEXT = ../..

.PHONY: all build-all $(RUNTIMES) push-all clean

all: build-all

build-all: $(RUNTIMES)

$(RUNTIMES):
	@echo "Building $(REGISTRY)/$@:$(VERSION)..."
	docker build -t $(REGISTRY)/$@:$(VERSION) -f $@/Dockerfile $(BUILD_CONTEXT)

# Build a single image: make python311
python311 nodejs20 rust-stable dotnet8 gcc13 scala3 ghc96:
	docker build -t $(REGISTRY)/$@:$(VERSION) -f $@/Dockerfile $(BUILD_CONTEXT)

push-all:
	@for runtime in $(RUNTIMES); do \
		echo "Pushing $(REGISTRY)/$$runtime:$(VERSION)..."; \
		docker push $(REGISTRY)/$$runtime:$(VERSION); \
	done

# Verify images exist
list:
	@for runtime in $(RUNTIMES); do \
		echo "$(REGISTRY)/$$runtime:$(VERSION)"; \
	done

# Test that runtime binary works in each image
test:
	@for runtime in $(RUNTIMES); do \
		echo "Testing $(REGISTRY)/$$runtime:$(VERSION)..."; \
		docker run --rm $(REGISTRY)/$$runtime:$(VERSION) /app/runtime version || exit 1; \
	done

clean:
	@for runtime in $(RUNTIMES); do \
		docker rmi $(REGISTRY)/$$runtime:$(VERSION) 2>/dev/null || true; \
	done
