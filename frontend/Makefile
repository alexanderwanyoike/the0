.PHONY: security-audit security-image security-scan

SECURITY_IMAGE ?= the0-frontend:security

security-audit:
	@command -v yarn >/dev/null 2>&1 || { echo "yarn is required for dependency auditing. Install: https://yarnpkg.com/getting-started/install"; exit 1; }
	yarn audit --level high

security-image:
	@command -v docker >/dev/null 2>&1 || { echo "docker is required to build the image. Install: https://docs.docker.com/get-docker/"; exit 1; }
	@command -v trivy >/dev/null 2>&1 || { echo "trivy is required to scan the image. Install: https://aquasecurity.github.io/trivy/latest/getting-started/installation/"; exit 1; }
	docker build -t $(SECURITY_IMAGE) .
	trivy image --exit-code 1 --severity HIGH,CRITICAL $(SECURITY_IMAGE)

security-scan: security-audit security-image
