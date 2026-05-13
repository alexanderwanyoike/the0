.PHONY: build test publish clean security-audit security-scan

build:
	sbt compile

test:
	sbt test

publish: test
	@if [ -z "$$GITHUB_TOKEN" ]; then echo "Error: GITHUB_TOKEN not set"; exit 1; fi
	sbt publish

clean:
	sbt clean
	rm -rf target project/target

security-audit:
	@command -v sbt >/dev/null 2>&1 || { echo "sbt is required for dependency auditing. Install: https://www.scala-sbt.org/download/"; exit 1; }
	@command -v osv-scanner >/dev/null 2>&1 || { echo "osv-scanner is required for dependency auditing. Install: go install github.com/google/osv-scanner/v2/cmd/osv-scanner@latest"; exit 1; }
	@command -v jq >/dev/null 2>&1 || { echo "jq is required for dependency audit summaries. Install jq and retry."; exit 1; }
	SECURITY_AUDIT_STRICT=1 bash ../../.github/scripts/security/scala-osv-audit.sh sdk-scala

security-scan: security-audit
