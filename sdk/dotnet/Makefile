.PHONY: build test pack publish clean security-audit security-scan

VERSION := $(shell grep -oP '(?<=<Version>)[^<]+' The0.csproj)

build:
	dotnet build -c Release

test:
	cd tests && dotnet test

pack: build
	dotnet pack -c Release

publish: pack test
	@if [ -z "$$NUGET_API_KEY" ]; then echo "Error: NUGET_API_KEY not set"; exit 1; fi
	dotnet nuget push bin/Release/The0.Sdk.$(VERSION).nupkg \
		--api-key $$NUGET_API_KEY \
		--source https://api.nuget.org/v3/index.json

clean:
	rm -rf bin obj
	cd tests && rm -rf bin obj

security-audit:
	@command -v dotnet >/dev/null 2>&1 || { echo "dotnet is required for dependency auditing. Install: https://dotnet.microsoft.com/download"; exit 1; }
	@command -v jq >/dev/null 2>&1 || { echo "jq is required for dependency audit summaries. Install jq and retry."; exit 1; }
	dotnet restore
	SECURITY_AUDIT_STRICT=1 bash ../../.github/scripts/security/dotnet-audit.sh sdk-dotnet

security-scan: security-audit
