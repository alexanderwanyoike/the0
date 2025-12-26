.PHONY: build test publish clean

GITHUB_SOURCE := https://nuget.pkg.github.com/alexanderwanyoike/index.json
PACKAGE_NAME := The0.Sdk
VERSION := $(shell grep -oP '(?<=<Version>)[^<]+' The0.csproj)

build:
	dotnet build -c Release

test:
	cd tests && dotnet test

pack: build
	dotnet pack -c Release

publish: pack test
	@if [ -z "$$GITHUB_TOKEN" ]; then echo "Error: GITHUB_TOKEN not set"; exit 1; fi
	dotnet nuget push bin/Release/$(PACKAGE_NAME).$(VERSION).nupkg \
		--source "$(GITHUB_SOURCE)" \
		--api-key $$GITHUB_TOKEN

clean:
	rm -rf bin obj
	cd tests && rm -rf bin obj
