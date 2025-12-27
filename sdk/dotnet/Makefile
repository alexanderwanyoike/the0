.PHONY: build test pack publish clean

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
