.PHONY: build test publish clean security-audit security-scan

build:
	yarn build

test:
	yarn test

publish: build test
	npm publish

clean:
	rm -rf dist node_modules

security-audit:
	@command -v yarn >/dev/null 2>&1 || { echo "yarn is required for dependency auditing. Install: https://yarnpkg.com/getting-started/install"; exit 1; }
	yarn audit --level high

security-scan: security-audit
