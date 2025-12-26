.PHONY: build test publish clean

build:
	yarn build

test:
	yarn test

publish: build test
	npm publish

clean:
	rm -rf dist node_modules
