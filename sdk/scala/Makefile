.PHONY: build test publish clean

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
