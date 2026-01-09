.PHONY: build test clean sdist

build:
	cabal build

test:
	cabal test --enable-tests

clean:
	cabal clean
	rm -rf dist-newstyle

sdist:
	cabal sdist
