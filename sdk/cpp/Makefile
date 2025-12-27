.PHONY: test clean

# C++ is header-only, no build needed

test:
	cd tests && mkdir -p build && cd build && cmake .. && cmake --build . && ./the0_test

clean:
	rm -rf tests/build
