.PHONY: test clean

# C++ is header-only, no build needed

test:
	cd tests && mkdir -p build && cd build && cmake .. && cmake --build . && ctest --output-on-failure

clean:
	rm -rf tests/build
