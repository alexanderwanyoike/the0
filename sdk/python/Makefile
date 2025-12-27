.PHONY: build test publish clean

build:
	python -m build

test:
	python -m unittest discover -s tests -v

publish: build test
	python -m twine upload dist/*

clean:
	rm -rf dist build *.egg-info __pycache__ .pytest_cache
	find . -type d -name "__pycache__" -exec rm -rf {} + 2>/dev/null || true
