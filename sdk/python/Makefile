.PHONY: build test publish clean security-audit security-scan

build:
	python -m build

test:
	python -m unittest discover -s tests -v

publish: build test
	python -m twine upload dist/*

clean:
	rm -rf dist build *.egg-info __pycache__ .pytest_cache
	find . -type d -name "__pycache__" -exec rm -rf {} + 2>/dev/null || true

security-audit:
	@command -v python >/dev/null 2>&1 || command -v python3 >/dev/null 2>&1 || { echo "python is required for dependency auditing. Install Python 3 and retry."; exit 1; }
	@command -v jq >/dev/null 2>&1 || { echo "jq is required for dependency audit summaries. Install jq and retry."; exit 1; }
	@python_bin="$$(command -v python || command -v python3)"; "$$python_bin" -m pip_audit --version >/dev/null 2>&1 || { echo "pip-audit is required for dependency auditing. Install: python -m pip install pip-audit"; exit 1; }
	SECURITY_AUDIT_STRICT=1 bash ../../.github/scripts/security/python-audit.sh sdk-python

security-scan: security-audit
