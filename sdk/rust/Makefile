.PHONY: build test publish clean security-audit security-scan

build:
	cargo build --release

test:
	cargo test

publish: test
	cargo publish

clean:
	cargo clean

security-audit:
	@command -v cargo >/dev/null 2>&1 || { echo "cargo is required for dependency auditing. Install Rust: https://rustup.rs/"; exit 1; }
	@command -v cargo-audit >/dev/null 2>&1 || { echo "cargo-audit is required for dependency auditing. Install: cargo install cargo-audit --locked"; exit 1; }
	@command -v jq >/dev/null 2>&1 || { echo "jq is required for dependency audit summaries. Install jq and retry."; exit 1; }
	SECURITY_AUDIT_STRICT=1 bash ../../.github/scripts/security/cargo-audit.sh sdk-rust

security-scan: security-audit
