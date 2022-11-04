build:
	cargo build --release

test:
	cargo test --release --test integration_tests

.PHONY: build test