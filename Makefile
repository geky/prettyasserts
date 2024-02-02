
# this indirection is so commands with env are easily copied on the terminal
CARGO ?= RUSTFLAGS="$(RUSTFLAGS)" cargo

.PHONY: all build
all build:
	$(CARGO) build
	cp target/debug/qadte ./qadte

.PHONY: clean
clean:
	$(CARGO) clean
	rm -f ./qadte
