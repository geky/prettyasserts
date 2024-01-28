
# this indirection is so commands with env are easily copied on the terminal
CARGO ?= RUSTFLAGS="$(RUSTFLAGS)" cargo

.PHONY: all build
all build:
	$(CARGO) build
	cp target/debug/c-tree-edit ./c-tree-edit

.PHONY: clean
clean:
	$(CARGO) clean
	rm -f ./c-tree-edit
