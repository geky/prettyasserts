
ifdef DEBUG
TARGET = target/debug
else
CARGOFLAGS += --release
TARGET = target/release
endif

# this indirection is so commands with env are easily copied on the terminal
CARGO ?= RUSTFLAGS="$(RUSTFLAGS)" cargo

.PHONY: all build
all build:
	$(CARGO) build $(CARGOFLAGS)
	cp $(TARGET)/prettyasserts ./prettyasserts

.PHONY: clean
clean:
	$(CARGO) clean $(CARGOFLAGS)
	rm -f ./prettyasserts
