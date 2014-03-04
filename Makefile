RUSTC ?= rustc

dummy1 := $(shell mkdir bin 2> /dev/null)

all:
	$(RUSTC) -o bin/racer src/racer.rs

clean:
	rm -rf bin
