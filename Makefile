RUSTC ?= rustc

dummy1 := $(shell mkdir bin 2> /dev/null)

all:
	$(RUSTC) -O -o bin/racer src/main.rs

clean:
	rm -rf bin
