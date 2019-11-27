.PHONY: all build clean exec fmt

build:
	dune build src/main.exe

fmt :
	dune build @fmt

all:
	build

clean:
	dune clean
