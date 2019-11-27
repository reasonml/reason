.PHONY: all clean

all:
	dune build main.exe

clean:
	dune clean

test-dijkstra:
	dune build main.exe demo/calc.exe
	_build/default/main.exe _build/default/demo/parser.cmly

test-custom:
	dune build main.exe demo/calc.exe
	_build/default/main.exe -custom _build/default/demo/parser.cmly
