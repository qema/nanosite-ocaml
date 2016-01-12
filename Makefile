all:
	ocamlbuild -pkgs compiler-libs.toplevel,str,omd,batteries nanosite.d.byte

test:
	ocamlbuild -pkgs compiler-libs.toplevel,str,omd,batteries tests/run_tests.d.byte
	ocamlrun run_tests.d.byte
