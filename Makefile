all:
	ocamlbuild -pkgs compiler-libs.toplevel,str nanosite.d.byte

test:
	ocamlbuild -pkgs compiler-libs.toplevel,str tests/run_tests.d.byte
	ocamlrun run_tests.d.byte
