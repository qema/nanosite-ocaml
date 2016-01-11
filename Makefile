all:
	ocamlbuild -pkgs compiler-libs.toplevel,str nanosite.byte

test:
	ocamlbuild -pkgs compiler-libs.toplevel,str tests/run_tests.byte
