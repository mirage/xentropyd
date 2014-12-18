 
.PHONY: main.native
main.native: setup.data
	ocaml setup.ml -build

setup.data:
	ocaml setup.ml -configure

.PHONY: clean
	ocaml setup.ml -clean
