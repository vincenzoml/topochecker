.PHONY: clean almostclean build
TOPOCHECKER=../../src/topochecker

build:
	ocamlbuild -use-ocamlfind -package csv generate_model.native && cp -L generate_model.native generate_model

infer:
	ocamlbuild -use-ocamlfind -package csv generate_model.inferred.mli

almostclean: 
	rm -rf *.cm* a.out *~ \#* *.o output generate_model generate_model.native _build

clean: almostclean 
	rm -f london_map_transport.dot

test: clean build
	./generate_model test.out && neato -Tx11 london_map_transport.dot



