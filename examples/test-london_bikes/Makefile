.PHONY: clean almostclean build
TOPOCHECKER=../../src/topochecker

build:
	ocamlbuild -use-ocamlfind -package camlimages.core -package camlimages -package camlimages.all_formats -package csv generate_model.native && cp -L generate_model.native generate_model

infer:
	ocamlbuild -use-ocamlfind -package camlimages.core -package camlimages -package camlimages.all_formats -package csv generate_model.inferred.mli

almostclean: 
	rm -rf *.cm* a.out *~ \#* *.o output generate_model generate_model.native _build

clean: almostclean 
	rm -f kripke.dot space.dot eval.csv test.out

test: clean build
	./generate_model test.out && neato -Tx11 test.out



