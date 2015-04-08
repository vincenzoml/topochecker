.PHONY: clean allclean test export
OCAMLC = ocamlopt.opt -thread -I /usr/lib/ocaml/camlimages graphics.cmxa camlimages_core.cmxa camlimages_graphics.cmxa unix.cmxa threads.cmxa 

ccsmc: ccsmc.cmx parser.cmx lexer.cmx image.cmx main.ml
	$(OCAMLC) $^ -o $@

parser.cmx: parser.ml
	$(OCAMLC) -c parser.mli
	$(OCAMLC) -c parser.ml

lexer.ml: lexer.mll
	ocamllex lexer.mll

%.ml: %.mly
	ocamlyacc $^

export: ccsmc almostclean

%.cmx: %.ml
	$(OCAMLC) -c $^ -o $@

almostclean: 
	rm -f *.cm* a.out *~ \#* *.o example-map/*_output.bmp example-map/*~ example-map/\#*

clean: almostclean 
	rm -f ccsmc parser.ml lexer.ml parser.mli
