.PHONY: clean almostclean interactive-maze export
OCAMLC = ocamlopt.opt -thread -I /usr/lib/ocaml/camlimages graphics.cmxa camlimages_core.cmxa camlimages_graphics.cmxa unix.cmxa threads.cmxa

#RLFE=$(shell which rlfe || which rlwrap || true)

RLFE=$(shell which rlfe)
ifneq ($(RLFE),)
	HIST=-h .hist
else
RLFE=$(shell which rlwrap)
ifneq ($(RLFE),)
	HIST=-H .hist
endif
endif


ccsmc: ccsmc.cmx parser.cmx lexer.cmx image.cmx main.ml
	$(OCAMLC) $^ -o $@

interactive-maze: ccsmc
	$(RLFE) $(HIST) ./ccsmc ../examples/maze/mazeIndiv.bmp ../examples/maze/mazeIndiv.ccsmc

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
