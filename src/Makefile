.PHONY: clean almostclean export
EXE=topochecker
TOP=topochecker.top

LIBS=graph.cmxa bigarray.cmxa str.cmxa csv.cmxa
ALIBS=$(LIBS:.cmxa=.cma)
PACKAGES=-package ocamlgraph -package csv
OCAMLC = ocamlfind ocamlc $(PACKAGES) $(ALIBS)
OCAMLOPT = ocamlfind ocamlopt $(PACKAGES) $(LIBS)
OCAMLMKTOP = ocamlfind ocamlmktop $(PACKAGES) $(ALIBS)
DEPS=util.cmx logic.cmx syntax.cmx tcParser.cmx tcLexer.cmx model.cmx tcParser.cmx checker.cmx modelLoader.cmx
ADEPS=$(DEPS:.cmx=.cmo)
MAIN=main
RLFE=$(shell which rlfe)
ifneq ($(RLFE),)
	HIST=-h .hist
else
RLFE=$(shell which rlwrap)
ifneq ($(RLFE),)
	HIST=-H .hist
endif
endif


$(EXE): $(DEPS) $(MAIN).cmx
	$(OCAMLOPT) $^ -o $@


$(TOP): $(ADEPS)
	$(OCAMLMKTOP) $^ -o $@

toptest: $(TOP)
	rlfe -h .hist ./$(TOP)

tcParser.cmx: tcParser.ml
	$(OCAMLOPT) -c tcParser.mli
	$(OCAMLOPT) -c tcParser.ml

tcParser.cmo: tcParser.ml
	$(OCAMLC) -c tcParser.mli
	$(OCAMLC) -c tcParser.ml

tcLexer.ml: tcLexer.mll
	ocamllex tcLexer.mll

%.ml: %.mly
	ocamlyacc $^

export: ccsmc almostclean

%.cmx: %.ml
	$(OCAMLOPT) -c $^ -o $@

%.cmo: %.ml
	$(OCAMLC) -c $^ -o $@

almostclean: 
	rm -f *.cm* a.out *~ \#* *.o

clean: almostclean 
	rm -f $(EXE) $(TOP) tcParser.ml tcLexer.ml tcParser.mli
