.PHONY: clean almostclean export
EXE=topochecker
TOP=topochecker.top

LIBS=graph.cmxa bigarray.cmxa csv.cmxa
ALIBS=$(LIBS:.cmxa=.cma)
PACKAGES=-package ocamlgraph -package csv
OCAMLC = ocamlfind ocamlc $(PACKAGES) $(ALIBS)
OCAMLOPT = ocamlfind ocamlopt $(PACKAGES) $(LIBS)
OCAMLMKTOP = ocamlfind ocamlmktop $(PACKAGES) $(ALIBS)
DEPS=util.cmx logic.cmx syntax.cmx slice.cmx expParser.cmx expLexer.cmx model.cmx intBool.cmx dotParser.cmx checker.cmx expLoader.cmx 
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

expParser.cmx: expParser.ml
	$(OCAMLOPT) -c expParser.mli
	$(OCAMLOPT) -c expParser.ml

expParser.cmo: expParser.ml
	$(OCAMLC) -c expParser.mli
	$(OCAMLC) -c expParser.ml

expLexer.ml: expLexer.mll
	ocamllex expLexer.mll

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
	rm -f $(EXE) $(TOP) expParser.ml expLexer.ml expParser.mli
