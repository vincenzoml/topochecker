.PHONY: clean almostclean export slow fast slowclean fastclean
EXE=topochecker
TOP=topochecker.top

#Command to compile without explicit dependencies:
#ocamlbuild -cflags -I,+ocamlgraph,-I,+csv -libs ocamlgraph/graph,csv/csv,bigarray,str main.native

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
	HIST=-h .hist
endif
endif

TARGET=slow
#COMMENT THE FOLLOWING THREE LINES TO FORCE OLD-STYLE MAKEFILED DEPENDENCIES
ifneq ($(shell which ocamlbuild),)
	TARGET=fast
endif
#END REGION TO COMMENT


$(EXE): $(TARGET)

slow: $(DEPS) $(MAIN).cmx
	make fastclean
	$(OCAMLOPT) $^ -o $(EXE)

fast:
	make slowclean
	ocamlbuild -cflags -I,+ocamlgraph,-I,+csv -libs ocamlgraph/graph,csv/csv,bigarray,str main.native && cp -L main.native $(EXE) && rm main.native

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

%.cmx: %.ml
	$(OCAMLOPT) -c $^ -o $@

%.cmo: %.ml
	$(OCAMLC) -c $^ -o $@

almostclean: slowclean fastclean

slowclean: 
	rm -rf *.cm* a.out *~ \#* *.o tcParser.ml tcLexer.ml tcParser.mli

fastclean:
	rm -rf _build main.native

clean: almostclean 
	rm -f $(EXE) $(TOP)
