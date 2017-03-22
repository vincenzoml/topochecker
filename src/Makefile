.PHONY: clean almostclean export slow fast slowclean fastclean
TOP=topochecker.top

#Command to compile without explicit dependencies:
#ocamlbuild -cflags -I,+ocamlgraph,-I,+csv -libs ocamlgraph/graph,csv/csv,bigarray,str main.native

LIBS=bigarray.cmxa unix.cmxa graph.cmxa str.cmxa csv.cmxa
ALIBS=$(LIBS:.cmxa=.cma)
PACKAGES=-package csv -I $(shell ocamlfind query ocamlgraph) -I $(shell ocamlfind query bytes)
OCAMLC = ocamlfind ocamlc $(PACKAGES) $(ALIBS)
OCAMLOPT = ocamlfind ocamlopt -p $(PACKAGES) $(LIBS)
OCAMLMKTOP = ocamlfind ocamlmktop $(PACKAGES) $(ALIBS)
DEPS=util.cmx logic.cmx syntax.cmx tcParser.cmx tcLexer.cmx model.cmx checker.cmx bmp.cmx bmpParser.cmx niftiParser.cmx dotParser.cmx modelLoader.cmx
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

ifneq ($(WINDIR),)
	WINDOWS=true
	EXE=topochecker.exe
else
	WINDOWS=false
	EXE=topochecker
endif

TARGET=slow

OCAMLBUILD_EXE=$(shell which ocamlbuild)

ifneq ($(OCAMLBUILD_EXE),)
	ifeq ($(WINDOWS),false)
		TARGET=fast
		OCAMLBUILD=ocamlbuild -use-ocamlfind -pkg camlimages,camlimages.all_formats -pkg bytes -pkg bigarray -pkg str -pkg unix -pkg ocamlgraph -pkg csv
	endif
endif


$(EXE): $(TARGET)

#%:
#	$(OCAMLBUILD) $@

slow: fastclean doslow

doslow: $(DEPS) $(MAIN).cmx
	$(OCAMLOPT) $^ -o $(EXE)

fast:
	make slowclean
	$(OCAMLBUILD) main.native && cp -L main.native $(EXE) && rm main.native

fastprof:
	make slowclean
	$(OCAMLBUILD) main.p.native

$(TOP): $(ADEPS)
	$(OCAMLMKTOP) $^ -o $@

toptest: $(TOP)
	$(RLFE) -h .hist ./$(TOP)

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
