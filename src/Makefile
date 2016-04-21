.PHONY: clean almostclean export slow fast slowclean fastclean
TOP=topochecker.top

#Command to compile without explicit dependencies:
#ocamlbuild -cflags -I,+ocamlgraph,-I,+csv -libs ocamlgraph/graph,csv/csv,bigarray,str main.native

LIBS=bigarray.cmxa unix.cmxa graph.cmxa str.cmxa csv.cmxa
ALIBS=$(LIBS:.cmxa=.cma)
PACKAGES=-package csv -I $(shell ocamlfind query ocamlgraph) -I $(shell ocamlfind query bytes)
OPTFLAGS = -p
OCAMLC = ocamlfind ocamlc $(PACKAGES) $(ALIBS)
OCAMLOPT = ocamlfind ocamlopt $(OPTFLAG) $(PACKAGES) $(LIBS)
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
		OCAMLBUILD=ocamlbuild -package bytes -package bigarray -package str -package unix -package ocamlgraph -package csv
	endif
endif


$(EXE): $(TARGET)

#%:
#	$(OCAMLBUILD) $@

slow: fastclean doslow

doslow: $(DEPS) $(MAIN).cmx
	make fastclean
	$(OCAMLOPT) $(OPTFLAGS) $^ -o $(EXE)

fast:
	make slowclean
	$(OCAMLBUILD) main.native && cp -L main.native $(EXE) && rm main.native

$(TOP): $(ADEPS)
	$(OCAMLMKTOP) $^ -o $@

toptest: $(TOP)
	$(RLFE) -h .hist ./$(TOP)

tcParser.cmx: tcParser.ml
	$(OCAMLOPT) $(OPTFLAGS) -c tcParser.mli
	$(OCAMLOPT) $(OPTFLAGS) -c tcParser.ml

tcParser.cmo: tcParser.ml
	$(OCAMLC) -c tcParser.mli
	$(OCAMLC) -c tcParser.ml

tcLexer.ml: tcLexer.mll
	ocamllex tcLexer.mll

%.ml: %.mly
	ocamlyacc $^

%.cmx: %.ml
	$(OCAMLOPT) $(OPTFLAGS) -c $^ -o $@

%.cmo: %.ml
	$(OCAMLC) -c $^ -o $@

almostclean: slowclean fastclean

slowclean: 
	rm -rf *.cm* a.out *~ \#* *.o tcParser.ml tcLexer.ml tcParser.mli

fastclean:
	rm -rf _build main.native

clean: almostclean 
	rm -f $(EXE) $(TOP)
