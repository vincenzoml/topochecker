REQUIRED_PACKAGES = camlimages,threads
REQUIRED_MODULES = graphics.cmxa camlimages_core.cmxa camlimages_graphics.cmxa unix.cmxa threads.cmxa
OCAMLOPT = ocamlfind ocamlopt -thread -package $(REQUIRED_PACKAGES) $(REQUIRED_MODULES)
CMX = Util.cmx Graph.cmx StlLogic.cmx Model.cmx StlConvert.cmx Interface.cmx parserGraph.cmx lexerGraph.cmx parser.cmx lexer.cmx Test.cmx main.cmx
EXE = stmc
RLWRAP = `which rlwrap`

$(EXE): $(CMX)
	$(OCAMLOPT) $(CMX) -o $@

%.cmx: %.ml
	$(OCAMLOPT) -c $^

%.ml: %.mll
	ocamllex $^

%.ml: %.mly
	ocamlyacc $^
	$(OCAMLOPT) -c $*.mli

almostclean :
	rm -f time_tmp.* *.cmi *.cmx *~ \#* *.o lexerGraph.ml lexer.ml parserGraph.ml parser.ml parserGraph.mli parser.mli 

clean : almostclean
	rm -f $(EXE)

interactive-example-trace: $(EXE)
	$(RLWRAP) ./$(EXE) ../examples/trace/maps/map ../examples/trace/time.dot ../examples/trace/formula.stmc 

