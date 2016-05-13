.PHONY: clean almostclean
TOPOCHECKER=../../src/topochecker

run: maketool
	ocaml generate_model.ml
	mkdir -p output
	$(TOPOCHECKER) test.topochecker output/state

maketool:
	cd ../../src && make

almostclean: 
	rm -rf *.cm* a.out *~ \#* *.o output *.slice *.fmla

clean: almostclean 
	rm -f kripke.dot space.dot eval.csv
