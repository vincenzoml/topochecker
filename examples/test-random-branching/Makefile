.PHONY: clean almostclean
TOPOCHECKER=../../src/topochecker

run: maketool
	mkdir -p output
	$(TOPOCHECKER) test.topochecker output/state

generate_model:
	ocaml generate_model.ml

maketool:
	cd ../../src && make

almostclean: 
	rm -rf *.cm* a.out *~ \#* *.o output

clean: almostclean 
	rm -f kripke.dot space.dot eval.csv
