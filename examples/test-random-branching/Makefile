.PHONY: clean almostclean
TOPOCHECKER=../../src/topochecker

run: maketool
	ocaml ./generate_model.ml
	mkdir -p output output2
	$(TOPOCHECKER) test.topochecker

generate_model:
	ocaml generate_model.ml

maketool:
	cd ../../src && make

almostclean: 
	rm -rf *.cm* a.out *~ \#* *.o output *.fmla *.slice

clean: almostclean 
	rm -f kripke.dot space.dot eval.csv
