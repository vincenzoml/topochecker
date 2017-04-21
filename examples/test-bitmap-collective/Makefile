.PHONY: clean almostclean
TOPOCHECKER=../../src/topochecker

run: maketool
	mkdir -p output
	OCAMLRUNPARAM=-b $(TOPOCHECKER) test.topochecker output/state

maketool:
	cd ../../src && make debug

clean: 
	rm -rf out* *~ *.fmla *.slice

view: clean run
	xdg-open output/out_0.bmp
