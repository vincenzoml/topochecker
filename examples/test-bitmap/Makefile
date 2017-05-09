.PHONY: clean almostclean
TOPOCHECKER=../../src/topochecker

run: maketool
	mkdir -p output
	$(TOPOCHECKER) test.topochecker output/state

maketool:
	cd ../../src && make

clean: 
	rm -rf out* *~ *.fmla *.slice


view: run
	xdg-open out_0.bmp
