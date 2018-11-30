.PHONY: clean almostclean
TOPOCHECKER=../../src/topochecker

run: maketool
	mkdir -p output
	$(TOPOCHECKER) closeToAStop-streetPortion.topochecker

maketool:
	cd ../../src && make

clean: 
	rm -rf output *~ *.fmla *.slice


