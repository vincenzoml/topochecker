.PHONY: clean almostclean
TOPOCHECKER=../../src/topochecker

run: maketool
	mkdir -p output
	$(TOPOCHECKER) test.topochecker output/state

maketool:
	cd ../../src && make

clean: almostclean 
	rm -rf output *~


